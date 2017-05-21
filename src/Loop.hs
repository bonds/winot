{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE BangPatterns      #-}

module Loop where

import Action.Connect
import Data.UnixTime (UnixTime)
import Protolude hiding (handle)
import Status.Filter
import Status.Interface
import Status.Lease
import Status.Process
import Status.Route
import Util.Log
import Util.Run
import World
import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as S
import qualified Control.Concurrent.Thread.Delay as D
import qualified Control.Monad as M
import qualified Control.Monad.Logger as ML
import qualified Control.Monad.Loops as M
import qualified Data.Text as T
import qualified Data.UnixTime as Time
import qualified System.Exit as E
import qualified System.Posix.Process as P
import qualified System.Posix.Signals as P

default (Text, Integer, Double)

loop :: ML.LoggingT IO ()
loop = do
    startup
    M.iterateM_ iteration initialWorld

startup :: ML.LoggingT IO ()
startup = do

    -- quit if not running as root
    uid <- runRead LRStatus "id -u"
    liftIO $ M.when (T.strip uid /= "0") $ do
        print ("error: this script must be run by root" :: Text)
        E.exitFailure

    -- run at a low priority
    liftIO $ P.nice 20

    -- handle quit signals
    tid <- liftIO C.myThreadId
    liftIO $ mapM_ (handle tid) [P.sigINT, P.sigTERM, P.sigQUIT, P.sigHUP]

    return ()

handle :: ThreadId -> P.Signal -> IO P.Handler
handle tid sig =
    P.installHandler sig
        (P.Catch $ ML.runStdoutLoggingT $ cleanUp tid)
        Nothing

iteration :: World -> ML.LoggingT IO World
iteration oldWorld = do
    newTime             <- liftIO Time.getUnixTime
    newProcesses        <- processes
    newRoutes           <- routes
    newLeases           <- leases
    newFilterAnchors    <- filterAnchors
    newInterfaces       <- interfaces
    newInterfaceStats   <- interfaceStats
    ifs <- liftIO $ mergeInterfaceInfo
        newInterfaces
        (woInterfaces oldWorld)
        newInterfaceStats
        newRoutes
        newLeases

    let newWorld = oldWorld
            { woLoopTimes   = mergeLoopTimes newTime $ woLoopTimes oldWorld
            , woInterfaces  = ifs
            , woProcesses   = newProcesses
            , woFilterAnchors = newFilterAnchors
            }
    $(myLogTH) LLDevInfo [Tag "world"] $ Just $ show newWorld
    -- (re)load config
    -- choose route
    c1 <- checkRD0 $ woInterfaces newWorld
    c2 <- checkRdomains $ woInterfaces newWorld
    when (c1 && c2) $
        mapM_ (connect newWorld) $ connectableInterfaces $ woInterfaces newWorld

    liftIO $ delayNeeded >>= D.delay
    return newWorld

  where

    secondsBetweenLoops = 1 :: Integer

    delayNeeded :: IO Integer
    delayNeeded = case lastMay $ woLoopTimes oldWorld of
        Nothing -> return 0
        Just oldTime -> do
            currentTime <- liftIO Time.getUnixTime
            let loopLength = currentTime `Time.diffUnixTime` oldTime
            let sbl = Time.secondsToUnixDiffTime secondsBetweenLoops
            let gap = sbl - loopLength
            return $ udtToMicroseconds
              $ if sbl > gap then sbl else gap

    udtToMicroseconds :: Time.UnixDiffTime -> Integer
    udtToMicroseconds t =
        (round ((
              fromRational
            . toRational
            $ Time.udtSeconds t) :: Double) :: Integer)
        * (10::Integer)^(6::Integer)
        + toInteger (Time.udtMicroSeconds t)

cleanUp :: ThreadId -> ML.LoggingT IO ()
cleanUp tid = do
    $(myLogTH) LLDevInfo [Reason LRAction, Tag "cleanup"] Nothing
    _ <- liftIO $ C.throwTo tid E.ExitSuccess
    return ()

mergeLoopTimes :: UnixTime -> [UnixTime] -> [UnixTime]
mergeLoopTimes nt ots = take 2 $ nt : ots

mergeInterfaceInfo :: [Interface] -> [Interface] -> [IfStats] -> [Route] -> [Lease] -> IO [Interface]
mergeInterfaceInfo [] _ _ _ _ = return []
mergeInterfaceInfo (x:xs) oldInterfaces newStats newRoutes newLeases = do
    newII <- mergeII x oldInterfaces newStats newRoutes newLeases
    moreII <- mergeInterfaceInfo xs oldInterfaces newStats newRoutes newLeases
    return $ newII : moreII
  where
    -- keeping history even if bssid or network changed...hopefully that's good enough
    mergeII :: Interface -> [Interface] -> [IfStats] -> [Route] -> [Lease] -> IO Interface
    mergeII i os ns nr nl = do
        newLock <- S.atomically S.newEmptyTMVar
        newReady <- S.atomically $ S.newTVar False
        return $ x
            { ifBandwidthHistory  = Just $
                take 100 $ fromMaybe [] (makeNewBandwidthHistory i ns)
                         ++ fromMaybe [] (findOldBandwidthHistory i os)
            , ifWirelessDetail    =
                case ifWirelessDetail i of
                    Just ifwd -> case find (\old -> interface old == interface i) os of
                        Just iold -> case ifWirelessDetail iold of
                            Just od -> Just ifwd
                                { ifStrengthHistory = take 100 $ ifStrengthHistory ifwd ++ ifStrengthHistory od }
                            Nothing -> Just ifwd
                        Nothing -> Just ifwd
                    Nothing -> Nothing
            , ifLock              =
                case findOldLock i os of
                    Just il -> Just il
                    Nothing -> Just newLock
            , ifStatus            = (ifStatus i)
                {
                    ifReady       =
                        case findOldInterface i os of
                            Just o -> ifReady $ ifStatus o
                            Nothing -> Just newReady
                }
            , ifIPv4Detail        = case ifIPv4Detail i of
                Just ipd -> Just ipd
                    { ifIPv4Routes = Just $
                        filter (\y -> interface i == routeInterface y) nr
                    , ifIPv4Lease = find (\y -> interface i == lsInterface y && interfaceSSID i == lsSSID y) nl
                    }
                Nothing -> Nothing
            }

findOldInterface :: Interface -> [Interface] -> Maybe Interface
findOldInterface i = find (\old -> interface old == interface i)

findOldLock :: Interface -> [Interface] -> Maybe (S.TMVar ())
findOldLock i oi =
    case find (\old -> interface old == interface i) oi of
        Just a -> ifLock a
        Nothing -> Nothing

findOldBandwidthHistory :: Interface -> [Interface] -> Maybe [IfBandwidth]
findOldBandwidthHistory i oi =
    case find (\old -> interface old == interface i) oi of
        Just a -> ifBandwidthHistory a
        Nothing -> Nothing

makeNewBandwidthHistory :: Interface -> [IfStats] -> Maybe [IfBandwidth]
makeNewBandwidthHistory i ns =
    case find (\stats -> iftInterface stats == interface i) ns of
        Just a -> Just [IfBandwidth
            { ifbWhen     = iftWhen a
            , ifbBytes    = iftInputBytes a + iftOutputBytes a
            }]
        Nothing -> Just []

