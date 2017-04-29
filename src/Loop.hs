{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE BangPatterns #-}

module Loop where

import Protolude hiding (handle)
-- import Control.Monad ((>>))
import Util.Run
import Util.Log
import Status.Interface
import Status.Process
import Status.Route
import World
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Thread.Delay as D
import qualified Control.Monad as M
import qualified Control.Monad.Loops as M
import qualified Data.Text as T
import qualified System.Exit as E
import qualified System.Posix.Process as P
import qualified System.Posix.Signals as P
import qualified Control.Monad.Logger as ML
import qualified Data.UnixTime as Time
import Data.UnixTime (UnixTime)

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
    newInterfaces       <- interfaces
    newInterfaceStats   <- interfaceStats

    let newWorld = oldWorld
            { woLoopTimes   = mergeLoopTimes newTime $ woLoopTimes oldWorld
            , woInterfaces  = mergeInterfaceInfo
                newInterfaces
                (woInterfaces oldWorld)
                newInterfaceStats
            , woProcesses   = newProcesses
            , woRoutes      = newRoutes
            }
    $(myLogTH) LLDevInfo [Tag "world"] $ Just $ show newWorld
    -- (re)load config
    -- record signal strengths
    -- record bandwidth
    -- get interface list
    -- get interface stats
    -- get process list
    -- get routes
    -- choose route
    -- check plan
    -- check ulan
    -- check wlan
    -- check wwan

    liftIO $ delayNeeded >>= D.delay
    return newWorld

  where

    secondsBetweenLoops = 5 :: Integer

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

mergeInterfaceInfo :: [Interface] -> [Interface] -> [IfStats] -> [Interface]
mergeInterfaceInfo [] _ _ = []
mergeInterfaceInfo (x:xs) oldInterfaces newStats =
    mergeII x oldInterfaces newStats : mergeInterfaceInfo xs oldInterfaces newStats
  where
    -- keeping history even if bssid or network changed...hopefully that's good enough
    mergeII :: Interface -> [Interface] -> [IfStats] -> Interface
    mergeII i oi ns = x
        { ifBandwidthHistory  =
            take 100 $ makeNewBandwidthHistory i ns
                     ++ findOldBandwidthHistory i oi
        , ifWirelessDetail    =
            case ifWirelessDetail i of
                Just ifwd -> case find (\old -> interface old == interface i) oi of
                    Just iold -> case ifWirelessDetail iold of
                        Just od -> Just ifwd
                            { ifStrengthHistory = take 100 $ ifStrengthHistory ifwd ++ ifStrengthHistory od }
                        Nothing -> Just ifwd
                    Nothing -> Just ifwd
                Nothing -> Nothing
        }

findOldBandwidthHistory :: Interface -> [Interface] -> [IfBandwidth]
findOldBandwidthHistory i oi =
    case find (\old -> interface old == interface i) oi of
        Just a -> ifBandwidthHistory a
        Nothing -> []

makeNewBandwidthHistory :: Interface -> [IfStats] -> [IfBandwidth]
makeNewBandwidthHistory i ns =
    case find (\stats -> iftInterface stats == interface i) ns of
        Just a -> [IfBandwidth
            { ifbWhen     = iftWhen a
            , ifbBytes    = iftInputBytes a + iftOutputBytes a
            }]
        Nothing -> []

