{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Util where

import Protolude
import Prelude (($), String, (++), take)
import World
import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as S
import qualified Control.Concurrent.Thread.Delay as D
import qualified Control.Monad as M
import qualified Data.Text as T
import qualified GHC.IO.Handle as H
import qualified System.Exit as E
import qualified System.Log.Logger as L
import qualified System.Process as P
import qualified Data.Maybe as B
import qualified Data.Text.ICU as U
{-import qualified System.Log.FastLogger as L-}
{-import qualified Data.Time.LocalTime as DT-}

default (T.Text, Integer, Double)

ipPattern :: T.Text
ipPattern = "[0-9]*\\.[0-9]*\\.[0-9]*\\.[0-9]*"

atomRead :: S.TVar a -> IO a
atomRead = S.atomically . S.readTVar

atomWrite :: S.TVar a -> a -> IO ()
atomWrite x y = S.atomically $ S.writeTVar x y

tryLock :: T.Text -> S.TMVar () -> IO () -> IO ()
tryLock n lock actions = do
    let logPrefix = "winot.tryLock"
    gotLock <- S.atomically $ S.tryPutTMVar lock ()
    L.debugM logPrefix $ T.unpack $ T.concat ["tryLock ", n, " ", T.pack (show gotLock)]
    M.when gotLock $ do
        actions
        S.atomically $ S.takeTMVar lock

tryLockFork :: T.Text -> S.TMVar () -> IO () -> IO ()
tryLockFork n lock actions = do
    let logPrefix = "winot.tryLockFork"
    gotLock <- S.atomically $ S.tryPutTMVar lock ()
    L.debugM logPrefix $ T.unpack $ T.concat ["tryLock ", n, " ", T.pack (show gotLock)]
    M.when gotLock $ do
        _ <- C.forkIO $ do
            actions
            S.atomically $ S.takeTMVar lock
            M.return ()
        M.return ()

wasSuccess :: E.ExitCode -> Bool
wasSuccess E.ExitSuccess = True
wasSuccess (E.ExitFailure _) = False

firstIfAvailable :: T.Text -> [IFInfo] -> T.Text
firstIfAvailable prefix ifs = T.concat [prefix, T.pack (show $ fan numbers)]
  where
      names = [ name i | i <- ifs, prefix `T.isPrefixOf` name i ]
      numbers = B.catMaybes [ readMaybe [T.last x] | x <- names ]
      fan :: [Int] -> Int
      fan x@(_:_)  = maybe 0 (+1) $ maximumMay x
      fan _        = 0

-- ping up to X times, if any are OK, stop, and return OK
ping :: Int -> T.Text -> IO Bool
ping = pingVia 0

{-@ pingVia :: Nat -> count:Int -> T.Text -> IO Bool / [count] @-}
pingVia :: Int -> Int -> T.Text -> IO Bool
pingVia rt count host
    | count < 1 = M.return False
    | otherwise = do
        ec <- runEC $ T.concat ["ping -V ", T.pack (show rt), " -q -c 1 -w 1 ", host]
        if wasSuccess ec then M.return True else do
            D.delay $ (10::Integer)^(6::Integer) -- wait a second between ping attempts
            pingVia rt (count-1) host

detailOrEmpty :: Maybe IFInfo -> T.Text
detailOrEmpty (Just x) = detail x
detailOrEmpty Nothing = ""

lastN :: Int -> [a] -> [a]
lastN x l = reverse $ take x $ reverse l

run :: T.Text -> IO ()
run command = do
    _ <- runRead command
    M.return ()

runEC :: T.Text -> IO E.ExitCode
runEC command = do
    (_,ec) <- runReadEC command
    M.return ec

runRead :: T.Text -> IO T.Text
runRead command = do
    (stdout,_) <- runReadEC command
    M.return stdout

runReadEC :: T.Text -> IO (T.Text, E.ExitCode)
runReadEC command = do
    let logPrefix = "winot.runReadEC"
    {-logSet <- L.newStdoutLoggerSet L.defaultBufSize-}
    L.debugM logPrefix $ T.unpack $ T.concat ["run: ", command]
    {-debug logPrefix (T.concat ["run: ", command]) logSet-}
    (_, Just hout, Just herr, ph) <- P.createProcess
        (P.shell $ T.unpack command)
        {P.std_out = P.CreatePipe, P.std_err = P.CreatePipe}
    ec <- P.waitForProcess ph
    stdout <- H.hGetContents hout
    stderr <- H.hGetContents herr
    {-debugStdout logPrefix (T.lines $ T.pack stdout) logSet-}
    {-debugStderr logPrefix (T.lines $ T.pack stderr) logSet-}
    mapM_ (printLine logPrefix "| ") (T.lines $ T.pack stdout)
    mapM_ (printLine logPrefix "x ") (T.lines $ T.pack stderr)
    M.return (T.pack stdout, ec)
 where
   printLine logPre linePrefix line =
       L.debugM logPre $ T.unpack $ T.concat [linePrefix, line]

{-debug :: T.Text -> T.Text -> L.LoggerSet -> IO ()-}
{-debug loc msg = logLines (T.concat [" : ", loc, " : DEBUG] "]) [msg]-}

{-debugStd :: T.Text -> T.Text -> [T.Text] -> L.LoggerSet -> IO ()-}
{-debugStd pre loc = logLines (T.concat [" : ", loc, " : DEBUG", pre, " "])-}

{-debugStdout :: T.Text -> [T.Text] -> L.LoggerSet -> IO ()-}
{-debugStdout = debugStd "|"-}

{-debugStderr :: T.Text -> [T.Text] -> L.LoggerSet -> IO ()-}
{-debugStderr = debugStd "x"-}

{-logLines :: T.Text -> [T.Text] -> L.LoggerSet -> IO ()-}
{-logLines lPrefix ls lSet = do-}
    {-now <- DT.getZonedTime-}
    {-let nowText = T.pack $ show now-}
    {-mapM_-}
        {-(\l -> L.pushLogStrLn lSet (L.toLogStr $ T.concat [nowText, lPrefix, l]))-}
        {-ls-}
    {-return ()-}

lastMatchFirstGroup :: [[String]] -> Maybe T.Text
lastMatchFirstGroup xs = case lastMay xs of
    Just gs -> case headMay gs of
        Just g -> Just $ T.pack g
        Nothing -> Nothing
    Nothing -> Nothing

parseInterfaceList :: T.Text -> [IFInfo]
parseInterfaceList il = list
  where
    list = [IFInfo { name = fst record
                   , detail = T.unlines $ snd record
                   }
           | record <- snd $ infos (T.lines il) []]
    {-@ infos :: i:[T.Text] -> [(T.Text, [T.Text])] -> ([T.Text], [(T.Text, [T.Text])]) / [len i] @-}
    infos :: [T.Text] -> [(T.Text, [T.Text])] -> ([T.Text], [(T.Text, [T.Text])])
    infos [] records = ([], records)
    infos (x:xs) records
        | startsWithInterfaceName = infos xs $ records ++ [(interfaceName, [x])]
        | otherwise = case lastMay records of
              Just currentRecord -> infos xs $ initOrEmpty records ++ [(fst currentRecord, snd currentRecord ++ [x])]
              Nothing            -> infos xs records
        where
        startsWithInterfaceName = B.isJust (U.find (U.regex [] "^[a-zA-Z]*[0-9]: ") x)
        interfaceName = B.fromJust (U.group 0 (B.fromJust (U.find (U.regex [] "^[a-zA-Z]*[0-9]") x)))

initOrEmpty :: [a] -> [a]
initOrEmpty = reverse . drop 1 . reverse -- gives a empty list if less than 2 in list

idle :: World -> [Maybe Int] -> IO (Maybe Bool)
idle world l = if length l >= intervals then do
                   let ln = B.catMaybes $ lastN intervals l
                   if length ln == intervals then
                       case maximumMay ln of
                           Just m -> if m < imeans then
                                   M.return $ Just True
                               else
                                   M.return $ Just False
                           Nothing -> M.return Nothing
                   else
                       M.return Nothing
               else
                   M.return Nothing
  where
    intervals = B.maybe 30 (B.maybe 30 id . readMaybe . T.unpack) $ configString "IdleIntervalsBeforeIdle" world
    imeans = B.maybe 1000 (B.maybe 1000 id . readMaybe . T.unpack) $ configString "IdleMeansLessThanXBytes" world

recordBandwidth :: World -> T.Text -> S.TVar [Maybe Int] -> IO ()
recordBandwidth world interface bl = do
    bw <- bandwidth world interface
    l <- atomRead bl
    let !values = lastN (itemsToKeep-1) l ++ B.maybe [B.Nothing] (\x -> [readMaybe $ T.unpack x]) bw
    atomWrite bl values
    l' <- atomRead bl
    L.debugM (T.unpack logPrefix) $ T.unpack $ T.concat [interface, "bw: ", T.pack (show (lastN 5 l'))]
  where
    itemsToKeep = 100
    logPrefix = T.concat ["winot.recordBandwidth.", interface]

bandwidth :: World -> T.Text -> IO (Maybe T.Text)
bandwidth world interface = do
    stats <- atomRead (interfaceStats world)
    M.return $ case U.find
            (U.regex [U.Multiline] ("^" `T.append` interface `T.append` ".*"))
            stats of
        Just m -> T.words (B.fromJust $ U.group 0 m) `atMay` 6
        Nothing -> Nothing

updateInterfaceStats :: World -> IO ()
updateInterfaceStats world = do
    stats <- runRead $ "systat -w 100 -B ifstat " `T.append` sampleSizeInSeconds
    atomWrite (interfaceStats world) stats
  where
    sampleSizeInSeconds = "1"

