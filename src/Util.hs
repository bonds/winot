{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Util where

import Protolude
import Prelude (($), last, maximum, String, (++), read, take)
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

lastOrBlank :: [T.Text] -> T.Text
lastOrBlank x@(_:_) = last x
lastOrBlank _ = ""

wasSuccess :: E.ExitCode -> Bool
wasSuccess E.ExitSuccess = True
wasSuccess (E.ExitFailure _) = False

firstIfAvailable :: T.Text -> [IFInfo] -> T.Text
firstIfAvailable prefix ifs = T.concat [prefix, T.pack (show $ fan numbers)]
  where
      names = [ name i | i <- ifs, prefix `T.isPrefixOf` name i ]
      numbers = [ read [T.last x] | x <- names ]
      fan :: [Int] -> Int
      fan x@(_:_)  = maximum x + 1
      fan _        = 0

-- ping up to X times, if any are OK, stop, and return OK
ping :: Int -> T.Text -> IO Bool
ping = pingVia 0

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

lastX :: Int -> [a] -> [a]
lastX x l = reverse $ take x $ reverse l

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
lastMatchFirstGroup x@(_:_) =
    case last x of
        (_:y) -> case y of
            (z:_) -> Just (T.pack z)
            _ -> Nothing
        _ -> Nothing
lastMatchFirstGroup _ = Nothing

parseInterfaceList :: T.Text -> [IFInfo]
parseInterfaceList il = list
  where
    list = [IFInfo { name = fst record
                   , detail = T.unlines $ snd record
                   }
           | record <- snd $ infos (T.lines il, [])]
    infos :: ([T.Text], [(T.Text, [T.Text])]) -> ([T.Text], [(T.Text, [T.Text])])
    infos ([], records) = ([], records)
    infos (x:xs, records)
        | startsWithInterfaceName = infos (xs, records ++ [(interfaceName, [x])])
        | otherwise = infos (xs, initOrEmpty records ++ [(currentName, currentDetail ++ [x])])
      where
        startsWithInterfaceName = B.isJust (U.find (U.regex [] "^[a-zA-Z]*[0-9]: ") x)
        interfaceName = B.fromJust (U.group 0 (B.fromJust (U.find (U.regex [] "^[a-zA-Z]*[0-9]") x)))
        currentRecord = last records
        currentName = fst currentRecord
        currentDetail = snd currentRecord

initOrEmpty :: [a] -> [a]
initOrEmpty x = reverse $ drop 1 $ reverse x -- gives a empty list if less than 2 in list

