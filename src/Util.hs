{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Util where

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
            return ()
        return ()

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
    | count < 1 = return False
    | otherwise = do
        ec <- runEC $ T.concat ["ping -V ", T.pack (show rt), " -q -c 1 -w 1 ", host]
        if wasSuccess ec then return True else do
            D.delay $ (10::Integer)^(6::Integer) -- wait a second between ping attempts
            pingVia rt (count-1) host

pingMaybe :: Maybe T.Text -> IO Bool
pingMaybe (Just ip) = do
    let logPrefix = "winot.pingMaybe"
    L.debugM logPrefix $ T.unpack $ T.concat ["pinging ", ip]
    result <- ping 3 ip
    L.debugM logPrefix $ T.unpack $ T.concat ["ping success? ", T.pack (show result)]
    return result
pingMaybe Nothing = do
    let logPrefix = "winot.pingMaybe"
    L.debugM logPrefix "ping failed because no IP"
    return False

detailOrEmpty :: Maybe IFInfo -> T.Text
detailOrEmpty (Just x) = detail x
detailOrEmpty Nothing = ""

lastX :: Int -> [a] -> [a]
lastX x l = reverse $ take x $ reverse l

run :: T.Text -> IO ()
run command = do
    _ <- runRead command
    return ()

runEC :: T.Text -> IO E.ExitCode
runEC command = do
    (_,ec) <- runReadEC command
    return ec

runRead :: T.Text -> IO T.Text
runRead command = do
    (stdout,_) <- runReadEC command
    return stdout

runReadEC :: T.Text -> IO (T.Text, E.ExitCode)
runReadEC command = do
    let logPrefix = "winot.runReadEC"
    L.debugM logPrefix $ T.unpack $ T.concat ["run: ", command]
    (_, Just hout, Just herr, ph) <- P.createProcess
        (P.shell $ T.unpack command)
        {P.std_out = P.CreatePipe, P.std_err = P.CreatePipe}
    ec <- P.waitForProcess ph
    stdout <- H.hGetContents hout
    stderr <- H.hGetContents herr
    mapM_ (printLine logPrefix "| ") (T.lines $ T.pack stdout)
    mapM_ (printLine logPrefix "x ") (T.lines $ T.pack stderr)
    return (T.pack stdout, ec)
  where
    printLine logPre linePrefix line =
        L.debugM logPre $ T.unpack $ T.concat [linePrefix, line]

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
    infos (ls, records)
        | B.isJust (U.find (U.regex [] "^[a-zA-Z]*[0-9]: ") currentLine) = infos (tail ls, records ++
            [(B.fromJust (U.group 0 (B.fromJust (U.find (U.regex [] "^[a-zA-Z]*[0-9]") currentLine)))
                                                , [currentLine]
                                                )])
        | otherwise = infos (tail ls, init' records ++
                      [(fst currentRecord, snd currentRecord ++ [currentLine])])
      where
        currentLine = head ls
        currentRecord = last records
        init' :: [a] -> [a]
        init' x = reverse $ drop 1 $ reverse x -- gives a empty list if less than 2 in list


