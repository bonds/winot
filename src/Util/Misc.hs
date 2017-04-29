{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Util.Misc where

import GHC.Base (String)
import Protolude
import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as S
import qualified Control.Monad as M
import qualified Data.Text as T
import qualified System.Exit as E
import qualified System.Log.Logger as L
import qualified GHC.Show (show)

default (Text, Integer, Double)

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

lastN :: Int -> [a] -> [a]
lastN x l = reverse $ take x $ reverse l

lastMatchFirstGroup :: [[String]] -> Maybe T.Text
lastMatchFirstGroup xs = case lastMay xs of
    Just gs -> case headMay gs of
        Just g -> Just $ T.pack g
        Nothing -> Nothing
    Nothing -> Nothing

initOrEmpty :: [a] -> [a]
initOrEmpty = reverse . drop 1 . reverse -- gives a empty list if less than 2 in list

newtype WTVar a = TVar a

instance Show (WTVar a) where
    show = show
