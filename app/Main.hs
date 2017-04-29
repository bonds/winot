{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude
import Loop
-- import qualified Control.Monad.Logger as ML
import Util.Log

main :: IO ()
main = do
    -- _ <- liftIO $ ML.runStdoutLoggingT loop
    -- _ <- liftIO $ runNoLogs loop
    -- _ <- liftIO $ runFakeLogs loop
    _ <- liftIO $ runMyLogs loop
    return ()
