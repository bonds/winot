{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Util.Run where

import Protolude
import Util.Log
import qualified Data.Text as T
import qualified GHC.IO.Handle as H
import qualified System.Exit as E
import qualified System.Process as P
import qualified Control.Monad.Logger as ML

default (Text, Integer, Double)

run :: Reason -> Text -> ML.LoggingT IO ()
run why command = do
    _ <- runRead why command
    return ()

runEC :: Reason -> Text -> ML.LoggingT IO E.ExitCode
runEC why command = do
    (_,ec) <- runReadEC why command
    return ec

runRead :: Reason -> Text -> ML.LoggingT IO T.Text
runRead why command = do
    (out,_) <- runReadEC why command
    return out

runReadEC :: Reason -> Text -> ML.LoggingT IO (Text, E.ExitCode)
runReadEC why command = do
    $(myLogTH) LLDevInfo [Reason why, Tag "command"] (Just command)
    (_, Just hout, Just herr, ph) <- liftIO $ P.createProcess
        (P.shell $ T.unpack command)
        {P.std_out = P.CreatePipe, P.std_err = P.CreatePipe}
    out <- liftIO $ H.hGetContents hout
    err <- liftIO $ H.hGetContents herr
    mapM_ ($(myLogTH) LLDevInfo [Reason why, Tag "stdout"] . Just) (T.lines $ T.pack out)
    mapM_ ($(myLogTH) LLDevInfo [Reason why, Tag "stderr"] . Just) (T.lines $ T.pack err)
    ec <-  liftIO $ P.waitForProcess ph
    $(myLogTH) LLDevInfo [Reason why, Tag "exitcode"] $ Just $ show ec
    return (T.pack out, ec)


