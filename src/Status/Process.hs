{-# OPTIONS_GHC -Wall -Werror  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE BangPatterns #-}

module Status.Process where

import Protolude
import Util.Run
import Util.Log
import qualified Text.Trifecta as Parse
import qualified Data.Text as T
import qualified Control.Monad.Logger as ML

default (Text, Integer, Double)

-- TODO: get logs setup with the right levels/tags
-- TODO: write tests for all this
-- TODO: measure test coverage for tests
-- TODO: good documentation
-- TODO: create info directory, put process into it
-- TODO: don't pay CPU/IO cost for logging that we aren't using at the moment
-- TODO: group lo gs related to the same action

data Process = Process
    { command :: Text
    , pid     :: Integer
    , rtable  :: Integer
    , user    :: Text
    }
    deriving (Show, Eq)

processes :: ML.LoggingT IO [Process]
processes = do
    $(myLogTH) LLDevInfo [Reason LRStatus, Tag "breadcrumb"] Nothing
    procs <- getProcesses
    case parseProcesses procs of
        Parse.Success result -> return result
        Parse.Failure err -> do
            mapM_
                ($(myLogTH) LLUserTellDevAppIsBroken [Reason LRStatus, Tag "parseerror"] . Just)
                (T.lines $ show err)
            return []

getProcesses :: ML.LoggingT IO Text
getProcesses = runRead LRStatus "ps -Aww -o pid,rtable,user,command"

parseProcesses :: Text -> Parse.Result [Process]
parseProcesses t = Parse.parseString processesParser mempty (T.unpack t)

processesParser :: Parse.Parser [Process]
processesParser = do
    _ <- Parse.some $ Parse.notChar '\n'
    _ <- Parse.newline
    Parse.some processParser

processParser :: Parse.Parser Process
processParser = do
    Parse.whiteSpace
    pid' <- Parse.integer
    Parse.whiteSpace
    rtable' <- Parse.integer
    Parse.whiteSpace
    user' <- Parse.some $ Parse.notChar ' '
    Parse.someSpace
    command' <- Parse.some $ Parse.notChar '\n'
    _ <- Parse.newline
    return Process
        { pid     = pid'
        , rtable  = rtable'
        , user    = T.pack user'
        , command = T.pack command'
        }
