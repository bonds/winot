{-# OPTIONS_GHC -Wall -Werror  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Util.Log where

import Protolude
-- import Data.Function (id)
import qualified Data.Text as T
-- import qualified Data.Text.Encoding as T
import qualified Control.Monad.Logger as Log
import qualified Data.UnixTime as Time
import qualified Control.Concurrent as C
import qualified Language.Haskell.TH.Syntax as TH
-- import qualified Filesystem.Path.CurrentOS as FP
import qualified GHC.Show
import qualified Data.ByteString.Char8 as S8
import System.Log.FastLogger

data Level = LLDevInfo
           | LLDevThisShouldntHappen
           | LLUserInfo
           | LLUserNeedYourHelp
           | LLUserTellDevAppIsBroken
    deriving (Eq, Ord)

data Tag = Reason Reason
         | Output Output
         | Tag Text
    deriving Eq

data Reason = LRStatus
            | LRAction
            | LRConst
    deriving (Show, Eq)

data Output = LOStdout
            | LOStderr
            | LOExitcode
    deriving (Show, Eq)

myLogTH :: TH.Q TH.Exp
myLogTH = [|myLog $(TH.qLocation >>= Log.liftLoc)|]

myLog :: TH.Loc -> Level -> [Tag] -> Maybe Text -> Log.LoggingT IO ()
myLog loc lvl tags message =
    Log.monadLoggerLog loc "" (Log.LevelOther "")
        $ show lvl
        <> " "
        <> T.unwords (map show tags)
        <> case message of
            Just m -> " | " <> m
            Nothing -> ""

instance Show Level where
    show lvl = case lvl of
        LLDevInfo                 -> "#dev #info"
        LLDevThisShouldntHappen   -> "#dev #thisshouldneverhappen"
        LLUserInfo                -> "#user #info"
        LLUserNeedYourHelp        -> "#user #needyourhelp"
        LLUserTellDevAppIsBroken  -> "#user #telldevappisbroken"

instance Show Tag where
    show tag = case tag of
        Reason a -> T.unpack $ ("#" <>) . T.toLower . T.drop 2 $ show a
        Output a -> T.unpack $ ("#" <>) . T.toLower . T.drop 2 $ show a
        Tag a -> T.unpack $ "#" <> a

runNoLogs :: Log.LoggingT m a -> m a
runNoLogs = (`Log.runLoggingT` (return $ \ _ _ _ -> return ()))

runFakeLogs :: Log.LoggingT m a -> m a
runFakeLogs = (`Log.runLoggingT` (return $ \ a b _ -> print a >> print b ))

runMyLogs :: Log.LoggingT m a -> m a
runMyLogs = (`Log.runLoggingT` defaultOutput stdout)

defaultOutput :: Handle
              -> TH.Loc
              -> Log.LogSource
              -> Log.LogLevel
              -> Log.LogStr
              -> IO ()
defaultOutput h loc _ _ lmsg = do
    timeRaw <- liftIO Time.getUnixTime
    time <- liftIO $ Time.formatUnixTime "%a %Y%m%d %H:%M:%S" timeRaw
    zone <- liftIO $ Time.formatUnixTime "%z" timeRaw
    let ms = Time.utMicroSeconds timeRaw
    let time' = time <> "." <> show ms <> " " <> zone
    tid <- liftIO C.myThreadId
    let prefix = time'
               <> " | "
               <> fromMaybe "" (lastMay (S8.words (show tid)))
               <> ":"
               <> S8.pack (TH.loc_filename loc) <> ":" <> S8.pack (show $ fst $ TH.loc_start loc)
               <> " | "
    S8.hPutStrLn h $ prefix <> fromLogStr lmsg
    return ()

