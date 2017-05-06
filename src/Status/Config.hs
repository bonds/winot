{-# OPTIONS_GHC -Wall -Werror  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Status.Config where

import Protolude hiding (handle)
import Util.Log
import Util.Run
import qualified Data.Text as T
import qualified Control.Monad.Logger as ML
import qualified Text.Trifecta as Parse
-- import Data.List ((\\))

data WirelessPassword = WirelessPassword
    { wpNetwork  :: Text
    , wpPassword :: Maybe Text
    } deriving (Show, Eq)

wirelessPasswords :: ML.LoggingT IO [WirelessPassword]
wirelessPasswords = do
    $(myLogTH) LLDevInfo [Reason LRStatus, Tag "breadcrumb"] Nothing
    procs <- getWirelessPasswords
    case parseWirelessPasswords procs of
        Parse.Success result -> return result
        Parse.Failure err -> do
            mapM_
                ($(myLogTH) LLUserTellDevAppIsBroken [Reason LRStatus, Tag "parseerror"] . Just)
                (T.lines $ show err)
            return []

parseWirelessPasswords :: Text -> Parse.Result [WirelessPassword]
parseWirelessPasswords t = Parse.parseString wirelessPasswordsParser mempty (T.unpack t)

wirelessPasswordsParser :: Parse.Parser [WirelessPassword]
wirelessPasswordsParser = do
    _ <- Parse.try $ Parse.optional $ do
        _ <- Parse.text "device"
        _ <- Parse.some $ Parse.notChar '\n'
        Parse.newline
    Parse.whiteSpace
    Parse.some wirelessPasswordParser

wirelessPasswordParser :: Parse.Parser WirelessPassword
wirelessPasswordParser = do
    _ <- Parse.some $ Parse.noneOf [' ', '\n']
    Parse.whiteSpace
    network <- Parse.try $ Parse.stringLiteral <|> do
        a <- Parse.some $ Parse.noneOf [' ', '\n']
        return $ T.pack a
    password <-Parse.try parsePassword <|> do
        _ <-Parse.newline
        return Nothing
    return WirelessPassword
        { wpNetwork  = network
        , wpPassword = password
        }
  where
    parsePassword = do
        Parse.whiteSpace
        pw <- Parse.some $ Parse.noneOf [' ', '\n']
        _ <- Parse.newline
        return (Just $ T.pack pw)

getWirelessPasswords :: ML.LoggingT IO Text
getWirelessPasswords = runRead LRStatus "cat /etc/wireless.conf"

