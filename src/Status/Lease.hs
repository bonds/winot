{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Status.Lease where

import Protolude hiding (handle)
import Util.Run
import Util.Log
import qualified Control.Monad.Logger as ML
import qualified Text.Trifecta as Parse
import qualified Data.Text as T

default (Text, Integer, Double)

data Lease = Lease
    { lsInterface :: Text
    , lsSSID      :: Maybe Text
    , lsRouter    :: Text
    } deriving (Show, Eq)

data LeaseInfo = LIInterface Text | LISSID Text | LIRouter Text | LIIgnored
    deriving (Show, Eq)

leases :: ML.LoggingT IO [Lease]
leases = do
    $(myLogTH) LLDevInfo [Reason LRStatus, Tag "youarehere"] Nothing
    ls <- getLeases
    case parseLeases ls of
        Parse.Success result -> return result
        Parse.Failure err -> do
            mapM_
                ($(myLogTH) LLUserTellDevAppIsBroken [Reason LRStatus, Tag "parseerror"] . Just)
                (T.lines $ show err)
            return []

parseLeases :: Text -> Parse.Result [Lease]
parseLeases t = Parse.parseString leasesParser mempty (T.unpack t)

leasesParser :: Parse.Parser [Lease]
leasesParser = Parse.some leaseParser

leaseParser :: Parse.Parser Lease
leaseParser = do
    Parse.whiteSpace
    _ <- Parse.text "lease {"
    Parse.whiteSpace
    therest <- Parse.many
        $   Parse.try interfaceParser
        <|> Parse.try routerParser
        <|> Parse.try ssidParser
        <|> Parse.try unfamiliarLineParser
    Parse.whiteSpace
    _ <- Parse.text "}"
    Parse.whiteSpace
    case getInterface therest of
        Just i -> case getRouter therest of
            Just r -> return Lease
                { lsInterface         = i
                , lsSSID              = getSSID therest
                , lsRouter            = r
                }
            Nothing -> Parse.raiseErr $ Parse.failed "couldn't find the router"
        Nothing -> Parse.raiseErr $ Parse.failed "couldn't find the interface"

  where

    getInterface :: [LeaseInfo] -> Maybe Text
    getInterface lis = case headMay $ filter isInterface lis of
        Just (LIInterface s) -> Just s
        _                    -> Nothing

    getSSID :: [LeaseInfo] -> Maybe Text
    getSSID lis = case headMay $ filter isSSID lis of
        Just (LISSID w) -> Just w
        _               -> Nothing

    getRouter :: [LeaseInfo] -> Maybe Text
    getRouter lis = case headMay $ filter isRouter lis of
        Just (LIRouter w) -> Just w
        _                 -> Nothing

    isInterface (LIInterface _) = True
    isInterface _               = False

    isSSID (LISSID _) = True
    isSSID _          = False

    isRouter (LIRouter _) = True
    isRouter _            = False

interfaceParser :: Parse.Parser LeaseInfo
interfaceParser = do
    Parse.whiteSpace
    _ <- Parse.text "interface"
    Parse.whiteSpace
    i <- Parse.stringLiteral
    _ <- Parse.char ';'
    return $ LIInterface i

routerParser :: Parse.Parser LeaseInfo
routerParser = do
    Parse.whiteSpace
    _ <- Parse.text "option routers"
    Parse.whiteSpace
    r <- Parse.some $ Parse.notChar ';'
    _ <- Parse.char ';'
    return $ LIRouter $ T.pack r

ssidParser :: Parse.Parser LeaseInfo
ssidParser = do
    Parse.whiteSpace
    _ <- Parse.text "ssid"
    Parse.whiteSpace
    r <- Parse.stringLiteral
    _ <- Parse.char ';'
    return $ LISSID r

unfamiliarLineParser :: Parse.Parser LeaseInfo
unfamiliarLineParser = do
    _ <- Parse.some Parse.space
    _ <- Parse.some $ Parse.noneOf ['{', '}', '\n']
    _ <- Parse.newline
    return LIIgnored

getLeases :: ML.LoggingT IO Text
getLeases = runRead LRStatus "cat /var/db/dhclient.leases.*"

