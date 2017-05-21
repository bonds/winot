{-# OPTIONS_GHC -Wall -Werror  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Status.Filter where

import Protolude
import Util.Run
import Util.Log
import qualified Text.Trifecta as Parse
import qualified Data.Text as T
import qualified Control.Monad.Logger as ML
-- import qualified GHC.Show
import Net.Types
import qualified Net.IPv4.Text as IP
import qualified Net.IPv4.Range.Text as IPR

default (Text, Integer, Double)

--   anchor "2" all {
--     match out on stayd2r inet from any to ! <private> rtable 2 nat-to 10.0.0.136
--     match out inet from any to 10.0.0.0/24 rtable 2 nat-to 10.0.0.136
--   }

data FilterAnchor = FilterAnchor
    { anName    :: Text
    , anFilters :: [Filter]
    } deriving (Show, Eq)

data Filter =
      FiNATToPrivate
        { npNetwork :: IPv4Range
        , npRTable  :: Integer
        , npGateway :: IPv4
        }
    | FiNATToInternet
        { niGroup   :: Text
        , niRTable  :: Integer
        , niGateway :: IPv4
        }
    | FiUnrecognized
    deriving (Show, Eq)

filterAnchors :: ML.LoggingT IO [FilterAnchor]
filterAnchors = do
    $(myLogTH) LLDevInfo [Reason LRStatus, Tag "youarehere"] Nothing
    fs <- getFilters
    case parseAnchors fs of
        Parse.Success result -> return result
        Parse.Failure err -> do
            mapM_
                ($(myLogTH) LLUserTellDevAppIsBroken [Reason LRStatus, Tag "parseerror"] . Just)
                (T.lines $ show err)
            return []

parseAnchors :: Text -> Parse.Result [FilterAnchor]
parseAnchors t = Parse.parseString anchorsParser mempty (T.unpack t)

anchorsParser :: Parse.Parser [FilterAnchor]
anchorsParser = do
    anchors' <- Parse.some $ anchorParser <|> unrecParser
    return $ catMaybes anchors'
  where
    unrecParser = do
        _ <- Parse.some $ Parse.notChar '\n'
        _ <- Parse.newline
        return Nothing

anchorParser :: Parse.Parser (Maybe FilterAnchor)
anchorParser = do
    Parse.whiteSpace
    _ <- Parse.text "anchor"
    Parse.whiteSpace
    name <- Parse.stringLiteral
    Parse.whiteSpace
    _ <- Parse.text "all"
    Parse.whiteSpace
    _ <- Parse.char '{'
    Parse.whiteSpace
    filters <- Parse.many filterParser
    Parse.whiteSpace
    _ <- Parse.char '}'
    Parse.whiteSpace
    return $ Just FilterAnchor
        { anName    = name
        , anFilters = filters
        }

--   anchor "2" all {
--     match out on stayd2r inet from any to ! <private> rtable 2 nat-to 10.0.0.136
--     match out inet from any to 10.0.0.0/24 rtable 2 nat-to 10.0.0.136
--   }

filterParser :: Parse.Parser Filter
filterParser = n2pParser <|> n2iParser

--     match out inet from any to 10.0.0.0/24 rtable 2 nat-to 10.0.0.136
n2pParser :: Parse.Parser Filter
n2pParser = do
    Parse.whiteSpace
    _ <- Parse.text "match out inet from any to"
    Parse.whiteSpace
    network <- Parse.some $ Parse.notChar ' '
    Parse.whiteSpace
    _ <- Parse.text "rtable"
    Parse.whiteSpace
    rtable <- Parse.integer
    Parse.whiteSpace
    _ <- Parse.text "nat-to"
    Parse.whiteSpace
    gateway <- Parse.some $ Parse.noneOf [' ', '\n']
    _ <- Parse.newline
    case IPR.decode (T.pack network) :: Maybe IPv4Range of
        Just nw -> case IP.decode (T.pack gateway) :: Maybe IPv4 of
            Just gw ->
                return FiNATToPrivate
                    { npNetwork         = nw
                    , npRTable          = rtable
                    , npGateway         = gw
                    }
            Nothing -> Parse.raiseErr $ Parse.failed "couldn't parse gateway"
        Nothing -> Parse.raiseErr $ Parse.failed "couldn't parse ip range"

--     match out on stayd2r inet from any to ! <private> rtable 2 nat-to 10.0.0.136
n2iParser :: Parse.Parser Filter
n2iParser = do
    Parse.whiteSpace
    _ <- Parse.text "match out on"
    Parse.whiteSpace
    group' <- Parse.some $ Parse.notChar ' '
    Parse.whiteSpace
    _ <- Parse.text "inet from any to ! <private> rtable"
    Parse.whiteSpace
    rtable <- Parse.integer
    Parse.whiteSpace
    _ <- Parse.text "nat-to"
    Parse.whiteSpace
    gateway <- Parse.some $ Parse.noneOf [' ', '\n']
    _ <- Parse.newline
    case IP.decode (T.pack gateway) :: Maybe IPv4 of
        Just gw ->
            return FiNATToInternet
                { niGroup           = T.pack group'
                , niRTable          = rtable
                , niGateway         = gw
                }
        Nothing -> Parse.raiseErr $ Parse.failed "couldn't parse gateway"

getFilters :: ML.LoggingT IO Text
getFilters = runRead LRStatus "pfctl -a \"stayd/*\" -s rules"

