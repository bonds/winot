{-# OPTIONS_GHC -Wall -Werror  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Status.Route where

import Protolude
import Util.Run
import Util.Log
import qualified Text.Trifecta as Parse
import qualified Data.Text as T
import qualified Control.Monad.Logger as ML
-- import qualified GHC.Show

default (Text, Integer, Double)

data Route = Route
    { table           :: Integer
    , destination     :: Text
    , gateway         :: Text
    , flags           :: Text
    , routeInterface  :: Text
    }
    deriving (Show, Eq)

routes :: ML.LoggingT IO [Route]
routes = do
    $(myLogTH) LLDevInfo [Reason LRStatus, Tag "youarehere"] Nothing
    rs <- mapM getRoutesAndParse [0,1,2,3,4]
    return $ concat rs
  where
    getRoutesAndParse rt = do
        rs <- getRoutes rt
        case parseRoutes rs rt of
            Parse.Success result -> return result
            Parse.Failure err -> do
                mapM_
                    ($(myLogTH) LLUserTellDevAppIsBroken [Reason LRStatus, Tag "parseerror"] . Just)
                    (T.lines $ show err)
                return []

parseRoutes :: Text -> Integer -> Parse.Result [Route]
parseRoutes t rt = Parse.parseString (routesParser rt) mempty (T.unpack t)

routesParser :: Integer -> Parse.Parser [Route]
routesParser rt = noTable <|> routeTableParser rt
  where
    -- if the routing table doesn't exist, nothing prints to stdout
    -- and stderr says: "routing table 4: No such file or directory"
    noTable = do
        _ <- Parse.eof
        return []

routeTableParser :: Integer -> Parse.Parser [Route]
routeTableParser rt = do
    _ <- Parse.text "Routing tables\n"
    -- if the routing table exists, but is empty, the "Routing tables"
    -- header line prints to stdout but none of the other header lines
    -- are printed, i.e. no "Internet:" or "Destination" lines
    table' <- Parse.optional $ do
        Parse.whiteSpace
        _ <- Parse.text "Internet:"
        Parse.whiteSpace
        _ <- Parse.text "Destination"
        _ <- Parse.some $ Parse.notChar '\n'
        _ <- Parse.newline
        Parse.some $ routesLineParser rt
    return $ fromMaybe [] table'

routesLineParser :: Integer -> Parse.Parser Route
routesLineParser rt = do
    destination' <- Parse.some $ Parse.notChar ' '
    Parse.whiteSpace
    gateway' <- Parse.some $ Parse.notChar ' '
    Parse.whiteSpace
    flags' <- Parse.some $ Parse.notChar ' '
    Parse.whiteSpace
    _ <- Parse.integer
    Parse.whiteSpace
    _ <- Parse.integer
    Parse.whiteSpace
    _ <- Parse.some $ Parse.notChar ' '
    Parse.whiteSpace
    _ <- Parse.integer
    Parse.whiteSpace
    routeInterface' <- Parse.some $ Parse.noneOf [' ', '\n']
    Parse.whiteSpace
    return Route { table = rt
            , destination     = T.pack destination'
            , gateway         = T.pack gateway'
            , flags           = T.pack flags'
            , routeInterface  = T.pack routeInterface'
            }

getRoutes :: Integer -> ML.LoggingT IO Text
getRoutes rt = runRead LRStatus $ "route -T " <> show rt <> " -n show -inet"

