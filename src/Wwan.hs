{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Wwan where

import Protolude
import Prelude (($))
import Util
import World
import qualified Control.Concurrent.Thread.Delay as D
import qualified Control.Monad as M
import qualified Data.List as DL
import qualified Data.Maybe as B
import qualified Data.Text as T
import qualified Data.Text.ICU as U
import qualified System.Clock as K
import qualified System.Log.Logger as L

default (T.Text, Integer, Double)

checkWWAN :: World -> IO ()
checkWWAN world = do
    let logPrefix = "winot.checkWWAN"

    procList <- atomRead $ processList world
    ifList <- atomRead $ interfaceList world
    let status = wwanEnabled world
              && wwanProcOK procList
              && B.isJust wif
              && wwanConnOK (B.fromJust wif) ifList :: Bool
    L.debugM logPrefix $ T.unpack $ T.concat ["wwanOK ", T.pack (show status)]
    oldOK <- atomRead (wwanOK world)
    M.unless (oldOK == status) $ atomWrite (wwanOK world) status

    if status then M.unless oldOK $ L.infoM logPrefix "connected to wwan"
    else M.when (wwanEnabled world) $ do
        connectWWAN world
        M.return ()

  where
    wif = wwanIf world

connectWWAN :: World -> IO ()
connectWWAN world = do
    let logPrefix = "winot.connectWWAN"
    let waitXSecondsBeforeDone  = 1
    let waitXSecondsBeforeRetry = 30
    L.debugM logPrefix "start"

    startTime <- K.getTime K.Realtime
    lastConnectAttempt <- atomRead (lastWWANConnect world)

    M.when ((K.sec startTime - lastConnectAttempt) > waitXSecondsBeforeRetry) $ do
        let wp = configString "wwan_peer" world
        M.when (B.isJust wp) $ do
            L.infoM logPrefix "connecting to the wwan"
            connectTime <- K.getTime K.Realtime
            atomWrite (lastWWANConnect world) (K.sec connectTime)
            run $ "/usr/sbin/pppd call " `T.append` B.fromJust wp
            D.delay $ waitXSecondsBeforeDone * (10::Integer)^(6::Integer)

wwanGateway :: T.Text -> T.Text -> Maybe T.Text
wwanGateway rl wif =
    case U.find (U.regex [U.Multiline] (T.concat ["(", ipPattern, ").*UHl.*", wif])) rl of
        Just m -> U.group 1 m
        Nothing -> Nothing

wwanProcOK :: T.Text -> Bool
wwanProcOK procs = "/usr/sbin/pppd" `T.isInfixOf` procs

wwanConnOK:: T.Text -> [IFInfo] -> Bool
wwanConnOK wwif infos = "inet" `T.isInfixOf` detailOrEmpty (DL.find nameIsWwif infos)
  where
    nameIsWwif :: IFInfo -> Bool
    nameIsWwif info = name info == wwif

chooseWWANIf :: World -> IO (Maybe T.Text)
chooseWWANIf world = do
    let logPrefix = "winot.chooseWWANIf"
    ifs <- atomRead (interfaceList world)
    let configIf = configString "wwan_if" world
    let wif = B.fromMaybe (firstIfAvailable "ppp" ifs) configIf
    L.debugM logPrefix $ T.unpack $ "chose " `T.append` wif
    M.return $ Just wif

wwanEnabled :: World -> Bool
wwanEnabled world =
    B.isJust (wwanIf world) && B.isJust (configString "wwan_peer" world)

