{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Vpn where

import Protolude
import Util
import Wlan
import World
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Thread.Delay as D
import qualified Control.Monad as M
import qualified Data.Maybe as B
import qualified Data.Text as T
import qualified Data.Text.ICU as U
import qualified System.Clock as K
import qualified System.IO as IO
import qualified System.Log.Logger as L

default (T.Text, Integer, Double)

checkVPN :: World -> IO ()
checkVPN world =
    if   B.isJust (configString "vpn_server_public_ip" world)
       && B.isJust (configString "vpn_client_private_ip" world)
       && B.isJust (configString "vpn_server_private_ip" world)
       && B.isJust (configString "vpn_private_netmask" world)
       && B.isJust (configString "ssh_auth_sock_file" world)
    then do
        vif <- atomRead $ vpnIf world
        if B.isJust vif then do
            wok <- atomRead $ wlanOK world
            if wok then do
                vpnProcStatus <- vpnProcOK world
                if vpnProcStatus then do
                    vpnConnStatus <- vpnConnOK world
                    if vpnConnStatus then do
                        L.debugM logPrefix "vpnOK True"
                        oldOK <- atomRead (vpnOK world)
                        M.unless oldOK $ do
                            L.infoM logPrefix "connected to vpn"
                            atomWrite (vpnOK world) True
                    else do
                        L.debugM logPrefix "vpn choice: noconnection"
                        notOK
                        connect world
                else do
                    L.debugM logPrefix "vpn choice: noprocess"
                    notOK
                    connect world
            else do
                L.debugM logPrefix "vpn choice: nowlan"
                notOK
        else do
            L.debugM logPrefix "vpn choice: nointerface"
            notOK
            setupVPNIf world
            connect world
    else do
        L.debugM logPrefix "vpn choice: noconfig"
        notOK

  where
    logPrefix = "winot.checkVPN"
    notOK = do
        L.debugM logPrefix "vpnOK False"
        atomWrite (vpnOK world) False
        M.return ()

    connect w = do
        connectVPN w
        M.return ()

connectVPN :: World -> IO ()
connectVPN world = do
    let logPrefix = "winot.connectVPN"
    let waitXSecondsBeforeDone  = 1
    let waitXSecondsBeforeRetry = 30
    let cpip = configString "vpn_client_private_ip" world
    let spip = configString "vpn_server_private_ip" world
    let nmask = configString "vpn_private_netmask" world

    startTime <- K.getTime K.Realtime
    lastConnectAttempt <- atomRead (lastVPNConnect world)

    M.when ((K.sec startTime - lastConnectAttempt) > waitXSecondsBeforeRetry) $ do
        wif <- atomRead $ wlanIf world
        wlg <- wlanGateway wif
        sas <- sshAuthSock world
        let ip = configString "vpn_server_public_ip" world
        vif <- atomRead $ vpnIf world
        command <- vpnCommand world
        M.when ( B.isJust wlg
               && B.isJust ip
               && B.isJust command
               && B.isJust sas
               && B.isJust vif
               && B.isJust cpip
               && B.isJust spip
               && B.isJust nmask
               ) $ do
            L.infoM logPrefix "connecting to the vpn"
            L.debugM logPrefix $ T.unpack $ "vpn comand: " `T.append` B.fromJust command
            run $ T.concat [ "ifconfig "
                           , B.fromJust vif
                           , " create "
                           , B.fromJust cpip
                           , " "
                           , B.fromJust spip
                           , " netmask "
                           , B.fromJust nmask
                           , " up"
                           ]
            run $ "route delete " `T.append` B.fromJust ip
            run $ "route add " `T.append` B.fromJust ip `T.append` " " `T.append` B.fromJust wlg
            run $ "ifconfig " `T.append` B.fromJust vif `T.append` " down"
            run $ "ifconfig " `T.append` B.fromJust vif `T.append` " up"
            run $ "pkill -5 -f \"" `T.append` B.fromJust command `T.append` "\""
            connectTime <- K.getTime K.Realtime
            atomWrite (lastVPNConnect world) (K.sec connectTime)
            _ <- C.forkIO $ run $ T.concat [ "SSH_AUTH_SOCK="
                                           , B.fromJust sas
                                           , " "
                                           , B.fromJust command
                                           ]
            D.delay $ waitXSecondsBeforeDone * (10::Integer)^(6::Integer)

-- TODO: vpnCommand could change between calls, i.e. between start and cleanup
-- that would lead to the wrong kill commands to be run, need to store the command used
-- so we can reuse it at cleanup time
vpnCommand :: World -> IO (Maybe T.Text)
vpnCommand world = do
    vif <- atomRead $ vpnIf world
    return $ if B.isJust ip && B.isJust vif then
            Just (T.concat [ "ssh -N -w "
                           , T.singleton (T.last $ B.fromJust vif)
                           , ":any "
                           , B.fromJust ip
                           ])
        else
            Nothing
  where
    ip = configString "vpn_server_public_ip" world

sshAuthSock :: World -> IO (Maybe T.Text)
sshAuthSock world =
    if B.isJust fname then do
        contents <- IO.readFile $ T.unpack (B.fromJust fname)
        M.return $ case U.find (U.regex [] "SSH_AUTH_SOCK (.*);") (T.pack contents) of
            Just m  -> U.group 1 m
            Nothing -> Nothing
    else
        M.return Nothing
  where
    fname = configString "ssh_auth_sock_file" world

vpnProcOK :: World -> IO Bool
vpnProcOK world = do
    let logPrefix = "winot.vpnProcOK"
    vcomm <- vpnCommand world
    if B.isJust vcomm then do
        procs <- atomRead $ processList world
        if B.fromJust vcomm `T.isInfixOf` procs then do
            L.debugM logPrefix "True"
            return True
        else do
            L.debugM logPrefix "process not found"
            return False
    else do
        L.debugM logPrefix "command not configured"
        M.return False

vpnConnOK :: World -> IO Bool
vpnConnOK world = do
    bl <- atomRead $ vpnBandwidthLog world
    i <- idle world bl
    case i of
        Nothing -> do
            L.debugM logPrefix "cannot tell if idle"
            test
        Just True -> do
            L.debugM logPrefix "idle"
            test
        Just False -> do
            L.debugM logPrefix "active"
            M.return True
  where
    logPrefix = "winot.vpnConnOK"
    test =
        case configString "vpn_server_private_ip" world of
            Nothing -> do
                L.debugM logPrefix "no server private ip configured"
                return False
            Just ip' -> do
                result <- ping 3 ip'
                if result then do
                    L.debugM logPrefix "ping ok"
                    return True
                else do
                    L.debugM logPrefix "ping bad"
                    return False

setupVPNIf :: World -> IO ()
setupVPNIf world = do
    let logPrefix = "winot.setupVPNIf"
    ifs <- atomRead (interfaceList world)
    let vif = B.fromMaybe (firstIfAvailable "tun" ifs) configIf
    L.debugM logPrefix $ T.unpack $ "chose " `T.append` vif
    atomWrite (vpnIf world) (Just vif)
  where
    configIf = configString "vpn_if" world

vpnGateway :: T.Text -> T.Text -> Maybe T.Text
vpnGateway rl ip = case U.find (U.regex [U.Multiline] (
    "^" `T.append` ip `T.append` " *" `T.append` "(" `T.append` ipPattern `T.append` ")")) rl of
        Just m  -> U.group 1 m
        Nothing -> Nothing

