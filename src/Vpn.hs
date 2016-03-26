{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Vpn where

import Util
import Wlan
import World
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Thread.Delay as D
import qualified Control.Monad as M
import qualified Data.Maybe as B
import qualified Data.Text as T
import qualified Data.Text.ICU as U
import qualified System.Log.Logger as L

default (T.Text, Integer, Double)

checkVPN :: World -> IO ()
checkVPN world =
    if vpnEnabled world then do
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
                else connect world
            else connect world
        else notOK
    else notOK
  where
    logPrefix = "winot.checkVPN"
    notOK = do
        L.debugM logPrefix "vpnOK False"
        atomWrite (vpnOK world) False
        return ()

    connect w = do
        L.debugM logPrefix "vpnOK False"
        atomWrite (vpnOK w) False
        connectVPN w
        return ()

connectVPN :: World -> IO ()
connectVPN world = do
    let logPrefix = "winot.connectVPN"
    let waitXSecondsForVPNToConnect = 30
    wlg <- wlanGateway (wlanIf world)
    sas <- sshAuthSock world
    let ip = configString "vpn_server_public_ip" world
    let vif = vpnIf world
    let vspip = configString "vpn_server_private_ip" world
    M.when (B.isJust wlg && B.isJust ip && B.isJust (vpnCommand world) && B.isJust sas && B.isJust vif) $ do
        L.infoM logPrefix "connecting to the vpn"
        L.debugM logPrefix $ T.unpack $ "vpn comand: " `T.append` B.fromJust (vpnCommand world)
        run $ "route delete " `T.append` B.fromJust ip
        run $ "route add " `T.append` B.fromJust ip `T.append` " " `T.append` B.fromJust wlg
        run $ "ifconfig " `T.append` B.fromJust vif `T.append` " down"
        run $ "ifconfig " `T.append` B.fromJust vif `T.append` " up"
        run $ "pkill -5 -f \"" `T.append` B.fromJust (vpnCommand world) `T.append` "\""
        _ <- C.forkIO $ run $ T.concat [ "SSH_AUTH_SOCK="
                                       , B.fromJust sas
                                       , " "
                                       , B.fromJust (vpnCommand world)
                                       ]
        run "route -T 1 flush"
        run $ "route -T 1 add default " `T.append` B.fromJust vspip
        run $ "route -T 1 add " `T.append` B.fromJust ip `T.append` " " `T.append` B.fromJust wlg
        D.delay $ waitXSecondsForVPNToConnect * (10::Integer)^(6::Integer)

-- TODO: vpnCommand could change between calls, i.e. between start and cleanup
-- that would lead to the wrong kill commands to be run, need to store the command used
-- so we can reuse it at cleanup time
vpnCommand :: World -> Maybe T.Text
vpnCommand world =
    if B.isJust ip && B.isJust vif then
        Just (T.concat [ "ssh -N -w "
                       , T.singleton (T.last $ B.fromJust vif)
                       , ":any "
                       , B.fromJust ip
                       ])
    else
        Nothing
  where
    ip = configString "vpn_server_public_ip" world
    vif = vpnIf world

sshAuthSock :: World -> IO (Maybe T.Text)
sshAuthSock world =
    if B.isJust fname then do
        contents <- readFile $ T.unpack (B.fromJust fname)
        return $ case U.find (U.regex [] "SSH_AUTH_SOCK (.*);") (T.pack contents) of
            Just m  -> U.group 1 m
            Nothing -> Nothing
    else
        return Nothing
  where
    fname = configString "ssh_auth_sock_file" world

vpnProcOK :: World -> IO Bool
vpnProcOK world =
    if B.isJust vcomm then do
        procs <- atomRead $ processList world
        return $ B.fromJust vcomm `T.isInfixOf` procs
    else
        return False
  where
    vcomm = vpnCommand world

vpnConnOK :: World -> IO Bool
vpnConnOK world = do
    let logPrefix = "winot.vpnConnOK"
    L.debugM logPrefix "start"
    maybe (return False) (ping 3) ip
    {-maybe (return False) (ping 3) $ Just "8.8.8.8"-}
  where
    ip = configString "vpn_server_private_ip" world

chooseVPNIf :: World -> IO (Maybe T.Text)
chooseVPNIf world = do
    let logPrefix = "winot.chooseVPNIf"
    ifs <- atomRead (interfaceList world)
    let vif = B.fromMaybe (firstIfAvailable "tun" ifs) configIf
    L.debugM logPrefix $ T.unpack $ "chose " `T.append` vif
    return $ Just vif
  where
    configIf = configString "vpn_if" world

vpnEnabled :: World -> Bool
vpnEnabled world = B.isJust (vpnIf world)
                && B.isJust (configString "vpn_server_public_ip" world)
                && B.isJust (configString "vpn_client_private_ip" world)
                && B.isJust (configString "vpn_server_private_ip" world)
                && B.isJust (configString "vpn_private_netmask" world)
                && B.isJust (configString "ssh_auth_sock_file" world)

vpnGateway :: T.Text -> T.Text -> Maybe T.Text
vpnGateway rl ip = case U.find (U.regex [U.Multiline] (
    "^" `T.append` ip `T.append` " *" `T.append` "(" `T.append` ipPattern `T.append` ")")) rl of
        Just m  -> U.group 1 m
        Nothing -> Nothing

