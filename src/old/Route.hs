{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Route where

import Protolude
import Vpn
import Util.Misc
import Util.Run
import Wlan
import World
import Wwan
import qualified Control.Monad as M
import qualified Data.Maybe as B
import qualified Data.Text as T
import qualified Data.Text.ICU as U
import qualified System.Log.Logger as L

default (T.Text, Integer, Double)

checkRoute :: World -> IO ()
checkRoute world = do
    updateRouteList world
    wlanok <- atomRead $ wlanOK world
    if wlanok then do
        L.debugM logPrefix "route choice: wlanok"
        wlif <- atomRead $ wlanIf world
        wlg <- wlanGateway wlif
        vpnok <- atomRead $ vpnOK world
        let ip = configString "vpn_server_private_ip" world

        if vpnok && B.isJust ip then do
            L.debugM logPrefix "route choice: vpnok"
            rl <- atomRead (routeList world)
            setDefaultRoute rl (B.fromJust ip)
            atomWrite (routeVia world) VPN
        else do
            L.debugM logPrefix "route choice: vpnbad"
            let wovpn = configString "wlan_without_vpn_enabled" world
            if B.isJust wlg && B.isJust wovpn && B.isJust (readMaybe (T.unpack $ B.fromJust wovpn) :: Maybe Bool) then do
                L.debugM logPrefix "route choice: wovpn"
                rl <- atomRead (routeList world)
                setDefaultRoute rl (B.fromJust wlg)
                atomWrite (routeVia world) WLAN
            else do
                L.debugM logPrefix "route choice: nowovpn"
                tryWWAN world
    else do
        L.debugM logPrefix "route choice: wlanbad"
        tryWWAN world
    L.debugM logPrefix "finished"
  where
    logPrefix = "winot.checkRoutes"
    resetRoutes w r = do
        clearDefaultRoute r
        -- clearVPNRoute w
        atomWrite (routeVia w) None
        M.return ()
    tryWWAN w = do
        wwanok <- atomRead $ wwanOK w
        rl <- atomRead (routeList w)
        if wwanok then do
            L.debugM logPrefix "route choice: wwanok"
            wif <- atomRead $ wwanIf w
            if B.isJust wif then do
                L.debugM logPrefix "route choice: wifok"
                let wg = wwanGateway rl (B.fromJust wif)
                if B.isJust wg then do
                    L.debugM logPrefix "route choice: wgok"
                    setDefaultRoute rl (B.fromJust wg)
                    atomWrite (routeVia world) WWAN
                else do
                    L.debugM logPrefix "route choice: wgbad"
                    L.errorM logPrefix "wwan is ok but no wwan gateway?!"
                    L.debugM logPrefix $ T.unpack $ T.concat [".... wif is ", B.fromJust wif]
                    L.debugM logPrefix $ T.unpack $ T.concat [".... rl is ", rl]
                    resetRoutes world rl
            else do
                L.debugM logPrefix "route choice: wifbad"
                L.errorM logPrefix "wwan is ok but no wwan interface?!"
                resetRoutes world rl
        else do
            L.debugM logPrefix "route choice: wwanbad"
            resetRoutes world rl

routeVPNViaWLAN :: T.Text -> Maybe T.Text -> World -> IO ()
routeVPNViaWLAN rl wlg world = do
    let logPrefix = "winot.routeVPNViaWLAN"
    let vspi = configString "vpn_server_public_ip" world
    M.when (B.isJust vspi) $ do
        let vg = vpnGateway rl $ B.fromJust vspi
        M.when (B.isJust vg) $ do
            L.debugM logPrefix $ T.unpack $ T.concat ["vpnSPIP ", B.fromJust vspi]
            L.debugM logPrefix $ T.unpack $ T.concat ["vpnGateway ", B.fromJust vg]
            M.when (B.fromJust vg /= B.fromJust wlg) $ do
                L.infoM logPrefix "adding route to vpn server via wlan"
                clearVPNRoute world
                run $ T.concat ["route add ", B.fromJust vspi, " ", B.fromJust wlg]
                M.return ()

updateRouteList :: World -> IO ()
updateRouteList world = do
    lst <- runRead "route -n show -inet"
    atomWrite (routeList world) lst

defaultRouteIP :: T.Text -> Maybe T.Text
defaultRouteIP rl = case ip of
    Just x  -> U.group 1 x
    Nothing -> Nothing
  where
      ip = U.find (U.regex [U.Multiline] (T.concat ["^default *(", ipPattern, ")"])) rl

setDefaultRoute :: T.Text -> T.Text -> IO ()
setDefaultRoute rl ip = do
    let logPrefix = "winot.setDefaultRoute"
    let drip = defaultRouteIP rl
    M.when (B.isJust drip) $
        L.debugM logPrefix $ T.unpack $ T.concat ["current default gateway = ", B.fromJust drip]
    L.debugM logPrefix $ T.unpack $ T.concat [" target default gateway = ", ip]
    M.unless (drip == Just ip) $ do
        L.infoM "winot.setDefaultRoute" $ T.unpack $ T.concat ["changing default gatway to ", ip]
        clearDefaultRoute rl
        L.debugM logPrefix "add new default route"
        run $ T.concat ["route add default ", ip]
        M.return ()

clearDefaultRoute :: T.Text -> IO ()
clearDefaultRoute rl = do
    let logPrefix = "winot.clearDefaultRoute"
    L.debugM logPrefix "delete existing default route(s)"
    M.replicateM_ (length $ U.findAll (U.regex [U.Multiline] "^default") rl) $ run "route delete default"

clearVPNRoute :: World -> IO ()
clearVPNRoute world = do
    let vspi = configString "vpn_server_public_ip" world
    run $ T.concat ["route delete ", B.fromJust vspi]

