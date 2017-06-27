{-# OPTIONS_GHC -Wall -Werror  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Action.Connect where

import Protolude hiding (handle)
import Util.Log
import Util.Run
import Util.Misc
import Status.Connection
import Status.Interface
import Status.Filter
import Status.Lease
import World
import qualified Control.Concurrent as C
import qualified Control.Monad.Logger as ML
import qualified Control.Concurrent.STM as S
import Data.List ((\\), elemIndex)
import qualified Control.Concurrent.Thread.Delay as D
import qualified Data.Text as T
import Net.Types hiding (getIP)
import qualified Net.IPv4.Text as IP

-- TODO: delete extraneous filterAnchors, i.e. from removed interfaces
-- TODO: lock down traffic via PF so we don't leak from RD0 or others
-- TODO: delete PF entries for missing interfaces, probably should manage PF entries separately from interfaces entirely
-- TODO: urndis driver should be lumped with mobile instead of with PCI devs for prioritization purposes
checkAndFixRD0 :: World -> ML.LoggingT IO Bool
checkAndFixRD0 w =
    case find (\x -> interface x == "vether0") (woInterfaces w) of
        Just i -> checkAndFixVE0 i
        Nothing -> do
            $(myLogTH) LLDevInfo
                [Tag "connect", Tag "bad"] $
                Just "missing vether0"
            createVE0
            return False
            -- if result then checkAndFixVE0 w i else return False

checkAndFixVE0 :: Interface -> ML.LoggingT IO Bool
checkAndFixVE0 i =
    case ifRDomain i of
        Nothing -> checkAndFixVE0IP i
        Just _ -> do
            result <- runEC LRAction "ifconfig vether0 rdomain 0"
            if wasSuccess result then
                checkAndFixVE0IP i
            else do
                $(myLogTH) LLDevInfo
                    [Tag "connect", Tag "bad", Tag (interface i)] $
                    Just "couldn't fix rdomain on vether0"
                return False

checkAndFixVE0IP :: Interface -> ML.LoggingT IO Bool
checkAndFixVE0IP i =
    case ifIPv4Detail i of
        Just ipd ->
            if Just (ifIPv4IP ipd) == IP.decode "192.168.211.1"
            then checkAndFixVE0Gateway i else fixIt
        Nothing -> fixIt
  where
    fixIt = do
        result <- runEC LRAction "ifconfig vether0 inet 192.168.211.1/32"
        if wasSuccess result then
            checkAndFixVE0Gateway i
        else do
            $(myLogTH) LLDevInfo
                [Tag "connect", Tag "bad", Tag (interface i)] $
                Just "couldn't fix vether0 ip"
            return False

checkAndFixVE0Gateway :: Interface -> ML.LoggingT IO Bool
checkAndFixVE0Gateway i =
    case ifIPv4Detail i of
        Just ipd -> case defaultGateway i of
            Just dg ->
                if dg == ifIPv4IP ipd
                then checkAndFixDNS else fixIt ipd
            Nothing -> fixIt ipd
        Nothing -> do
            $(myLogTH) LLDevInfo
                [Tag "connect", Tag "bad", Tag (interface i)] $
                Just "missing vether0 ip"
            return False
  where
    fixIt ipd' = do
        removeDefaultRoutes i
        result <- runEC LRAction $ "route add default " <> IP.encode (ifIPv4IP ipd')
        if wasSuccess result then
            checkAndFixDNS
        else do
            $(myLogTH) LLDevInfo
                [Tag "connect", Tag "bad", Tag (interface i)] $
                Just "couldn't fix rd0 gateway"
            return False

-- TODO: implement this properly
checkAndFixDNS :: ML.LoggingT IO Bool
checkAndFixDNS = return True
    -- run LRAction "rcctl -f start unbound"

createVE0 :: ML.LoggingT IO ()
createVE0 = do
    run LRAction "ifconfig vether0 inet 192.168.211.1/32"
    run LRAction "ifconfig vether0 rdomain 0"
    run LRAction "route delete default"
    run LRAction "route add default 192.168.211.1"
    run LRAction "rcctl -f start unbound"

-- make certain every physical device is in its own rdomain
checkRdomains :: [Interface] -> ML.LoggingT IO Bool
checkRdomains ifs = do
    results <- helper True [] ifs
    return $ fst3 results
  where
    helper :: Bool -> [Interface] -> [Interface] -> ML.LoggingT IO (Bool, [Interface], [Interface])
    helper nochanges done [] = return (nochanges, done, [])
    helper nochanges done todo@(x:xs) = do
        let allIfs = done ++ todo
        if x `elem` connectableInterfaces allIfs && overlap x allIfs then do
            run LRAction $ "ifconfig " <> interface x <> " rdomain " <> show (next allIfs)
            helper False (done ++ [x { ifRDomain = Just $ next allIfs }]) xs
        else
            helper nochanges (done ++ [x]) xs
    fst3 (a,_,_) = a
    used xs = sort $ mapMaybe ifRDomain $ connectableInterfaces xs
    next xs = fromMaybe 0 $ headMay $ filter (`notElem` used xs) [1..]
    overlap x xs = isNothing (ifRDomain x) || ifRDomain x `elem` map Just (used $ xs \\ [x])

connect :: World -> Interface -> ML.LoggingT IO ()
connect w i =
    case ifLock i of
        Just lock -> do
            gotLock <- liftIO $ S.atomically $ S.tryPutTMVar lock ()
            when gotLock $ do
                _ <- liftIO $ C.forkIO $ runMyLogs $ do
                    $(myLogTH) LLDevInfo [Tag "lock", Tag "gotit", Tag (interface i)] Nothing
                    checkAndFix w i
                    liftIO $ S.atomically $ S.takeTMVar lock
                    $(myLogTH) LLDevInfo [Tag "lock", Tag "released", Tag (interface i)] Nothing
                    return ()
                return ()
        Nothing -> return ()

checkAndFix :: World -> Interface -> ML.LoggingT IO ()
checkAndFix w i
    -- | ifdClass (ifDriver i) == ICmobile = return ()
    | not (rdomainOK i) = do
        $(myLogTH) LLDevInfo
            [Tag "connect", Tag "bad", Tag (interface i)] $
            Just "incorrect rdomain"
        setReady w i False
    | not (upOK i) = do
        $(myLogTH) LLDevInfo
            [Tag "connect", Tag "bad", Tag (interface i)] $
            Just "interface down"
        setReady w i False
        bu <- bringIfUp i
        when (wasSuccess bu) $ checkAndFix w
            i { ifStatus = (ifStatus i) { ifUp = True } }
    | not(wireOK i) = do
        $(myLogTH) LLDevInfo
            [Tag "connect", Tag "bad", Tag (interface i)] $
            Just "not plugged into hub"
        setReady w i False
    | ifdClass (ifDriver i) == ICwireless
    && (not (nwidOK i) || not (wpakeyOK i) || not (strengthOK i)) = do
        $(myLogTH) LLDevInfo
            [Tag "connect", Tag "bad", Tag (interface i)] $
            Just "bad link"
        setReady w i False
        sac <- scanAndConnect i
        when sac $ do -- these always need to happen after getting the link, so don't wait until next loop
            getIP i
            -- fixNAT w i
            return ()
    | not (ipOK i) = do
        $(myLogTH) LLDevInfo
            [Tag "connect", Tag "bad", Tag (interface i)] $
            Just "missing ip"
        setReady w i False
        getIP i
    -- TODO: using IPSEC+GRE leads to a hang after a few minutes under OpenBSD 6.1
    -- | ifdClass (ifDriver i) == ICwireless = do
    --     -- TODO: check that bandwidth usage is low before believing dropped pings
    --     ok <- vpnReachable i
    --     checkAndFixVPN w i ok
    | otherwise = do
        ok <- internetOK i
        checkAndFixUnprotectedConnection w i ok

checkAndFixVPN :: World -> Interface -> Bool -> ML.LoggingT IO ()
checkAndFixVPN w i ok
    | not ok = do
        $(myLogTH) LLDevInfo
            [Tag "connect", Tag "bad", Tag (interface i)] $
            Just "cannot ping vpn server"
        setReady w i False
    | otherwise = do
        ipsok <- ipsecOK i
        checkAndFixIPSec w i ipsok

checkAndFixIPSec :: World -> Interface -> Bool -> ML.LoggingT IO ()
checkAndFixIPSec w i ok
    | not ok = do
        $(myLogTH) LLDevInfo
            [Tag "connect", Tag "bad", Tag (interface i)] $
            Just "cannot ping over ipsec tunnel"
        setReady w i False
        fixIpsec i
    | otherwise = do
        greok <- greOK i
        checkAndFixGRE w i greok

checkAndFixGRE :: World -> Interface -> Bool -> ML.LoggingT IO ()
checkAndFixGRE w i ok
    | not ok = do
        $(myLogTH) LLDevInfo
            [Tag "connect", Tag "bad", Tag (interface i)] $
            Just "cannot ping over gre tunnel"
        setReady w i False
        fixGre i
    | not (gatewayOK' w i) = do
        $(myLogTH) LLDevInfo
            [Tag "connect", Tag "bad", Tag (interface i)] $
            Just "bad gateway"
        setReady w i False
        fixGateway' i
    | not (natOK' w i) = do
        $(myLogTH) LLDevInfo
            [Tag "connect", Tag "bad", Tag (interface i)] $
            Just "nat config is bad"
        setReady w i False
        fixNAT' w i
    | otherwise = do
        $(myLogTH) LLDevInfo
            [Tag "connect", Tag (interface i)] $
            Just "ok"
        setReady w i True

checkAndFixUnprotectedConnection :: World -> Interface -> Bool -> ML.LoggingT IO ()
checkAndFixUnprotectedConnection w i ok
    | not (gatewayOK i) = do
        $(myLogTH) LLDevInfo
            [Tag "connect", Tag "bad", Tag (interface i)] $
            Just "bad gateway"
        setReady w i False
        fixGateway i
    | not ok = do
        $(myLogTH) LLDevInfo
            [Tag "connect", Tag "bad", Tag (interface i)] $
            Just "cannot ping internet"
        setReady w i False
    | not (natOK w i) = do
        $(myLogTH) LLDevInfo
            [Tag "connect", Tag "bad", Tag (interface i)] $
            Just "nat config is bad"
        setReady w i False
        fixNAT w i
    | otherwise = do
        $(myLogTH) LLDevInfo
            [Tag "connect", Tag (interface i)] $
            Just "ok"
        setReady w i True

    -- TODO: check that unbound is up

ipsecOK :: Interface -> ML.LoggingT IO Bool
ipsecOK i =
    case ifRDomain i of
        Just rd -> pingVia rd 5 $ "192.168.99." <> show (rd*2)
        Nothing -> return False

fixIpsec :: Interface -> ML.LoggingT IO ()
fixIpsec i =
    case ifRDomain i of
        Just rd -> case idealDefaultGateway i of
            Just ig -> do
                run LRAction $ "pkill -T " <> show rd <> " iked"
                run LRAction $ "route -T " <> show rd <> " exec ipsecctl -F"
                run LRAction $ "route -T " <> show rd <> " delete " <> "104.238.182.194"
                run LRAction $ "route -T " <> show rd <> " add " <> "104.238.182.194" <> " " <> IP.encode ig
                run LRAction
                    $ "ifconfig vether" <> show rd
                    <> " "
                    <> "192.168.99." <> show (rd*2+1) <> "/31"
                    <> " "
                    <> "rdomain " <> show rd
                run LRAction $ "ifconfig enc" <> show rd <> " rdomain " <> show rd
                run LRAction
                    $ "route -T "
                    <> show rd
                    <> " exec iked -D NAME=wlan -D FROM=\"192.168.99."
                    <> show (rd*2+1)
                    <> "\" -D TO=\"192.168.99."
                    <> show (rd*2)
                    <> "\" -D NIC="
                    <> interface i
                    <> " -D RDOMAIN="
                    <> show rd
                    <> " -D SRCID=\"vpn"
                    <> show rd
                    <> "@maybe.ggr.com\" -D TAP=\"enc"
                    <> show rd
                    <> "\" -D PEER=\""
                    <> "104.238.182.194"
                    <> "\" -D DSTID=\"vpn.ggr.com\""
            Nothing -> return ()
        Nothing -> return ()

greOK :: Interface -> ML.LoggingT IO Bool
greOK i =
    case ifRDomain i of
        Just rd -> pingVia rd 3 $ "192.168.13." <> show (rd*2)
        Nothing -> return False

fixGre :: Interface -> ML.LoggingT IO ()
fixGre i =
    case ifRDomain i of
        Just rd -> do
            run LRAction "sysctl net.inet.gre.allow=1"
            run LRAction $ "ifconfig gre" <> show rd <> " rdomain " <> show rd
            run LRAction $ "ifconfig gre" <> show rd <> " 192.168.13." <> show (rd*2+1) <> "/31"
            run LRAction
                $ "ifconfig gre" <> show rd
                <> " "
                <> "192.168.13." <> show (rd*2+1) <> "/31"
                <> " "
                <> "192.168.13." <> show (rd*2)
            run LRAction $ T.unwords
                [ "ifconfig"
                , "gre" <> show rd
                , "tunnel"
                , "192.168.99." <> show (rd*2+1)
                , "192.168.99." <> show (rd*2)
                , "tunneldomain"
                , show rd
                ]
        Nothing -> return ()

wireOK :: Interface -> Bool
wireOK i = case ifStatusDetail (ifStatus i) of
    Just s -> s /= "no carrier"
    Nothing -> True

setReady :: World -> Interface -> Bool -> ML.LoggingT IO ()
setReady w i s =
    if s then
        case ifReady $ ifStatus i of
            Just ir -> liftIO $ atomWrite ir s
            Nothing ->clearNAT w i
    else
        clearNAT w i

rdomainOK :: Interface -> Bool
rdomainOK = isJust . ifRDomain

upOK :: Interface -> Bool
upOK = ifUp . ifStatus

bringIfUp :: Interface -> ML.LoggingT IO ExitCode
bringIfUp i' = runEC LRAction $ "ifconfig " <> interface i' <> " up"

bringIfDown :: Interface -> ML.LoggingT IO ExitCode
bringIfDown i' = runEC LRAction $ "ifconfig " <> interface i' <> " down"

nwidOK :: Interface -> Bool
nwidOK i' = case ifWirelessDetail i' of
    Just iwd -> case ifSSID iwd of
        Just _ -> True
        Nothing -> False
    Nothing -> False

wpakeyOK :: Interface -> Bool
wpakeyOK _ = True -- TODO: check that the WPAKEY exists unless we're using an open network

strengthOK :: Interface -> Bool
strengthOK _ = True -- TODO: actually check strength

myFilters :: World -> Interface -> [Filter]
myFilters w i = case decidePriority w i of
    Just pri -> case find (\x -> anName x == pri) $ woFilterAnchors w of
        Just fa -> anFilters fa
        Nothing -> []
    Nothing -> []

natOK :: World -> Interface ->Bool
natOK w i = case ifRDomain i of
    Just rd -> rd >= 1 && rd <= 26 && -- so we don't go beyond the letters A and Z in the group name
        case ifIPv4Detail i of
            Just ipd -> case interfaceGroup i of
                Just ig ->
                      FiNATToPrivate
                        { npNetwork = ifIPv4Netmask ipd
                        , npRTable  = rd
                        , npGateway = ifIPv4IP ipd
                        } `elem` myFilters w i
                    && FiNATToInternet
                        { niGroup   = ig
                        , niRTable  = rd
                        , niGateway = ifIPv4IP ipd
                        } `elem` myFilters w i
                Nothing -> False
            Nothing -> False
    Nothing -> False

natOK' :: World -> Interface ->Bool
natOK' w i = case ifRDomain i of
    Just rd -> rd >= 1 && rd <= 26 && -- so we don't go beyond the letters A and Z in the group name
        case ifIPv4Detail i of
            Just ipd -> case interfaceGroup i of
                Just ig -> case IP.decode $ "192.168.13." <> show (rd*2+1) of
                    Just dg ->
                          FiNATToPrivate
                            { npNetwork = ifIPv4Netmask ipd
                            , npRTable  = rd
                            , npGateway = ifIPv4IP ipd
                            } `elem` myFilters w i
                        && FiNATToInternet
                            { niGroup   = ig
                            , niRTable  = rd
                            , niGateway = dg
                            } `elem` myFilters w i
                    Nothing -> False
                Nothing -> False
            Nothing -> False
    Nothing -> False

fixGateway :: Interface -> ML.LoggingT IO ()
fixGateway i =
    case ifRDomain i of
        Just rd -> case ifIPv4Detail i of
            Just ipd -> if ifdClass (ifDriver i) == ICmobile then do
                    removeDefaultRoutes i
                    run LRAction $ "route -T " <> show rd <> " add default " <> IP.encode (ifIPv4IP ipd)
                else
                    case ifIPv4Lease ipd of
                    Just l -> do
                        removeDefaultRoutes i
                        run LRAction $ "route -T " <> show rd <> " add default " <> lsRouter l
                    Nothing -> return ()
            Nothing -> return ()
        Nothing -> return ()

fixGateway' :: Interface -> ML.LoggingT IO ()
fixGateway' i =
    case ifRDomain i of
        Just rd -> do
            removeDefaultRoutes i
            run LRAction $ "route -T " <> show rd <> " add default 192.168.13." <> show (rd*2)
        Nothing -> return ()

-- TODO: have this actually delete all the default routes, not just one of them
removeDefaultRoutes :: Interface -> ML.LoggingT IO ()
removeDefaultRoutes i = helper $ defaultGateways i
  where
    helper :: [IPv4] -> ML.LoggingT IO ()
    helper [] = return ()
    helper (_:xs) =
        case ifRDomain i of
            Just rd -> do
                run LRAction $ "route -T " <> show rd <> " delete default"
                helper xs
            Nothing -> return ()

scanAndConnect :: Interface -> ML.LoggingT IO Bool
scanAndConnect i = do
    unless (modeOK i) (fixMode i)
    wns <- wirelessNetworks
    case wnNetworks wns of
        (_:_) -> case preferredNetwork wns of
            Just pn ->
                case wnSSID pn of
                    Just nwid -> do
                        $(myLogTH) LLDevInfo [Tag "connect"] $ Just $ "preferred network is " <> nwid
                        case wnPassword pn of
                            Just wpakey -> connectToWN i nwid wpakey
                            Nothing -> do
                                $(myLogTH) LLDevInfo
                                    [Tag "connect", Tag "bad", Tag (interface i)] $
                                    Just "missing password"
                                return False
                    Nothing -> do
                        $(myLogTH) LLDevInfo
                            [Tag "connect", Tag "bad", Tag (interface i)] $
                            Just "missing SSID"
                        return False
            Nothing -> do
                $(myLogTH) LLDevInfo
                    [Tag "connect", Tag "bad", Tag (interface i)] $
                    Just "no familiar networks"
                liftIO $ D.delay $ 15*1000000
                return False
        [] -> do
            $(myLogTH) LLDevInfo
                [Tag "connect", Tag "bad", Tag (interface i)] $
                Just "no networks found, interface in a bad state?"
            _ <- bringIfDown i -- restart interface, sometimes it helps
            return False

-- TODO: 11n and 11g support is unreliable, use 11a instead
modeOK :: Interface -> Bool
modeOK i = case ifMediaDetail i of
    Just ifmd -> ifmModePreference ifmd == "autoselect mode 11a"
    Nothing -> False

-- TODO: 11n and 11g support is unreliable, use 11a instead
fixMode :: Interface -> ML.LoggingT IO ()
fixMode i = run LRAction $ "ifconfig " <> interface i <> " mode 11a"

connectToWN :: Interface -> Text -> Text -> ML.LoggingT IO Bool
connectToWN i n w = do
    run LRAction $ T.unwords
        ["ifconfig", interface i, "nwid", "\"" <> n <> "\"", "wpakey", "\"" <> w <> "\"", "up"]
    waitForConnection i

getIP :: Interface -> ML.LoggingT IO ()
getIP i =
    if ifdName (ifDriver i) == "umb" then do
        -- umb has two 'ups' the one on the first line that all IFs have and the
        -- up on the status line...to give umb an IP, get the status line to
        -- be 'active' by bringing umb up even if its already up on the first
        -- line...don't use dhclient, that will just generate an error
        _ <- bringIfDown i
        _ <- bringIfUp i
        return ()
    else do -- TODO: this is probably not the right action for many other drivers
        run LRAction $ "dhclient " <> interface i
        return ()

clearNAT :: World -> Interface -> ML.LoggingT IO ()
clearNAT w i = case decidePriority w i of
    Just pri -> case find (\x -> pri == anName x) (woFilterAnchors w) of
        Just fa -> case anFilters fa of
            [] -> return ()
            _ -> run LRAction $ "pfctl -a stayd/" <> pri <> " -F rules"
        Nothing -> return ()
    Nothing -> return ()

fixNAT' :: World -> Interface -> ML.LoggingT IO ()
fixNAT' w i = do
    $(myLogTH) LLDevInfo [Reason LRAction, Tag "youarehere"] Nothing
    case ifRDomain i of
        Just r ->case interfaceGroup i of
            Just ig -> case decidePriority w i of
                Just pri -> do
                    run LRAction $ "pfctl -a stayd/" <> pri <> " -F rules"
                    run LRAction $
                          "echo \""
                        <> "match out to "
                            <> interface i <> ":network"
                            <> " nat-to " <> interface i <> ":0 rtable " <> show r
                        <> "\n"
                        <> "match out on " <> ig
                            <> " inet from any to ! <private> rtable " <> show r
                            <> " nat-to " <> "192.168.13." <> show (r*2+1)
                        <> "\" | pfctl -a stayd/" <> pri <> " -f -"
                Nothing -> return ()
            Nothing -> return ()
        Nothing -> return ()

fixNAT :: World -> Interface -> ML.LoggingT IO ()
fixNAT w i = do
    $(myLogTH) LLDevInfo [Reason LRAction, Tag "youarehere"] Nothing
    case ifRDomain i of
        Just r ->case interfaceGroup i of
            Just ig -> case decidePriority w i of
                Just pri -> do
                    run LRAction $ "pfctl -a stayd/" <> pri <> " -F rules"
                    run LRAction $
                          "echo \""
                        <> "match out to "
                            <> interface i <> ":network"
                            <> " nat-to " <> interface i <> ":0 rtable " <> show r
                        <> "\n"
                        <> "match out on " <> ig
                            <> " inet from any to ! <private> rtable " <> show r
                            <> " nat-to " <> interface i <> ":0"
                        <> "\" | pfctl -a stayd/" <> pri <> " -f -"
                Nothing -> return ()
            Nothing -> return ()
        Nothing -> return ()

interfaceGroup :: Interface -> Maybe Text
interfaceGroup _ = Just "vether0"

-- interfaceGroup :: Interface -> Maybe Text
-- interfaceGroup i = case ifRDomain i of
--     Just r -> if r >= 1 && r <= 26
--         then Just $ "stayd" <> T.pack [chr(ord 'A' + fromIntegral r - 1)]
--         else Nothing
--     Nothing -> Nothing

internetOK :: Interface -> ML.LoggingT IO Bool
internetOK i =
    case ifRDomain i of
        Just rd -> pingVia rd 3 "8.8.8.8"
        Nothing -> return False

vpnReachable :: Interface -> ML.LoggingT IO Bool
vpnReachable i =
    case ifRDomain i of
        Just rd -> pingVia rd 3 "104.238.182.194"
        Nothing -> return False

waitForConnection :: Interface -> ML.LoggingT IO Bool
waitForConnection i = helper 1
  where
    helper :: Integer -> ML.LoggingT IO Bool
    helper count = do
        -- TODO use the World view of ifconfig instead of polling it again here
        a <- runEC LRStatus $ "ifconfig " <> interface i <> " | grep status | grep active"
        case a of
            ExitSuccess -> return True
            ExitFailure _ ->
                if count < 30 then do
                    liftIO $ D.delay $ 1*1000000
                    helper $ count+1
                else return False

ipOK :: Interface -> Bool
ipOK i = isJust $ ifIPv4Detail i

gatewayOK :: Interface -> Bool
gatewayOK i = case defaultGateway i of
    Just dg -> case idealDefaultGateway i of
        Just ig -> dg == ig
        Nothing -> False
    Nothing -> False

-- TODO: refactor routes to be associated with rdomains instead of interfaces
-- TODO: refactor default gateways to be associated with rdomains instead of interfaces
gatewayOK' :: World -> Interface -> Bool
gatewayOK' w i = case ifRDomain i of
    Just rd -> case find (\x -> interface x == ("gre" <> show rd)) (woInterfaces w) of
        Just gre -> case defaultGateway gre of
            Just dg -> case IP.decode $ "192.168.13." <> show (rd*2) of
                Just ig -> dg == ig
                Nothing -> False
            Nothing -> False
        Nothing -> False
    Nothing -> False

idealDefaultGateway :: Interface -> Maybe IPv4
idealDefaultGateway i =
    case ifIPv4Detail i of
        Just ipd ->
            if ifdClass (ifDriver i) == ICmobile then
                return $ ifIPv4IP ipd
            else
                case ifIPv4Lease ipd of
                    Just l -> IP.decode (lsRouter l)
                    Nothing -> Nothing
        Nothing -> Nothing

decideGroup :: World -> ML.LoggingT IO (Maybe Text)
decideGroup w = do
    ifs <- liftIO $ filterM isReady $ woInterfaces w
    case lastMay $ sortOn fst $ addPriority w ifs of
        Just pai -> case ifRDomain $ snd pai of
            Just rd ->return $ Just $ show rd <> "rd"
            Nothing -> return Nothing
        Nothing -> return Nothing
  where
    isReady i = case ifReady $ ifStatus i of
        Just r -> atomRead r
        Nothing -> return False
    addPriority w' = map (\x -> (decidePriority w' x, x))

decidePriority :: World -> Interface -> Maybe Text
decidePriority w i
    | ifdClass (ifDriver i) == ICmobile =
        case elemIndex i mobileIfs of
            Just ifs -> Just $ "1" <> T.pack [chr(ord 'A' + ifs)]
            Nothing -> Nothing
    | ifdClass (ifDriver i) == ICwireless =
        case elemIndex i wirelessIfs of
            Just ifs -> Just $ "2" <> T.pack [chr(ord 'A' + ifs)]
            Nothing -> Nothing
    | ifdClass (ifDriver i) == ICusb =
        case elemIndex i usbIfs of
            Just ifs -> Just $ "3" <> T.pack [chr(ord 'A' + ifs)]
            Nothing -> Nothing
    | ifdClass (ifDriver i) == ICpci =
        case elemIndex i pciIfs of
            Just ifs -> Just $ "4" <> T.pack [chr(ord 'A' + ifs)]
            Nothing -> Nothing
    | otherwise = Nothing
  where
    mobileIfs = filter (\x -> ifdClass (ifDriver x) == ICmobile) $ woInterfaces w
    wirelessIfs = filter (\x -> ifdClass (ifDriver x) == ICwireless) $ woInterfaces w
    usbIfs = filter (\x -> ifdClass (ifDriver x) == ICusb) $ woInterfaces w
    pciIfs = filter (\x -> ifdClass (ifDriver x) == ICpci) $ woInterfaces w

-- create ipsec tunnel
-- create gre tunnel
-- add route to vpn server
-- set default gateway to use gre tunnel
-- mark as ready

-- function active {
--     count=1
--     while [ $count -lt 30 ]
--     do
--         if ifconfig $1 | grep "status" | grep "active"; then
--             return 0
--         else
--             echo -n "."
--             sleep 1
--         fi
--         count=`expr $count + 1`
--     done
--     return 1
-- }

-- NIC=$(echo $(nics) | head -n1)
-- RDOMAIN=2
-- LOCAL_SUFFIX=5
-- REMOTE_SUFFIX=4

-- while true; do
--     if ! echo "$info" | grep "rdomain $RDOMAIN"; then
--         echo setting rdomain...
--         ifconfig $NIC rdomain $RDOMAIN
--     fi
--     if ! ping -c 3 -w 1 -V $RDOMAIN $gw; then
--         echo connecting to wifi...
--         rm /tmp/wireless.conf
--         echo "device $NIC\ninclude /home/scott/.wireless.conf" > /tmp/wireless.conf
--         /usr/local/bin/wireless /tmp/wireless.conf
--         rm /tmp/wireless.conf
--         ifconfig $NIC -bssid
--         if active $NIC; then
--             dhclient $NIC
--             pfctl -a stayd/wlan -F rules
--             echo "match out on stayd_wlan inet from any to ! <private> rtable $RDOMAIN nat-to $NIC:0\nmatch out to $NIC:network nat-to $NIC:0 rtable $RDOMAIN" | pfctl -a stayd/$RDOMAIN -f -
--             link=1
--         else
--             link=0
--         fi
--     else
--         link=1
--     fi
--     # if [ $link == 1 ]; then
--     #     if ! ping -c 1 -w 1 -V $RDOMAIN 192.168.99.$REMOTE_SUFFIX; then
--     #         echo creating ipsec tunnel...
--     #         pkill -T $RDOMAIN iked
--     #         ifconfig vether$RDOMAIN 192.168.99.$LOCAL_SUFFIX/31 rdomain $RDOMAIN
--     #         ifconfig enc$RDOMAIN rdomain $RDOMAIN
--     #         route -T $RDOMAIN exec iked -f -D NAME=wlan -D FROM="192.168.99.$LOCAL_SUFFIX" -D TO="192.168.99.$REMOTE_SUFFIX" -D NIC=$NIC -D RDOMAIN=$RDOMAIN -D SRCID="vpn$RDOMAIN@maybe.ggr.com" -D TAP="enc$RDOMAIN"
--     #         iked=1
--     #     else
--     #         iked=1
--     #     fi
--     # else
--     #     iked=0
--     # fi
--     # if [ $iked == 1 ]; then
--     #     if ! ping -c 1 -w 1 -V $RDOMAIN 192.168.13.$REMOTE_SUFFIX; then
--     #         echo connecting to gre...
--     #         ifconfig gre$RDOMAIN rdomain $RDOMAIN
--     #         ifconfig gre$RDOMAIN 192.168.13.$LOCAL_SUFFIX/31 192.168.13.$REMOTE_SUFFIX
--     #         ifconfig gre$RDOMAIN tunnel 192.168.99.$LOCAL_SUFFIX 192.168.99.$REMOTE_SUFFIX tunneldomain $RDOMAIN
--     #         gre=1
--     #     else
--     #         gre=1
--     #     fi
--     # else
--     #     gre=0
--     # fi
--     # if [ $iked == 1 ]; then
--     #     if ! ping -c 1 -w 1 -V $RDOMAIN 192.168.14.$REMOTE_SUFFIX; then
--     #         echo creating vxlan tunnel...
--     #         ifconfig vxlan$RDOMAIN rdomain $RDOMAIN vnetid $RDOMAIN
--     #         ifconfig vxlan$RDOMAIN 192.168.14.$LOCAL_SUFFIX/24
--     #         ifconfig vxlan$RDOMAIN tunnel 192.168.99.$LOCAL_SUFFIX 192.168.99.$REMOTE_SUFFIX tunneldomain $RDOMAIN
--     #         gre=1
--     #     else
--     #         gre=1
--     #     fi
--     # else
--     #     gre=0
--     # fi
--     # if [ $gre == 1 ]; then
--     #     # echo "match out on stayd_wlan inet from any to ! <private> rtable $RDOMAIN nat-to 192.168.13.$LOCAL_SUFFIX" | pfctl -a stayd/wlan -f -
--     #     route -T $RDOMAIN delete 104.238.182.194
--     #     route -T $RDOMAIN add 104.238.182.194 $gw
--     #     route -T $RDOMAIN change default 192.168.13.$REMOTE_SUFFIX
--     #     echo ok
--     # fi
--     sleep 5
-- done
