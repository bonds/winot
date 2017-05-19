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
import Data.List ((\\))
import qualified Control.Concurrent.Thread.Delay as D
import qualified Data.Text as T
import Net.Types hiding (getIP)
import qualified Net.IPv4.Text as IP

-- TODO: actually check that all the config for vether0 is correct and fix the broken parts only, as needed, including which physical interface to use for internet acess
checkRD0 :: [Interface] -> ML.LoggingT IO Bool
checkRD0 ifs =
    case find (\x -> interface x == "vether0") ifs of
        Just i ->
            if isJust (ifRDomain i) then fixIt
            else return True
        Nothing ->fixIt
  where
    fixIt = do
        run LRAction "ifconfig vether0 inet 192.168.211.1/32"
        run LRAction "ifconfig vether0 rdomain 0"
        run LRAction "ifconfig vether0 group stayd1r"
        run LRAction "route delete default"
        run LRAction "route add default 192.168.211.1"
        run LRAction "rcctl -f start unbound"
        return False

-- make certain every physical device is in its own rdomain
-- TODO: make certain preferred devices are higher numbered, as the anchors are sorted, and whichever rules are last win
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
                    $(myLogTH) LLDevInfo [Tag "lock", Tag "got"] $ Just $ interface i
                    helper w i
                    liftIO $ S.atomically $ S.takeTMVar lock
                    $(myLogTH) LLDevInfo [Tag "lock", Tag "release"] $ Just $ interface i
                    return ()
                return ()
        Nothing -> return ()
  where
    helper w' i'
      | ifdClass (ifDriver i') == ICwireless = connectWireless w' i'
      | otherwise = do
        $(myLogTH) LLDevInfo [Tag "connect", Tag "badclass"] $ Just $ "unimplemented interface class: " <> show (ifDriver i')
        return ()

connectWireless :: World -> Interface -> ML.LoggingT IO ()
connectWireless world wif
    | not (rdomainOK wif) = do
        $(myLogTH) LLDevInfo [Tag "cw", Tag "badwifi"] $ Just "incorrect rdomain"
        setReady wif False
    | not (upOK wif) = do
        $(myLogTH) LLDevInfo [Tag "cw", Tag "badwifi"] $ Just "interface down"
        setReady wif False
        bu <- bringIfUp wif
        when (wasSuccess bu) $ connectWireless world
            wif { ifStatus = (ifStatus wif) { ifUp = True } }
    | not (nwidOK wif) || not (wpakeyOK wif) || not (strengthOK wif) = do
        $(myLogTH) LLDevInfo [Tag "cw", Tag "badwifi"] $ Just "bad link"
        setReady wif False
        _ <- scanAndConnect wif
        getIP wif
        fixNAT wif
        return ()
        -- sac <- scanAndConnect wif
        -- when sac $ connectWireless wif
        --     { ifWirelessDetail = Just IfWirelessDetail
        --         { ifSSID = Just "fakeonetopasstest"
        --         , ifBSSID = Nothing
        --         , ifStrengthHistory = []
        --         , ifNetworks = WirelessNetworks {wnLastScan = Nothing, wnNetworks = []}
        --         }
        --     }
    | not (ipOK wif) = do
        $(myLogTH) LLDevInfo [Tag "cw", Tag "badwifi"] $ Just "missing ip"
        setReady wif False
        getIP wif
    | not (gatewayOK wif) = do
        $(myLogTH) LLDevInfo [Tag "cw", Tag "badwifi"] $ Just "bad gateway"
        setReady wif False
        fixGateway wif
    | otherwise = do
        internetOK wif >>= remainingChecks
        return ()
  where
    rdomainOK = isJust . ifRDomain
    setReady :: Interface -> Bool -> ML.LoggingT IO ()
    setReady i s = case ifReady $ ifStatus i of
        Just ir -> liftIO $ atomWrite ir s
        Nothing -> return ()
    upOK = ifUp . ifStatus
    bringIfUp i = runEC LRAction $ "ifconfig " <> interface i <> " up"
    nwidOK i = case ifWirelessDetail i of
        Just iwd -> case ifSSID iwd of
            Just _ -> True
            Nothing -> False
        Nothing -> False
    wpakeyOK _ = True -- TODO: check that the WPAKEY exists unless we're using an open network
    strengthOK _ = True -- TODO: actually check strength
    remainingChecks iok
        | not iok = do
            $(myLogTH) LLDevInfo [Tag "cw", Tag "badwifi"] $ Just "cannot ping internet"
            setReady wif False
        | not (natOK wif) = do
            $(myLogTH) LLDevInfo [Tag "cw", Tag "badwifi"] $ Just "nat config is bad"
            setReady wif False
            fixNAT wif
            -- connectWireless wif
        | otherwise = do
            $(myLogTH) LLDevInfo [Tag "cw"] $ Just "ok"
            setReady wif True

    -- TODO: check that unbound is up

natOK :: Interface ->Bool
natOK i = case ifRDomain i of
    Just rd -> case ifFilters i of
        Just iff -> case ifIPv4Detail i of
            Just ipd ->
                  FiNATToPrivate
                    { npNetwork = ifIPv4Netmask ipd
                    , npRTable  = rd
                    , npGateway = ifIPv4IP ipd
                    } `elem` iff
                && FiNATToInternet
                    { niGroup   = "stayd" <> show rd <> "r"
                    , niRTable  = rd
                    , niGateway = ifIPv4IP ipd
                    } `elem` iff
            Nothing -> False
        Nothing -> False
    Nothing -> False

fixGateway :: Interface -> ML.LoggingT IO ()
fixGateway i =
    case ifRDomain i of
        Just rd -> case ifIPv4Detail i of
            Just ipd -> case ifIPv4Lease ipd of
                Just l -> do
                    removeDefaultRoutes i
                    run LRAction $ "route -T " <> show rd <> " add default " <> lsRouter l
                Nothing -> return ()
            Nothing -> return ()
        Nothing -> return ()

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
scanAndConnect wif = do
    wns <- wirelessNetworks
    case preferredNetwork wns of
        Just pn ->
            case wnSSID pn of
                Just nwid -> do
                    $(myLogTH) LLDevInfo [Tag "cw"] $ Just $ "preferred network is " <> nwid
                    case wnPassword pn of
                        Just wpakey -> connectToWN wif nwid wpakey
                        Nothing -> do
                            $(myLogTH) LLDevInfo [Tag "cw", Tag "badwifi"] $ Just "missing password"
                            return False
                Nothing -> do
                    $(myLogTH) LLDevInfo [Tag "cw", Tag "badwifi"] $ Just "missing SSID"
                    return False
        Nothing -> do
            $(myLogTH) LLDevInfo [Tag "cw", Tag "badwifi"] $ Just "no familiar networks"
            liftIO $ D.delay $ 15*1000000
            return False

connectToWN :: Interface -> Text -> Text -> ML.LoggingT IO Bool
connectToWN i n w = do
    run LRAction $ T.unwords
        ["ifconfig", interface i, "nwid", "\"" <> n <> "\"", "wpakey", "\"" <> w <> "\"", "up"]
    waitForConnection i

getIP :: Interface -> ML.LoggingT IO ()
getIP i = run LRAction $ "dhclient " <> interface i

fixNAT :: Interface -> ML.LoggingT IO ()
fixNAT i = do
    $(myLogTH) LLDevInfo [Reason LRAction, Tag "youarehere"] Nothing
    case ifRDomain i of
        Nothing -> return ()
        Just r -> do
            run LRAction $ "pfctl -a stayd/" <> show r <> " -F rules"
            run LRAction $
                  "echo \""
                <> "match out on stayd" <> show r <> "r" -- OpenBSD says groups may not end in digits
                    <> " inet from any to ! <private> rtable " <> show r
                    <> " nat-to " <> interface i <> ":0\n"
                <> "match out to "
                    <> interface i <> ":network"
                    <> " nat-to " <> interface i <> ":0 rtable " <> show r
                <> "\" | pfctl -a stayd/" <> show r <> " -f -"

internetOK :: Interface -> ML.LoggingT IO Bool
internetOK i =
    case ifRDomain i of
        Just rd -> pingVia rd 3 "8.8.8.8"
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
    Just dg -> case ifIPv4Detail i of
        Just ipd -> case ifIPv4Lease ipd of
            Just l -> Just dg == IP.decode (lsRouter l)
            Nothing -> False
        Nothing -> False
    Nothing -> False

-- add NAT rules from vether0 to this connection
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
