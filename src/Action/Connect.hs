{-# OPTIONS_GHC -Wall -Werror  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Action.Connect where

import Protolude hiding (handle)
import Util.Log
import Util.Run
import Status.Interface
import qualified Control.Concurrent as C
import qualified Control.Monad.Logger as ML
import qualified Control.Concurrent.STM as S
import Data.List ((\\))

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
        run LRAction "ifconfig vether0 group stayd_wlan"
        run LRAction "route delete default"
        run LRAction "route add default 192.168.211.1"
        return False

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

connect :: Interface -> ML.LoggingT IO ()
connect i =
    case ifLock i of
        Just lock -> do
            gotLock <- liftIO $ S.atomically $ S.tryPutTMVar lock ()
            when gotLock $ do
                _ <- liftIO $ C.forkIO $ runMyLogs $ do
                    $(myLogTH) LLDevInfo [Tag "youarehere"] Nothing
                    helper i
                    liftIO $ S.atomically $ S.takeTMVar lock
                    return ()
                return ()
        Nothing -> return ()
  where
    helper i'
      | ifdClass (ifDriver i') == ICwireless = connectWireless i'
      | otherwise = return ()

connectWireless :: Interface -> ML.LoggingT IO ()
connectWireless _ = putStrLn ("hello, wireless" :: Text)

-- load wireless config
-- scan for networks
-- find matching network with strongest bssid
-- if none, give up, otherwise continue
-- connect to wireless network
-- wait until connection is made
-- get IP from DHCP server
-- set default gateway to use raw connection

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
