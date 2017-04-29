{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE BangPatterns #-}

module Connect where

import Protolude hiding (handle)
-- import Control.Monad ((>>))
import Util.Run
import Util.Log
import Status.Interface
import Status.Process
import Status.Route
import World
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Thread.Delay as D
import qualified Control.Monad as M
import qualified Control.Monad.Loops as M
import qualified Data.Text as T
import qualified System.Exit as E
import qualified System.Posix.Process as P
import qualified System.Posix.Signals as P
import qualified Control.Monad.Logger as ML
import qualified Data.UnixTime as Time
import Data.UnixTime (UnixTime)
-- import qualified Foreign.C.Types

default (Text, Integer, Double)

-- configure rdomain 0 with vether0
-- set the rdomain
-- connect to physical network
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

