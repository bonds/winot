{-# LANGUAGE DeriveGeneric #-}

module Status where

import Protolude
import Prelude (($), (++))
import World
import Wlan
import Wwan
import Util
import Vpn
import qualified Control.Monad as M
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified GHC.Generics as N
import qualified Data.ByteString.Lazy as BS
import qualified System.Clock as K
import qualified Data.Maybe as B

data Status = Status { csUsing    :: ConnectionMedium
                     , csWwan     :: ConnectionStatus
                     , csWlan     :: ConnectionStatus
                     , csWlanSSID :: T.Text
                     , csWlanBSSID :: T.Text
                     , csWlanStrength :: T.Text
                     , csVpn      :: ConnectionStatus
                     , csNetworks :: [WLANNetwork]
                     , csUpdated  :: Int64
                     , csScanned  :: Int64
                     } deriving (Show, N.Generic)

data ConnectionStatus = Disabled | Waiting | Connecting | Connected deriving (Show, N.Generic)

data WLANNetwork = WLANNetwork { csSsid :: T.Text
                               , csBssids :: [BSSID]
                               } deriving (Show, N.Generic)

data BSSID = BSSID { csBssid :: T.Text
                   , csStrength :: T.Text
                   } deriving (Show, N.Generic)

instance A.ToJSON Status
instance A.ToJSON WLANNetwork
instance A.ToJSON BSSID
instance A.ToJSON ConnectionStatus

apsToNetworks :: [APInfo] -> [WLANNetwork]
apsToNetworks aps = snd $ helper aps []
  where
    {-@ helper :: aps:[APInfo] -> [WLANNetwork] -> ([APInfo], [WLANNetwork]) / [len aps] @-}
    helper :: [APInfo] -> [WLANNetwork] -> ([APInfo], [WLANNetwork])
    helper (a:as) ns = helper as ((newNs (ssid a) ns) ++ [WLANNetwork { csSsid = ssid a
                                                       , csBssids = [ BSSID { csBssid = bssid a
                                                                            , csStrength = strength a
                                                                            }
                                                                    ] ++ blist (ssid a) ns
                                                       }])
      where
        newNs ss = filter (\x -> csSsid x /= ss)
        blist s n = case find (\x -> csSsid x == s) n of
                       Just l -> csBssids l
                       Nothing -> []
    helper _ ns = ([],ns)

outputStatus :: World -> IO ()
outputStatus world = do
    time <- K.getTime K.Realtime
    aps <- atomRead $ wlanList world
    let networks = apsToNetworks aps
    ifList <- atomRead $ interfaceList world
    strengths <- atomRead $ wlanSignalStrengthLog world
    wwok <- atomRead $ wwanOK world
    wlok <- atomRead $ wlanOK world
    vok  <- atomRead $ vpnOK world
    lscan <- atomRead $ lastScan world
    using <- atomRead $ routeVia world
    let status = Status { csUsing    = using
                        , csWwan     = wwan world wwok
                        , csWlan     = wlan world wlok
                        , csWlanSSID = case wlanIf world of
                                           Just wlif -> B.fromMaybe "" $ currentSSID wlif ifList
                                           Nothing   -> ""
                        , csWlanBSSID = case wlanIf world of
                                            Just wlif -> B.fromMaybe "" $ currentBSSID wlif ifList
                                            Nothing   -> ""
                        , csWlanStrength = case lastMay strengths of
                                               Just s -> T.pack $ show s
                                               Nothing -> ""
                        , csVpn      = vpn world wlok vok
                        , csUpdated  = K.sec time
                        , csScanned  = lscan
                        , csNetworks = networks
                        }
    BS.writeFile "/var/winot/status" (A.encodePretty status)
    M.return ()
  where
    wwan w wwok = if wwanEnabled w then
                          if wwok then
                              Connected
                          else
                              Connecting
                      else
                          Disabled
    wlan w wlok = if wlanEnabled w then
                         if wlok then
                             Connected
                         else
                             Connecting
                      else
                         Disabled
    vpn w wlok vok = if vpnEnabled w then
                             if wlok then
                                 if vok then
                                     Connected
                                 else
                                     Connecting
                             else
                                 Waiting
                        else
                             Disabled
