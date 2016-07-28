{-# LANGUAGE DeriveGeneric #-}

module World where

import Protolude
import Prelude (($), readFile)
import qualified Control.Concurrent.STM as S
import qualified Control.Monad as M
import qualified Data.HashMap.Lazy as H
import qualified Data.Text as T
import qualified GHC.Int as G
import qualified Text.Toml as O
import qualified Text.Toml.Types as O
import qualified GHC.Generics as N
import qualified Data.Aeson as A

default (T.Text)

data World = World { config :: O.Table
                   , checkVPNLock :: S.TMVar ()
                   , checkWLANLock :: S.TMVar ()
                   , checkWWANLock :: S.TMVar ()
                   , checkRouteLock :: S.TMVar ()
                   , interfaceList :: S.TVar [IFInfo]
                   , interfaceListLock :: S.TMVar ()
                   , interfaceStats :: S.TVar T.Text
                   , interfaceStatsLock:: S.TMVar ()
                   , loopTimes :: [G.Int64]
                   , processList :: S.TVar T.Text
                   , processListLock :: S.TMVar ()
                   , routeList :: S.TVar T.Text
                   , routeListLock :: S.TMVar ()
                   , wlanSignalStrengthLog :: S.TVar [Int]
                   , wlanBandwidthLog :: S.TVar [Maybe Int]
                   , vpnBandwidthLog :: S.TVar [Maybe Int]
                   , wlanList :: S.TVar [APInfo]
                   , lastScan :: S.TVar G.Int64
                   , lastVPNConnect :: S.TVar G.Int64
                   , lastWLANConnect :: S.TVar G.Int64
                   , lastWWANConnect :: S.TVar G.Int64
                   , vpnIf :: Maybe T.Text
                   , vpnOK :: S.TVar Bool
                   , wlanIf :: Maybe T.Text
                   , wlanOK :: S.TVar Bool
                   , wwanIf :: Maybe T.Text
                   , wwanOK :: S.TVar Bool
                   , routeVia :: S.TVar ConnectionMedium
                   {-, loggerSet :: L.LoggerSet-}
                   }

instance Show World where
    show w = show [show (config w), show (loopTimes w)]

data IFInfo = IFInfo { name :: T.Text
                     , detail :: T.Text
                     } deriving (Show, Eq)

data APInfo = APInfo { ssid :: T.Text
                     , chan :: T.Text
                     , bssid :: T.Text
                     , strength :: T.Text
                     , speed :: T.Text
                     , options :: [T.Text]
                     , raw :: T.Text
                     } deriving (Show, Eq)

data ConnectionMedium = None | WWAN | WLAN | VPN deriving (Show, N.Generic)
instance A.ToJSON ConnectionMedium

initialWorld :: IO (Maybe World)
initialWorld = do
    rt <- S.atomically $ S.newTVar T.empty
    it <- S.atomically $ S.newTVar []
    pt <- S.atomically $ S.newTVar T.empty
    vk <- S.atomically $ S.newTVar False
    wlok <- S.atomically $ S.newTVar False
    wwok <- S.atomically $ S.newTVar False
    ls <- S.atomically $ S.newTVar 0
    lvc <- S.atomically $ S.newTVar 0
    lwwc <- S.atomically $ S.newTVar 0
    lwlc <- S.atomically $ S.newTVar 0
    bwl <- S.atomically $ S.newTVar []
    vbwl <- S.atomically $ S.newTVar []
    ssl <- S.atomically $ S.newTVar []
    wll <- S.atomically $ S.newTVar []
    l1 <- S.atomically S.newEmptyTMVar
    l2 <- S.atomically S.newEmptyTMVar
    l4 <- S.atomically S.newEmptyTMVar
    l5 <- S.atomically S.newEmptyTMVar
    l6 <- S.atomically S.newEmptyTMVar
    l7 <- S.atomically S.newEmptyTMVar
    l8 <- S.atomically S.newEmptyTMVar
    l10 <- S.atomically S.newEmptyTMVar
    rv <- S.atomically $ S.newTVar None
    {-lset <- L.newFileLoggerSet L.defaultBufSize "/var/log/winot2"-}
    is <- S.atomically $ S.newTVar T.empty

    con <- readFile "/etc/winot"
    let conf = O.parseTomlDoc "" $ T.pack con

    M.return $ case conf of
               Left _ -> Nothing
               Right c -> Just World { loopTimes = []
                                     , config            = c
                                     , checkVPNLock      = l1
                                     , checkWLANLock     = l10
                                     , checkWWANLock     = l2
                                     , checkRouteLock    = l7
                                     , processListLock   = l4
                                     , interfaceListLock = l5
                                     , interfaceStatsLock = l8
                                     , routeListLock     = l6
                                     , vpnIf = Nothing
                                     , vpnOK = vk
                                     , wlanIf = Nothing
                                     , wlanOK = wlok
                                     , wwanIf = Nothing
                                     , wwanOK = wwok
                                     , routeList = rt
                                     , interfaceList = it
                                     , processList = pt
                                     , wlanSignalStrengthLog = ssl
                                     , wlanBandwidthLog = bwl
                                     , vpnBandwidthLog = vbwl
                                     , wlanList = wll
                                     , lastScan = ls
                                     , lastVPNConnect = lvc
                                     , lastWWANConnect = lwwc
                                     , lastWLANConnect = lwlc
                                     , routeVia = rv
                                     , interfaceStats = is
                                     {-, loggerSet = lset-}
                                     }

configString :: T.Text -> World -> Maybe T.Text
configString setting world =
    case H.lookup setting cfg of
        Just node -> case node of
            O.NTValue n -> case n of
                O.VString s -> Just s
                _ -> Nothing
            _ -> Nothing
        Nothing -> Nothing
  where
    cfg = config world

