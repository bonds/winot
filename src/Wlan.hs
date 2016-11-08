{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Wlan where

import Data.HashMap.Lazy as H ((!), HashMap)
import Protolude
import Util
import World
import qualified Control.Concurrent.Thread.Delay as D
import qualified Control.Monad as M
import qualified Data.List as DL
import qualified Data.Maybe as B
import qualified Data.Text as T
import qualified Data.Text.ICU as U
import qualified Safe as F
import qualified System.Clock as K
import qualified System.Directory as SD
import qualified System.IO as IO
import qualified System.Log.Logger as L
import qualified Text.Toml.Types as O
import qualified Text.Trifecta as Tr

default (T.Text, Integer, Double)

checkWLAN :: World -> IO ()
checkWLAN world = do
    let logPrefix = "winot.checkWLAN"

    if F.readDef True
            (T.unpack
                (B.fromMaybe "True"
                    (configString "wlan_enabled" world))) then do
        wif <- atomRead (wlanIf world)
        if B.isJust wif then do
            checkWLANScanRequest world
            ifList <- atomRead $ interfaceList world
            if wlanConnOK (B.fromJust wif) ifList (familiarSSIDs world) :: Bool then
                if wlanIPOK (B.fromJust wif) ifList then do
                    wsok <- wlanSignalOK world
                    if wsok then do
                        wlg <- wlanGateway wif
                        pok <- maybe (M.return False) (ping 3) wlg
                        if pok then do
                            oldOK <- atomRead (wlanOK world)
                            M.unless oldOK $ do
                                L.infoM logPrefix "connected to wlan"
                                atomWrite (wlanOK world) True
                        else do
                            L.debugM logPrefix "wlan choice: badping"
                            go connectWLANConn world
                    else do
                        L.infoM logPrefix "looking for a closer wlan access point"
                        L.debugM logPrefix "wlan choice: weaksignal"
                        go wlanScan world
                else do
                    L.debugM logPrefix "wlan choice: noip"
                    atomWrite (wlanOK world) False
                    dhclient (B.fromJust wif)
            else do
                L.debugM logPrefix "wlan choice: noconnection"
                go connectWLANConn world
        else do
            L.debugM logPrefix "wlan choice: nointerface"
            atomWrite (wlanOK world) False
            setupWLANIf world
            go connectWLANConn world
    else do
        L.debugM logPrefix "wlan choice: disabled"
        atomWrite (wlanOK world) False

    status <- atomRead (wlanOK world)
    L.debugM logPrefix $ T.unpack $ "wlanOK " `T.append` T.pack (show status)

  where
    go todo w = do
        atomWrite (wlanOK w) False
        _ <- todo w
        M.return ()

dhclient :: T.Text -> IO ()
dhclient interface = do
    let logPrefix = "winot.dhclient"
    L.debugM logPrefix $ T.unpack $ T.concat ["getting an IP via DHCP for ", interface]
    run $ T.concat ["dhclient -i routers ", interface]
    M.return ()

connectWLANConn :: World -> IO ()
connectWLANConn world = do
    let logPrefix = "winot.connectWLANConn"
    let waitXSecondsBeforeDone  = 1
    let waitXSecondsBeforeRetry = 30

    startTime <- K.getTime K.Realtime
    lastConnectAttempt <- atomRead (lastWLANConnect world)

    if (K.sec startTime - lastConnectAttempt) > waitXSecondsBeforeRetry then do
        L.debugM logPrefix "starting"
        wif <- atomRead $ wlanIf world
        if B.isJust wif then do
            wlanScan world
            aps <- atomRead $ wlanList world
            M.when (null aps) $ L.debugM logPrefix "no networks found"
            let fs = familiarSSIDs world
            let familiarAndInRange = filter (\x -> ssid x `elem` fs) aps
            if not (null familiarAndInRange) then do
                let s = B.maybe "" ssid $ lastMay $ DL.sortOn strength familiarAndInRange
                let a = B.fromJust $ lookupSSID s world
                let nwid = case a ! T.pack "ssid" of
                               O.NTValue n -> case n of
                                   O.VString vs -> Just vs
                                   _          -> Nothing
                               _         -> Nothing
                let password = case a ! T.pack "password" of
                                   O.NTValue n -> case n of
                                       O.VString vs2 -> Just vs2
                                       _           -> Nothing
                                   _         -> Nothing
                if B.isJust nwid && B.isJust password then do
                    L.infoM logPrefix $ T.unpack $ T.concat ["connecting to ", B.fromJust nwid]
                    connectTime <- K.getTime K.Realtime
                    atomWrite (lastWLANConnect world) (K.sec connectTime)
                    run $ T.concat [ "ifconfig "
                                   , B.fromJust wif
                                   , " nwid \""
                                   , B.fromJust nwid
                                   , "\" wpakey "
                                   , B.fromJust password
                                   , " up"
                                   ]
                    dhclient $ B.fromJust wif
                    D.delay $ waitXSecondsBeforeDone * (10::Integer)^(6::Integer)
                    M.return ()
                else L.debugM logPrefix "missing password"
            else L.debugM logPrefix "no familiar networks found"
        else L.debugM logPrefix "no wlan interface, so why is this being called?"
    else L.debugM logPrefix "skipping because last attempt was too recent"

wlanSignalOK :: World -> IO Bool
wlanSignalOK world = do
    ssls <- secondsSinceLastScan world
    wsw  <- wlanSignalWeak world
    bl <- atomRead $ wlanBandwidthLog world
    wi   <- idle world bl
    M.return $ not $ ssls > bscans && wsw && B.fromMaybe False wi
  where
    bscans = F.readDef 60 $ T.unpack $ B.fromMaybe "60" (configString "MinimumSecondsBetweenScans" world)

wlanSignalWeak :: World -> IO Bool
wlanSignalWeak world = do
    l <- atomRead $ wlanSignalStrengthLog world
    M.return $ length l >= intervals && case F.maximumMay (lastN intervals l) of
        Just m -> m < wsmeans
        Nothing -> False
  where
    intervals = B.maybe 30 (B.fromMaybe 30 . readMaybe . T.unpack) $ configString "WeakSignalIntervalsBeforeWeak" world
    wsmeans = B.maybe 20 (B.fromMaybe 20 . readMaybe . T.unpack) $
        configString "WeakSignalMeansLessThan" world

-- it appears that scanning leads OpenBSD to switch to the higher powered
-- BSSID if one is available with the same SSID, but the scanning process
-- (sometimes) interrupts and then renegotiates the current connection, regardless
-- whether a new BSSID with a stronger signal was found or whether we kept
-- the same BSSID, so only scan when the signal is consistently weak and the
-- connection is relatively idle
wlanScan :: World -> IO ()
wlanScan world = do
    wif <- atomRead $ wlanIf world
    M.when (B.isJust wif) $ do
        currentTime <- K.getTime K.Realtime
        atomWrite (lastScan world) (K.sec currentTime)
        out <- runRead $ "ifconfig " `T.append` B.fromJust wif `T.append` " scan"
        aps <- lst out
        mapM_
            (L.debugM logPrefix . T.unpack . T.append "scan raw: ")
            (T.lines out)
        L.debugM logPrefix . T.unpack . T.append "scan aps: " $ T.pack (show aps)
        atomWrite (wlanList world) aps
        M.return ()
  where
    logPrefix = "winot.wlanScan"
    -- lst r = B.mapMaybe apLineToInfo (T.lines r)
    lst r = do
        results <- mapM (\line ->
                    case apLineToInfo' line of
                        Tr.Success a -> return $ Just a
                        Tr.Failure e -> do
                            L.debugM logPrefix $ show e
                            return Nothing
                )
                (T.lines r)
        return $ B.catMaybes results

--                nwid blahdeblah chan 1 bssid ab:cd:ef:93:20:ea -80dBm HT-MCS23 short_preamble,short_slottime
--                nwid "Network With Space" chan 48 bssid ab:cd:ef:24:1c:ca -79dBm HT-MCS15 privacy,short_slottime,wpa2
apLineToInfo :: T.Text -> Maybe APInfo
apLineToInfo line =
    -- OpenBSD 5.9 format: case U.find (U.regex [U.Multiline] "^[\t ]*nwid (.*) chan (.*) bssid (.*) (.*)% (.*)M (.*)") line of
    -- OpenBSD 6.0 format
    case U.find (U.regex [U.Multiline] "^[\t ]*nwid (.*) chan (.*) bssid (.*) -(.*)dBm (.*) (.*)") line of
        Nothing -> Nothing
        Just m ->
            Just APInfo { ssid       = T.replace "\"" "" $ B.fromJust $ U.group 1 m
                        , chan       = B.fromJust $ U.group 2 m
                        , bssid      = B.fromJust $ U.group 3 m
                        , strength   = B.fromJust $ U.group 4 m
                        , speed      = B.fromJust $ U.group 5 m
                        , options    = T.splitOn "," $ B.fromJust $ U.group 6 m
                        , raw = line
                }

apLineToInfo' :: T.Text -> Tr.Result APInfo
apLineToInfo' line =
    -- OpenBSD 5.9 format: case U.find (U.regex [U.Multiline] "^[\t ]*nwid (.*) chan (.*) bssid (.*) (.*)% (.*)M (.*)") line of
    -- OpenBSD 6.0 format
    -- case U.find (U.regex [U.Multiline] "^[\t ]*nwid (.*) chan (.*) bssid (.*) -(.*)dBm (.*) (.*)") line of
    Tr.parseString (do
        _ <- Tr.whiteSpace
        n  <- nwidP
        c  <- chanP
        b  <- bssidP
        st <- strP
        sp <- valueThenSpace
        o <-  optP

        return APInfo  { ssid       = n
                  , chan       = c
                  , bssid      = b
                  , strength   = st
                  , speed      = sp
                  , options    = o
                  , raw        = line
                  }
        )
        mempty $ T.unpack line

  where
    nwidP :: Tr.Parser T.Text
    nwidP = do
        _ <- Tr.text "nwid"
        _ <- Tr.space
        result <-
            Tr.stringLiteral
            <|>
            (do
              i <- Tr.some (Tr.notChar ' ')
              _ <- Tr.space
              return i
            )
        return $ T.pack result

    chanP :: Tr.Parser T.Text
    chanP = do
        _ <- Tr.text "chan"
        _ <- Tr.space
        result <- Tr.some Tr.digit
        _ <- Tr.space
        return $ T.pack result

    bssidP :: Tr.Parser T.Text
    bssidP = do
        _ <- Tr.text "bssid"
        _ <- Tr.space
        valueThenSpace

    strP :: Tr.Parser T.Text
    strP = do
        result <- Tr.integer
        _ <- Tr.text "dBm"
        _ <- Tr.space
        return $ show result

    valueThenSpace :: Tr.Parser T.Text
    valueThenSpace = do
        result <- Tr.some (Tr.notChar ' ')
        _ <- Tr.space
        return $ T.pack result

    optP :: Tr.Parser [T.Text]
    optP = do
        result <- Tr.commaSep $ Tr.some Tr.characterChar
        return $ map T.pack result

secondsSinceLastScan :: World -> IO Integer
secondsSinceLastScan world = do
    ct <- K.getTime K.Realtime
    ls <- atomRead $ lastScan world
    M.return $ fromIntegral $ K.sec ct - ls

wlanConnOK :: T.Text -> [IFInfo] -> [T.Text] -> Bool
wlanConnOK wlif infos ssids = ("status: active" `T.isInfixOf` detailOrEmpty (DL.find (\i -> name i == wlif) infos))
                              && B.isJust csid && B.fromJust csid `DL.elem` ssids
  where
    csid = currentSSID wlif infos

wlanIPOK :: T.Text -> [IFInfo] -> Bool
wlanIPOK wlif infos = "inet" `T.isInfixOf` detailOrEmpty (DL.find (\i -> name i == wlif) infos)

wlanIfconfig :: T.Text -> [IFInfo] -> T.Text
wlanIfconfig wlif infos = detailOrEmpty (DL.find (\i -> name i == wlif) infos)

currentSSID :: T.Text -> [IFInfo] -> Maybe T.Text
currentSSID wlif infos = case U.find (U.regex [U.Multiline] "nwid \"?([^\"]*)\"? chan") (wlanIfconfig wlif infos) of
                           Just m -> U.group 1 m
                           _      -> Nothing

currentBSSID :: T.Text -> [IFInfo] -> Maybe T.Text
currentBSSID wlif infos = case U.find (U.regex [U.Multiline] "bssid (.*) [0-9]{1,2}%") (wlanIfconfig wlif infos) of
                            Just m -> U.group 1 m
                            _      -> Nothing

setupWLANIf :: World -> IO ()
setupWLANIf world = do
    let logPrefix = "winot.setupWLANIf"
    L.debugM logPrefix "start"
    ifs <- atomRead (interfaceList world)
    let configWif = configString "wlan_if" world
    let wif = if B.isJust configWif then configWif
              else case [ name i | i <- ifs, isWLAN i ] of
                     (w:_)     -> Just w
                     _         -> Nothing
    L.debugM logPrefix $ T.unpack $ "chose " `T.append` B.fromMaybe "none" wif
    atomWrite (wlanIf world) wif

isWLAN :: IFInfo -> Bool
isWLAN i = B.isJust (U.find (U.regex [U.Multiline] "groups:.*wlan") (detail i))

recordWLANSignalStrength :: World -> IO ()
recordWLANSignalStrength world = do
    let logPrefix = "winot.recordWLANSignalStrength"
    wif <- atomRead $ wlanIf world
    M.when (B.isJust wif) $ do
        l <- atomRead $ wlanSignalStrengthLog world
        infos <- atomRead $ interfaceList world
        M.when (B.isJust (newItem wif infos)) $ do
            let !values = lastN (itemsToKeep-1) l <> [B.fromJust (newItem wif infos)]
            atomWrite
                (wlanSignalStrengthLog world)
                values
    log2 <- atomRead $ wlanSignalStrengthLog world
    L.debugM logPrefix $ T.unpack $ "wlansig: " `T.append` T.pack (show (lastN 5 log2))
  where
    itemsToKeep = 100
    d wif infos = detailOrEmpty (DL.find (\i -> name i == B.fromJust wif) infos)
    newItem wif infos = case U.find (U.regex [U.Multiline] "bssid.* (.*)%") (d wif infos) of
        Just m -> readMaybe $ T.unpack $ B.fromJust $ U.group 1 m :: Maybe Int
        Nothing -> Nothing

wlanNetworks :: World -> [O.Table]
wlanNetworks world = case config world ! T.pack "networks" of
                       O.NTArray a -> a
                       _ -> []

lookupSSID :: T.Text -> World -> Maybe (H.HashMap T.Text O.Node)
lookupSSID s world =
    case matches of
      (x:_)     -> Just x
      _         -> Nothing
  where
    matches = filter
        (\n -> case n ! T.pack "ssid" of
            O.NTValue a -> case a of
                O.VString b -> b == s
                _ -> False
            _ -> False)
        (wlanNetworks world)

wlanGateway :: Maybe T.Text -> IO (Maybe T.Text)
wlanGateway wif = do
    let logPrefix = "winot.wlanGateway"
    if B.isJust wif then do
        L.debugM logPrefix "start"
        lease <- IO.readFile $ T.unpack ("/var/db/dhclient.leases." `T.append` B.fromJust wif)
        let matches = case U.findAll (U.regex [U.Multiline] "routers (.*);") (T.pack lease) of
                          ms@(_:_) -> case lastMay ms of
                              Just m -> U.group 1 m
                              Nothing -> Nothing
                          _ -> Nothing
        L.debugM logPrefix $ T.unpack $ "wlanGateway matches: " `T.append` T.pack (show matches)
        M.return matches
    else M.return Nothing

familiarSSIDs :: World -> [T.Text]
familiarSSIDs world =
    B.mapMaybe (\x -> case x ! "ssid" of
        O.NTValue a -> case a of
            O.VString b -> Just b
            _ -> Nothing
        _ -> Nothing)
        (wlanNetworks world)

scanRequested :: World -> IO Bool
scanRequested world = do
    ssls <- secondsSinceLastScan world
    dfe <- SD.doesFileExist "/tmp/winot-scan"
    let bscans = F.readDef 60 $ T.unpack $ B.fromMaybe "60" (configString "MinimumSecondsBetweenScans" world)
    M.return $ ssls > bscans && dfe

checkWLANScanRequest :: World -> IO ()
checkWLANScanRequest world = do
    sr <- scanRequested world
    M.when sr $ wlanScan world
    _ <- try (SD.removeFile "/tmp/winot-scan") :: IO (Either IOException ())
    M.return ()
