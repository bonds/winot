{-# OPTIONS_GHC -Wall -Werror  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Status.Interface where

import Data.List ((\\))
import Data.UnixTime (UnixTime, getUnixTime)
import Protolude
import Status.Config
import Status.Filter
import Status.Lease (Lease)
import Status.Route
import Util.Log
import Util.Orphan ()
import Util.Run
import qualified Control.Concurrent.STM as S
import qualified Control.Monad.Logger as ML
import qualified Data.Text as T
import qualified Text.Trifecta as Parse
import Net.Types hiding (getIPv4)
import qualified Net.IPv4.Text as IP
import qualified Net.IPv4.Range as IPR

default (Text, Integer, Double)

data Interface = Interface
    { ifDriver            :: IfDriver -- e.g. athn; aka athn0 without the 0
    , ifInstance          :: Integer        -- the 0 in athn0
    , ifStatus            :: IfStatus
    , ifRDomain           :: Maybe Integer
    , ifFilters           :: Maybe [Filter]
    , ifBandwidthHistory  :: Maybe [IfBandwidth] -- set to Nothing if we haven't collected any yet
    , ifWirelessDetail    :: Maybe IfWirelessDetail
    , ifIPv4Detail        :: Maybe IfIPv4Detail
    , ifLock              :: Maybe (S.TMVar ())
    } deriving (Show, Eq)

data IfDriver = IfDriver
    { ifdName             :: Text
    , ifdClass            :: IfClass
    } deriving (Show, Eq)

data IfClass = ICunknown | ICvirtual | ICmobile | ICwireless | ICusb | ICpci
    deriving (Show, Eq, Ord)

data IfStatus = IfStatus
    { ifUp                :: Bool -- ifconfig says its up
    , ifReady             :: Maybe (S.TVar Bool) -- ready for routing internet bound traffic
    , ifStatusDetail      :: Maybe Text -- ifconfig status line
    } deriving (Show, Eq)

data IfBandwidth = IfBandwidth
    { ifbWhen             :: UnixTime
    , ifbBytes            :: Integer
    } deriving (Show, Eq)

data IfStrength = IfStrength
    { ifsWhen             :: UnixTime
    , ifsStrength         :: Integer
    } deriving (Show, Eq)

data IfWirelessDetail = IfWirelessDetail
    { ifSSID              :: Maybe Text
    , ifBSSID             :: Maybe Text
    , ifStrengthHistory   :: [IfStrength]
    , ifNetworks          :: WirelessNetworks
    } deriving (Show, Eq)

data WirelessNetworks = WirelessNetworks
    { wnNetworks          :: [WirelessNetwork]
    , wnLastScan          :: Maybe UnixTime
    } deriving (Show, Eq)

data WirelessNetwork = WirelessNetwork
    { wnSSID              :: Maybe Text
    , wnPassword          :: Maybe Text
    , wnAPs               :: [WirelessAccessPoint]
    } deriving (Show, Eq)

data WirelessAccessPoint = WirelessAccessPoint
    { wnBSSID             :: Text
    , wnChan              :: Integer
    , wnStrength          :: Integer
    , wnSpeed             :: Text
    } deriving (Show, Eq)

data IfIPv4Detail = IfIPv4Detail
    { ifIPv4IP            :: IPv4
    , ifIPv4Netmask       :: IPv4Range
    , ifIPv4Broadcast     :: Maybe IPv4
    , ifIPv4Routes        :: Maybe [Route]
    , ifIPv4Lease         :: Maybe Lease
    } deriving (Show, Eq)

data InterfaceInfo =
      IIWD IfWirelessDetail
    | IIIPv4 IfIPv4Detail
    | IIStatus Text
    | IIIgnored
    deriving Show

-- TODO: prefer 5ghz networks over 2ghz as long as they are above a certain strength
preferredNetwork :: WirelessNetworks -> Maybe WirelessNetwork
preferredNetwork wns = lastMay $ sortOn strength $ wnNetworks $ familiarNetworks wns
  where
    strength :: WirelessNetwork -> Integer
    strength wn = maximum $ map wnStrength $ wnAPs wn

familiarNetworks :: WirelessNetworks -> WirelessNetworks
familiarNetworks wns = wns { wnNetworks = filter (isJust . wnPassword) (wnNetworks wns) }

defaultGateway :: Interface -> Maybe IPv4
defaultGateway i = head $ defaultGateways i

-- TODO: this totally ignores MAC address gateways, but that means if we have one
-- we won't delete/fix it
defaultGateways :: Interface -> [IPv4]
defaultGateways i = case ifIPv4Detail i of
    Just ipd -> case ifIPv4Routes ipd of
        Just rs -> mapMaybe (IP.decode . gateway) $ filter (\x -> destination x == "default") rs
        Nothing -> []
    Nothing -> []

interfaceSSID :: Interface -> Maybe Text
interfaceSSID i = case ifWirelessDetail i of
    Just wd -> ifSSID wd
    Nothing -> Nothing

interface :: Interface -> Text
interface a = ifdName (ifDriver a) <> show (ifInstance a)

connectableInterfaces :: [Interface] -> [Interface]
connectableInterfaces = filter (\x -> ICmobile <= ifdClass ( ifDriver x))

usbWiredInterfaces :: ML.LoggingT IO [Interface]
usbWiredInterfaces = do
    ifs <- interfaces
    wifs <- usbWiredIfDrivers
    return $ filter (\x -> T.init (interface x) `elem` wifs) ifs

pciWiredInterfaces :: ML.LoggingT IO [Interface]
pciWiredInterfaces = do
    ifs <- interfaces
    wired <- wiredIfDrivers
    usb <- usbWiredIfDrivers
    return $ filter (\x -> T.init (interface x) `elem` wired && T.init (interface x) `notElem` usb) ifs

wiredInterfaces :: ML.LoggingT IO [Interface]
wiredInterfaces = do
    ifs <- interfaces
    wifs <- wiredIfDrivers
    return $ filter (\x -> T.init (interface x) `elem` wifs) ifs

wirelessInterfaces :: ML.LoggingT IO [Interface]
wirelessInterfaces = do
    ifs <- interfaces
    wifs <- wirelessIfDrivers
    return $ filter (\x -> T.init (interface x) `elem` wifs) ifs

realInterfaces :: ML.LoggingT IO [Interface]
realInterfaces = do
    ifs <- interfaces
    vifs <- virtualIfDrivers
    return $ filter (\x -> T.init (interface x) `notElem` vifs) ifs

interfaces :: ML.LoggingT IO [Interface]
interfaces = do
    $(myLogTH) LLDevInfo [Reason LRStatus, Tag "youarehere"] Nothing
    procs <- getInterfaces
    time <- liftIO getUnixTime
    drivers' <- drivers
    case parseInterfaces procs time drivers' of
        Parse.Success result -> return $ sortOn (ifdClass . ifDriver) result
        Parse.Failure err -> do
            mapM_
                ($(myLogTH) LLUserTellDevAppIsBroken [Reason LRStatus, Tag "parseerror"] . Just)
                (T.lines $ show err)
            return []

getInterfaces :: ML.LoggingT IO Text
getInterfaces = runRead LRStatus "ifconfig"

parseInterfaces :: Text -> UnixTime -> [IfDriver] -> Parse.Result [Interface]
parseInterfaces t time ds = Parse.parseString (interfacesParser time ds) mempty (T.unpack t)

interfacesParser :: UnixTime -> [IfDriver] -> Parse.Parser [Interface]
interfacesParser time ds = Parse.some $ interfaceParser time ds

interfaceParser :: UnixTime -> [IfDriver] -> Parse.Parser Interface
interfaceParser time ds = do
    ifDriver' <- Parse.some Parse.letter
    ifInstance' <- Parse.integer
    _ <- Parse.text ": flags="
    Parse.whiteSpace
    flagPrefix <- Parse.integer
    _ <- Parse.char '<'
    someFlags <- Parse.commaSep $ Parse.some $ Parse.noneOf [',', '>']
    let flags' = show flagPrefix : someFlags
    _ <- Parse.char '>'
    Parse.whiteSpace
    rdomain' <- Parse.optional $ do
        _ <- Parse.text "rdomain"
        Parse.whiteSpace
        Parse.integer
    _ <- Parse.many $ Parse.notChar '\n'
    _ <- Parse.newline
    therest <- Parse.many
        $   Parse.try (wirelessParser time)
        <|> Parse.try ipv4Parser
        <|> Parse.try statusParser
        <|> unfamiliarLineParser
    return Interface
        { ifDriver            = IfDriver
            { ifdName         = T.pack ifDriver'
            , ifdClass        = getClass $ T.pack ifDriver'
            }
        , ifInstance          = ifInstance'
        , ifStatus            = IfStatus
            { ifUp            = "UP" `elem` flags'
            , ifStatusDetail  = getStatus therest
            , ifReady         = Nothing
            }
        , ifRDomain           = rdomain'
        , ifBandwidthHistory  = Nothing
        , ifWirelessDetail    = getWireless therest
        , ifLock              = Nothing
        , ifIPv4Detail        = getIPv4 therest
        , ifFilters           = Nothing
        }

  where

    getClass :: Text -> IfClass
    getClass d
        | d `elem` classDrivers ICpci      = ICpci
        | d `elem` classDrivers ICusb      = ICusb
        | d `elem` classDrivers ICwireless = ICwireless
        | d `elem` classDrivers ICvirtual  = ICvirtual
        | otherwise                   = ICunknown

    classDrivers :: IfClass -> [Text]
    classDrivers c = map ifdName (filter (\x -> c == ifdClass x) ds)

    getStatus :: [InterfaceInfo] -> Maybe Text
    getStatus iis = case headMay $ filter isStatus iis of
        Just (IIStatus s) -> Just s
        _                 -> Nothing

    getWireless :: [InterfaceInfo] -> Maybe IfWirelessDetail
    getWireless iis = case headMay $ filter isWireless iis of
        Just (IIWD w) -> Just w
        _             -> Nothing

    getIPv4 :: [InterfaceInfo] -> Maybe IfIPv4Detail
    getIPv4 iis = case headMay $ filter isIPv4 iis of
        Just (IIIPv4 w) -> Just w
        _               -> Nothing

    isStatus (IIStatus _) = True
    isStatus _            = False

    isWireless (IIWD _) = True
    isWireless _        = False

    isIPv4 (IIIPv4 _) = True
    isIPv4 _          = False

unfamiliarLineParser :: Parse.Parser InterfaceInfo
unfamiliarLineParser = do
    _ <- Parse.some Parse.space
    _ <- Parse.some $ Parse.notChar '\n'
    _ <- Parse.newline
    return IIIgnored

statusParser :: Parse.Parser InterfaceInfo
statusParser = do
    Parse.whiteSpace
    _ <- Parse.text "status:"
    Parse.whiteSpace
    status' <- Parse.some $ Parse.notChar '\n'
    _ <- Parse.newline
    return $ IIStatus $ T.pack status'

wirelessParser :: UnixTime -> Parse.Parser InterfaceInfo
wirelessParser time = do
    Parse.whiteSpace
    _ <- Parse.text "ieee80211:"
    Parse.whiteSpace
    wn <- Parse.optional $ wirelessNetworkParser []
    nwid <- Parse.optional nwidParser
    case wn of
        Just wn' ->
            return $ IIWD IfWirelessDetail
                { ifSSID         = wnSSID wn'
                , ifBSSID         = fmap wnBSSID $ head $ wnAPs wn'
                , ifStrengthHistory   =
                    case fmap wnStrength $ head $ wnAPs wn' of
                        Just s ->
                            [IfStrength
                                { ifsWhen         = time
                                , ifsStrength     = s
                                }]
                        Nothing -> []
                , ifNetworks          = emptyScan
                }
        Nothing ->
            return $ IIWD IfWirelessDetail
                { ifSSID         = nwid
                , ifBSSID         = Nothing
                , ifStrengthHistory   = []
                , ifNetworks          = emptyScan
                }
  where
    nwidParser = do
        _ <- Parse.text "nwid"
        Parse.whiteSpace
        networkID' <- Parse.stringLiteral <|> do
            a <- Parse.some $ Parse.noneOf [' ', '\n']
            return $ T.pack a
        _ <- Parse.newline
        return networkID'

ipv4Parser :: Parse.Parser InterfaceInfo
ipv4Parser = do
    Parse.whiteSpace
    _ <- Parse.text "inet"
    Parse.whiteSpace
    ip <- Parse.some $ Parse.notChar ' '
    Parse.whiteSpace
    _ <- Parse.text "netmask"
    Parse.whiteSpace
    netmask <- Parse.some $ Parse.notChar ' '
    broadcast <- Parse.optional $ Parse.try $ do
        Parse.whiteSpace
        _ <- Parse.text "broadcast"
        Parse.whiteSpace
        Parse.some $ Parse.noneOf [' ', '\n']
    _ <- Parse.newline
    case IP.decode (T.pack ip) :: Maybe IPv4 of
        Just ip' ->
            return $ IIIPv4 IfIPv4Detail
                { ifIPv4IP          = ip'
                , ifIPv4Netmask     = IPR.normalize $ IPv4Range ip' (netmaskToInt netmask)
                , ifIPv4Broadcast   = fmap T.pack broadcast >>= IP.decode
                , ifIPv4Routes      = Nothing
                , ifIPv4Lease       = Nothing
                }
        Nothing -> Parse.raiseErr $ Parse.failed "couldn't parse ip"

netmaskToInt :: [Char] -> Word8
netmaskToInt t
    | t == "0xff000000" = 8
    | t == "0xffff0000" = 16
    | t == "0xffffff00" = 24
    | t == "0xffffffff" = 32
    | otherwise = 32

-- instance Show Interface where
--     show i = "{name = " <> T.unpack (interface i) <> ", up = " <> show (up i) <> rd <> st <> wi <> "}"
--       where
--         rd = case rdomain i of
--             Just r -> ", rdomain = " <> show r
--             Nothing -> ""
--         st = case status i of
--             Just s -> T.unpack $ ", status = " <> s
--             Nothing -> ""
--         wi = case wireless i of
--             Just w -> ", wireless = " <> show w
--             Nothing -> ""
drivers :: ML.LoggingT IO [IfDriver]
drivers = do
    wired <- wiredIfDrivers
    usb <- usbWiredIfDrivers
    wireless <- wirelessIfDrivers
    virtual <- virtualIfDrivers
    let pci = wired \\ usb
    return $ dlist ICpci pci ++ dlist ICusb usb ++ dlist ICwireless wireless ++ dlist ICvirtual virtual
  where
    dlist ifd = map (\x -> IfDriver {ifdName = x, ifdClass = ifd})

virtualIfDrivers :: ML.LoggingT IO [Text]
virtualIfDrivers = do
    $(myLogTH) LLDevInfo [Reason LRStatus, Tag "youarehere"] Nothing
    cs <- getVirtualIfDrivers
    case parseVirtualIfDrivers cs of
        Parse.Success result -> return result
        Parse.Failure err -> do
            mapM_
                ($(myLogTH) LLUserTellDevAppIsBroken [Reason LRStatus, Tag "parseerror"] . Just)
                (T.lines $ show err)
            return []

parseVirtualIfDrivers :: Text -> Parse.Result [Text]
parseVirtualIfDrivers t = Parse.parseString virtualIfDriversParser mempty (T.unpack t)

virtualIfDriversParser :: Parse.Parser [Text]
virtualIfDriversParser = do
    vis <- Parse.some (Parse.noneOf [' ', '\n']) `Parse.sepBy` Parse.symbol " "
    return $ map T.pack vis

getVirtualIfDrivers :: ML.LoggingT IO Text
getVirtualIfDrivers = runRead LRConst "ifconfig -C"

pciWiredIfDrivers :: ML.LoggingT IO [Text]
pciWiredIfDrivers = do
    wired <- wiredIfDrivers
    usb <- usbWiredIfDrivers
    return $ wired \\ usb

wiredIfDrivers :: ML.LoggingT IO [Text]
wiredIfDrivers = do
    $(myLogTH) LLDevInfo [Reason LRStatus, Tag "youarehere"] Nothing
    cs <- getWiredIfDrivers
    case parseWiredIfDrivers cs of
        Parse.Success result -> do
            vs <- virtualIfDrivers
            return $ filter (`notElem` vs) result
        Parse.Failure err -> do
            mapM_
                ($(myLogTH) LLUserTellDevAppIsBroken [Reason LRStatus, Tag "parseerror"] . Just)
                (T.lines $ show err)
            return []

parseWiredIfDrivers :: Text -> Parse.Result [Text]
parseWiredIfDrivers t = Parse.parseString wiredIfDriversParser mempty (T.unpack t)

wiredIfDriversParser :: Parse.Parser [Text]
wiredIfDriversParser = do
    ifs <- Parse.some $ do
        ifx <- Parse.some (Parse.noneOf [' ', '\n', '(', ')'])
        _ <- Parse.some (Parse.noneOf ['\n'])
        _ <- Parse.newline
        return ifx
    return $ map T.pack ifs

usbWiredIfDrivers :: ML.LoggingT IO [Text]
usbWiredIfDrivers = do
    $(myLogTH) LLDevInfo [Reason LRStatus, Tag "youarehere"] Nothing
    cs <- getWiredIfDrivers
    let cs' = T.unlines $ filter (T.isInfixOf "USB") $ T.lines cs
    case parseWiredIfDrivers cs' of
        Parse.Success result -> do
            vs <- virtualIfDrivers
            return $ filter (`notElem` vs) result
        Parse.Failure err -> do
            mapM_
                ($(myLogTH) LLUserTellDevAppIsBroken [Reason LRStatus, Tag "parseerror"] . Just)
                (T.lines $ show err)
            return []

getWiredIfDrivers :: ML.LoggingT IO Text
getWiredIfDrivers = runRead LRConst "apropos -s 4 \"Ethernet\""

wirelessIfDrivers :: ML.LoggingT IO [Text]
wirelessIfDrivers = do
    $(myLogTH) LLDevInfo [Reason LRStatus, Tag "youarehere"] Nothing
    cs <- getWirelessIfDrivers
    case parseWiredIfDrivers cs of
        Parse.Success result -> do
            vs <- virtualIfDrivers
            return $ filter (`notElem` vs) result
        Parse.Failure err -> do
            mapM_
                ($(myLogTH) LLUserTellDevAppIsBroken [Reason LRStatus, Tag "parseerror"] . Just)
                (T.lines $ show err)
            return []

getWirelessIfDrivers :: ML.LoggingT IO Text
getWirelessIfDrivers = runRead LRConst "apropos -s 4 \"wireless network device\""

data IfStats = IfStats
    { iftInterface      :: Text
    , iftWhen           :: UnixTime
    , iftInputBytes     :: Integer
    , iftOutputBytes    :: Integer
    }
    deriving Show

interfaceStats :: ML.LoggingT IO [IfStats]
interfaceStats = do
    $(myLogTH) LLDevInfo [Reason LRStatus, Tag "youarehere"] Nothing
    ifs <- getInterfaceStats
    time <- liftIO getUnixTime
    let ifs' = T.unlines $ tailSafe . tailSafe . tailSafe . tailSafe . initSafe $ T.lines ifs
    case parseInterfaceStats time ifs' of
        Parse.Success result -> do
            vs <- virtualIfDrivers
            return $ filter (\x -> T.init (iftInterface x) `notElem` vs) result
        Parse.Failure err -> do
            mapM_
                ($(myLogTH) LLUserTellDevAppIsBroken [Reason LRStatus, Tag "parseerror"] . Just)
                (T.lines $ show err)
            return []

parseInterfaceStats :: UnixTime -> Text -> Parse.Result [IfStats]
parseInterfaceStats time t = Parse.parseString (interfaceStatsParser time) mempty (T.unpack t)

interfaceStatsParser :: UnixTime -> Parse.Parser [IfStats]
interfaceStatsParser time = Parse.some $ interfaceStatsLineParser time

interfaceStatsLineParser :: UnixTime -> Parse.Parser IfStats
interfaceStatsLineParser time = do
    interface' <- Parse.some $ Parse.notChar ' '
    Parse.whiteSpace
    _ <- Parse.some $ Parse.notChar ' '
    Parse.whiteSpace
    _ <- Parse.integer
    Parse.whiteSpace
    inputBytes' <- Parse.integer
    Parse.whiteSpace
    _ <- Parse.integer
    Parse.whiteSpace
    _ <- Parse.integer
    Parse.whiteSpace
    outputBytes' <- Parse.integer
    _ <- Parse.some $ Parse.notChar '\n'
    _ <- Parse.newline
    return IfStats { iftInterface    = T.pack interface'
              , iftWhen         = time
              , iftInputBytes   = inputBytes'
              , iftOutputBytes  = outputBytes'
              }

getInterfaceStats :: ML.LoggingT IO Text
getInterfaceStats = runRead LRStatus $ "systat -w 100 -B ifstat " <> sampleSizeInSeconds
  where
    sampleSizeInSeconds = "1"

wirelessNetworks :: ML.LoggingT IO WirelessNetworks
wirelessNetworks = do
    $(myLogTH) LLDevInfo [Reason LRStatus, Tag "youarehere"] Nothing
    wifs <- wirelessInterfaces
    case head wifs of
        Just wif -> do
            ws <- getWirelessNetworks wif
            time <- liftIO getUnixTime
            passwords <- wirelessPasswords
            case parseWirelessScan time passwords ws of
                Parse.Success result -> return result
                Parse.Failure err -> do
                    mapM_
                        ($(myLogTH) LLUserTellDevAppIsBroken [Reason LRStatus, Tag "parseerror"] . Just)
                        (T.lines $ show err)
                    return emptyScan
        _ -> return emptyScan

emptyScan :: WirelessNetworks
emptyScan = WirelessNetworks
    { wnNetworks          = []
    , wnLastScan          = Nothing
    }

parseWirelessScan :: UnixTime -> [WirelessPassword] -> Text -> Parse.Result WirelessNetworks
parseWirelessScan time ps t = Parse.parseString (wirelessScanParser time ps) mempty (T.unpack t)

wirelessNetworkParser :: [WirelessPassword] -> Parse.Parser WirelessNetwork
wirelessNetworkParser ps = do
    _ <- Parse.text "nwid"
    Parse.whiteSpace
    networkID' <- Parse.stringLiteral <|> do
        a <- Parse.some $ Parse.noneOf [' ', '\n']
        return $ T.pack a
    Parse.whiteSpace
    _ <- Parse.text "chan"
    Parse.whiteSpace
    chan <- Parse.integer
    Parse.whiteSpace
    _ <- Parse.text "bssid"
    Parse.whiteSpace
    bssid <- Parse.some $ Parse.notChar ' '
    Parse.whiteSpace
    _ <- Parse.char '-'
    strength <- Parse.integer
    _ <- Parse.text "dBm"
    Parse.whiteSpace
    speed <- Parse.some $ Parse.notChar ' '
    Parse.whiteSpace
    _ <- Parse.many $ Parse.notChar '\n'
    _ <- Parse.newline
    return WirelessNetwork
        { wnSSID            = ssid networkID'
        , wnPassword        = findPassword networkID'
        , wnAPs             = [WirelessAccessPoint
            { wnBSSID       = T.pack bssid
            , wnChan        = chan
            , wnStrength    = strength
            , wnSpeed       = T.pack speed
            }]
        }
  where
    ssid n = if n == "" then Nothing else Just n
    findPassword n =
        case find (\x -> ssid n == Just (wpNetwork x)) ps of
            Just wp -> case wpPassword wp of
                Just wp' -> Just wp'
                Nothing  -> Just "" -- this is an open network, not a missing password
            Nothing -> Nothing

wirelessScanParser :: UnixTime -> [WirelessPassword] -> Parse.Parser WirelessNetworks
wirelessScanParser time ps = do
    _ <- Parse.some $
        Parse.try $ do
            Parse.try parseIEEE <|> parseMisc
            Parse.newline
    networks <- Parse.some $ Parse.try $ do
        Parse.whiteSpace
        wirelessNetworkParser ps
    return WirelessNetworks
        { wnNetworks          = fst $ consolidate [] networks
        , wnLastScan          = Just time
        }
  where
    parseIEEE = do
        Parse.whiteSpace
        _ <- Parse.text "ieee80211:"
        _ <- Parse.some $ Parse.notChar '\n'
        return ()
    parseMisc = do
        _ <- Parse.some $ Parse.notChar '\n' <* Parse.notFollowedBy (Parse.text "nwid")
        return ()
    consolidate :: [WirelessNetwork] -> [WirelessNetwork] -> ([WirelessNetwork], [WirelessNetwork])
    consolidate done [] = (done, [])
    consolidate done (x:xs) =
        case find (\a -> wnSSID x == wnSSID a) done of
            Just wn -> consolidate ((done \\ [wn]) ++ [wn { wnAPs = wnAPs wn ++ wnAPs x}]) xs
            Nothing -> consolidate (done ++ [x]) xs

getWirelessNetworks :: Interface -> ML.LoggingT IO Text
getWirelessNetworks i = runRead LRStatus $ "ifconfig " <> interface i <> " scan"

