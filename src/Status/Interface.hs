{-# OPTIONS_GHC -Wall -Werror  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Status.Interface where

import Protolude
import Util.Run
import Util.Log
-- import Util.Misc
import qualified Text.Trifecta as Parse
import qualified Data.Text as T
import qualified Control.Monad.Logger as ML
-- import qualified GHC.Int as G
-- import qualified Control.Concurrent.STM as S
import Data.UnixTime (UnixTime, getUnixTime)
-- import qualified GHC.Show
import Data.List ((\\))

default (Text, Integer, Double)

data Interface = Interface
    { ifDriver            :: IfDriver -- e.g. athn; aka athn0 without the 0
    , ifInstance          :: Integer        -- the 0 in athn0
    , ifStatus            :: IfStatus
    , ifRDomain           :: Maybe Integer
    , ifBandwidthHistory  :: [IfBandwidth]
    , ifWirelessDetail    :: Maybe IfWirelessDetail
    } deriving Show

data IfDriver = IfDriver
    { ifdName             :: Text
    , ifdClass            :: IfClass
    } deriving Show

data IfClass = ICunknown | ICvirtual | ICmobile | ICwireless | ICusb | ICpci
    deriving (Show, Eq, Ord)

data IfStatus = IfStatus
    { ifUp                :: Bool -- ifconfig says its up
    , ifReady             :: Bool -- ready for routing internet bound traffic
    , ifStatusDetail      :: Maybe Text -- ifconfig status line
    } deriving Show

data IfBandwidth = IfBandwidth
    { ifbWhen             :: UnixTime
    , ifbBytes            :: Integer
    } deriving Show

data IfStrength = IfStrength
    { ifsWhen             :: UnixTime
    , ifsStrength         :: Integer
    } deriving Show

data IfWirelessDetail = IfWirelessDetail
    { ifNetworkID         :: Maybe Text
    , ifStationID         :: Maybe Text
    , ifStrengthHistory   :: [IfStrength]
    , ifNetworks          :: WirelessNetworks
    } deriving Show

data WirelessNetworks = WirelessNetworks
    { wnNetworks          :: [WirelessNetwork]
    , wnLastScan          :: Maybe UnixTime
    } deriving Show

data WirelessNetwork = WirelessNetwork
    { wnSSID              :: Text
    , wnAPs               :: [WirelessAccessPoint]
    } deriving Show

data WirelessAccessPoint = WirelessAccessPoint
    { wnBSSID             :: Text
    , wnChan              :: Integer
    , wnStrength          :: Integer
    , wnSpeed             :: Text
    } deriving Show

data InterfaceInfo = WirelessInfo IfWirelessDetail | Status Text | Ignored

interface :: Interface -> Text
interface a = ifdName (ifDriver a) <> show (ifInstance a)

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
    therest <- Parse.many $ Parse.try statusParser <|> Parse.try (wirelessParser time) <|> unfamiliarLineParser
    return Interface
        { ifDriver            = IfDriver
            { ifdName         = T.pack ifDriver'
            , ifdClass        = getClass $ T.pack ifDriver'
            }
        , ifInstance          = ifInstance'
        , ifStatus            = IfStatus
            { ifUp            = "UP" `elem` flags'
            , ifStatusDetail  = getStatus therest
            , ifReady         = False
            }
        , ifRDomain           = rdomain'
        , ifBandwidthHistory  = []
        , ifWirelessDetail    = getWireless therest
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
        Just (Status s) -> Just s
        _               -> Nothing

    getWireless :: [InterfaceInfo] -> Maybe IfWirelessDetail
    getWireless iis = case headMay $ filter isWireless iis of
        Just (WirelessInfo w) -> Just w
        _                     -> Nothing

    isStatus (Status _) = True
    isStatus _          = False

    isWireless (WirelessInfo _) = True
    isWireless _                = False

unfamiliarLineParser :: Parse.Parser InterfaceInfo
unfamiliarLineParser = do
    _ <- Parse.some Parse.space
    _ <- Parse.some $ Parse.notChar '\n'
    _ <- Parse.newline
    return Ignored

statusParser :: Parse.Parser InterfaceInfo
statusParser = do
    Parse.whiteSpace
    _ <- Parse.text "status:"
    Parse.whiteSpace
    status' <- Parse.some $ Parse.notChar '\n'
    _ <- Parse.newline
    return $ Status $ T.pack status'

wirelessParser :: UnixTime -> Parse.Parser InterfaceInfo
wirelessParser time = do
    Parse.whiteSpace
    _ <- Parse.text "ieee80211:"
    Parse.whiteSpace
    _ <- Parse.text "nwid"
    Parse.whiteSpace
    networkID' <- Parse.stringLiteral <|> do
        a <- Parse.some $ Parse.noneOf [' ', '\n']
        return $ T.pack a
    Parse.whiteSpace
    _ <- Parse.optional $ do
        _ <- Parse.text "chan"
        Parse.whiteSpace
        _ <- Parse.integer
        Parse.whiteSpace
    stationIDAndStrength <- Parse.optional $ do
        _ <- Parse.text "bssid"
        Parse.whiteSpace
        bssid <- Parse.some $ Parse.notChar ' '
        Parse.whiteSpace
        _ <- Parse.char '-'
        strength <- Parse.integer
        _ <- Parse.text "dBm"
        return (T.pack bssid, strength)
    _ <- Parse.many $ Parse.notChar '\n'
    _ <- Parse.newline
    return $ WirelessInfo IfWirelessDetail
        { ifNetworkID         =
            case networkID' of
                "" -> Nothing
                a  -> Just a
        , ifStationID         = fmap fst stationIDAndStrength
        , ifStrengthHistory   =
            case fmap snd stationIDAndStrength of
                Just s ->
                    [IfStrength
                        { ifsWhen         = time
                        , ifsStrength     = s
                        }]
                Nothing -> []
        , ifNetworks          = WirelessNetworks
            { wnNetworks      = []
            , wnLastScan      = Nothing
            }
        }

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

-- updateInterfaceStats :: World -> IO ()
-- updateInterfaceStats world = do
--     stats <- getInterfaceStats
--     atomWrite (interfaceStats world) stats

