{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Loop where

import Protolude
import Control.Monad ((>>))
import Route
import Status
import Util
import Vpn
import Wlan
import World
import Wwan
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Thread.Delay as D
import qualified Control.Monad as M
import qualified Control.Monad.Loops as M
import qualified Data.Maybe as B
import qualified Data.Text as T
import qualified Foreign.C.Types as F
import qualified System.Clock as K
import qualified System.Exit as E
import qualified System.Log.Formatter as LF
import qualified System.Log.Handler as LH
import qualified System.Log.Handler.Simple as LH
import qualified System.Log.Logger as L
import qualified System.Posix.Files as P
import qualified System.Posix.Process as P
import qualified System.Posix.Signals as P
import qualified System.Directory as SD
import qualified Text.Toml as O

default (Text, Integer, Double)

loop :: IO ()
loop = do
    let logPrefix = "winot.main"

    P.nice 20 -- run at low priority
    uid <- runRead "id -u"
    M.when (T.strip uid /= "0") $ do
        print ("error: this script must be run by root" :: Text)
        E.exitFailure

    -- load configuration and initial setup

    iw <- initialWorld
    world <- getConfiguration iw
    SD.createDirectoryIfMissing False "/var/winot"

    -- log to a file

    let cll = configString "log_level" world
    let defaultLogLevel =
            if cll == Just "debug" then L.DEBUG
            else L.INFO

    L.updateGlobalLogger L.rootLoggerName L.removeHandler -- remove default handler (stderr)
    L.updateGlobalLogger "winot" (L.setLevel defaultLogLevel)
    nl <- LH.fileHandler "/var/log/winot" L.DEBUG >>=
         \lh -> M.return $
            LH.setFormatter lh (LF.simpleLogFormatter "[$time : $loggername : $prio] $msg")
    L.updateGlobalLogger "winot" (L.addHandler nl)
    L.infoM logPrefix "starting up"

    -- handle quit signals

    tid <- C.myThreadId
    _ <- P.installHandler P.sigINT  (P.Catch (cleanUp world >> C.throwTo tid E.ExitSuccess)) Nothing
    _ <- P.installHandler P.sigTERM (P.Catch (cleanUp world >> C.throwTo tid E.ExitSuccess)) Nothing
    _ <- P.installHandler P.sigQUIT (P.Catch (cleanUp world >> C.throwTo tid E.ExitSuccess)) Nothing
    _ <- P.installHandler P.sigHUP  (P.Catch (cleanUp world >> C.throwTo tid E.ExitSuccess)) Nothing

    -- start the main loop

    M.iterateM_ mainLoop world

mainLoop :: World -> IO World
mainLoop world = do
    let logPrefix = "winot.mainLoop"
    let secondsBetweenLoops = 1
    L.debugM logPrefix "start loop"

    world' <- recordLoopTimes world >>= getConfiguration
    L.debugM logPrefix $ T.unpack $ "lastLoop: " `T.append` T.pack (show (headMay (loopTimes world')))
    L.debugM logPrefix $ T.unpack $ "thisLoop: " `T.append` T.pack (show (lastMay (loopTimes world')))

    _ <- C.forkIO $ recordWLANSignalStrength world'
    wlif <- atomRead $ wlanIf world'
    M.when (B.isJust wlif) $ do
        _ <- C.forkIO $ recordBandwidth world' (B.fromJust wlif) (wlanBandwidthLog world')
        M.return ()
    vif <- atomRead $ vpnIf world'
    M.when (B.isJust vif) $ do
        _ <- C.forkIO $ recordBandwidth world' (B.fromJust vif) (vpnBandwidthLog world')
        M.return ()

    -- skip most of the work if we're in our steady state of being connected to VPN
    -- dr <- runRead "route -n get -inet default"
    -- vpnok <- maybe
    --     (M.return False)
    --     (\ip -> if ip `T.isInfixOf` dr then vpnConnOK world' else M.return False)
    --     (configString "vpn_server_private_ip" world)
    let vpnok = False

    tryLockFork "interfaceListLock" (interfaceListLock world') (updateInterfaceList world')
    tryLockFork "interfaceStatsLock" (interfaceStatsLock world') (updateInterfaceStats world')
    tryLockFork "checkWLANLock" (checkWLANLock world') (checkWLANScanRequest world')
    M.unless vpnok $ do
        tryLockFork "processListLock" (processListLock world') (updateProcessList world')
        tryLockFork "checkWWANLock" (checkWWANLock world') (checkWWAN world')
        tryLockFork "checkWLANLock" (checkWLANLock world') (checkWLAN world')
        tryLockFork "checkVPNLock" (checkVPNLock world') (checkVPN world')
        tryLockFork "checkRoute" (checkRouteLock world') (checkRoute world')
        M.return ()

    _ <- outputStatus world'
    _ <- D.delay $ secondsBetweenLoops * (10 :: Integer) ^ (6 :: Integer)
    M.return world'

cleanUp :: World -> IO ()
cleanUp world = do
    let logPrefix = "winot.cleanUp"
    L.infoM logPrefix "exiting"

    -- TODO: kill all the running threads, so we aren't fighting with ourself


    -- kill related processes that are not under our direct control
    run "pkill -f pppd"
    vcomm <- vpnCommand world
    M.when (B.isJust vcomm) $ do
        L.debugM logPrefix $ T.unpack $ "this causes a crash: pkill -f " `T.append` B.fromJust vcomm
        {-system $ "pkill -f " `T.append` B.fromJust vcomm --crash!-}
        M.return ()

    -- clear out the route and interface config changes we made
    _ <- D.delay $ 1 * (10 :: Integer) ^ (6 :: Integer) -- wait for pppd to exit and free up wwanif
    run "route -qn flush"
    vif <- atomRead $ vpnIf world
    M.when (B.isJust vif) $ do
        run $ "ifconfig " `T.append` B.fromJust vif `T.append` " destroy"
        M.return ()
    wlif <- atomRead $ wlanIf world
    M.when (B.isJust wlif) $ do
        run $ "ifconfig " `T.append` B.fromJust wlif `T.append` " -nwid -wpakey -inet down"
        M.return ()
    wwif <- atomRead $ wwanIf world
    M.when (B.isJust wwif) $ do
        run $ "ifconfig " `T.append` B.fromJust wwif `T.append` " destroy"
        M.return ()

updateProcessList :: World -> IO ()
updateProcessList world = do
    lst <- runRead "ps axw"
    atomWrite (processList world) lst
    M.return ()

updateInterfaceList :: World -> IO ()
updateInterfaceList world = do
    lst <- runRead "ifconfig"
    atomWrite (interfaceList world) (parseInterfaceList lst)
    M.return ()

recordLoopTimes :: World -> IO World
recordLoopTimes world = do
    let numOfTimestampsToKeep = 2
    time <- K.getTime K.Realtime
    let !newLoopTimes = reverse (take (numOfTimestampsToKeep-1) $ reverse (loopTimes world)) <> [K.sec time]
    M.return world { loopTimes = newLoopTimes }

getConfiguration :: World -> IO World
getConfiguration world = do
    let logPrefx = "winot.getConfiguration"
    con <- readFile "/etc/winot"
    fs <- P.getFileStatus "/etc/winot"
    let mt = case P.modificationTime fs of
                F.CTime ct -> ct
    if mt > configModified world then do
        L.debugM logPrefx "reading configuration"
        let conf = O.parseTomlDoc "" con
        case conf of
            Left _ -> E.die "could not parse config, probably"
            Right c ->
                return world { config = c
                        , configModified = mt
                        }
    else
        return world
