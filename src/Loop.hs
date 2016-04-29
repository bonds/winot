{-# LANGUAGE BangPatterns #-}

module Loop where

import Protolude
import Prelude (($), take, (++))
import Control.Monad ((>>=), (>>))
import Route
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
import qualified System.Clock as K
import qualified System.Exit as E
import qualified System.Log.Formatter as LF
import qualified System.Log.Handler as LH
import qualified System.Log.Handler.Simple as LH
import qualified System.Log.Logger as L
import qualified System.Posix.Process as P
import qualified System.Posix.Signals as P

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

    w <- initialWorld
    M.when (B.isNothing w) (E.die "could not parse config, probably")
    let world = B.fromJust w

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

    -- more setup

    updateInterfaceList world -- get initial interface list for use in choosing wwanif
    wwif <- chooseWWANIf world
    wlif <- chooseWLANIf world
    vif <- chooseVPNIf world
    let world' = world { wwanIf = wwif
                       , vpnIf = vif
                       , wlanIf = wlif
                       }

    L.debugM logPrefix $ T.unpack $ T.concat ["wwanIf is ", T.pack (show wwif)]
    if wwanEnabled world' then do
        L.debugM logPrefix "wwan enabled"
        M.when (B.isJust wwif) $ do
            run "pkill -f pppd"
            run $ "ifconfig " `T.append` B.fromJust wwif `T.append` " create up"
            M.return ()
    else
        L.infoM logPrefix "wwan disabled"

    if wlanEnabled world' then do
        L.debugM logPrefix "wlan enabled"
        M.return ()
    else
        L.infoM logPrefix "wlan disabled"

    if vpnEnabled world' then do
        L.debugM logPrefix "vpn enabled"
        let cpip = configString "vpn_client_private_ip" world'
        let spip = configString "vpn_server_private_ip" world'
        let nmask = configString "vpn_private_netmask" world'
        if B.isJust cpip && B.isJust spip && B.isJust nmask && B.isJust vif then do
            run $ T.concat [ "ifconfig "
                           , B.fromJust vif
                           , " create "
                           , B.fromJust cpip
                           , " "
                           , B.fromJust spip
                           , " netmask "
                           , B.fromJust nmask
                           , " up"
                           ]
            M.return ()
        else
            L.errorM logPrefix "vpn missing config, but enabled?!"
    else
        L.infoM logPrefix "vpn disabled"

    -- handle quit signals

    tid <- C.myThreadId
    _ <- P.installHandler P.sigINT  (P.Catch (cleanUp world' >> C.throwTo tid E.ExitSuccess)) Nothing
    _ <- P.installHandler P.sigTERM (P.Catch (cleanUp world' >> C.throwTo tid E.ExitSuccess)) Nothing
    _ <- P.installHandler P.sigQUIT (P.Catch (cleanUp world' >> C.throwTo tid E.ExitSuccess)) Nothing
    _ <- P.installHandler P.sigHUP  (P.Catch (cleanUp world' >> C.throwTo tid E.ExitSuccess)) Nothing

    -- start the main loop

    M.iterateM_ mainLoop world'

mainLoop :: World -> IO World
mainLoop world = do
    let logPrefix = "winot.mainLoop"
    L.debugM logPrefix "start loop"
    let secondsBetweenLoops = 1

    world' <- recordLoopTimes world
    L.debugM logPrefix $ T.unpack $ "lastLoop: " `T.append` T.pack (show (headMay (loopTimes world')))
    L.debugM logPrefix $ T.unpack $ "thisLoop: " `T.append` T.pack (show (lastMay (loopTimes world')))

    _ <- C.forkIO $ recordWLANSignalStrength world'
    _ <- C.forkIO $ recordWLANBandwidth world'

    dr <- runRead "route -n get -inet default"
    vpnok <- maybe
        (M.return False)
        (\ip -> if ip `T.isInfixOf` dr then ping 3 ip else M.return False)
        (configString "vpn_server_private_ip" world)

    M.unless vpnok $ do
        tryLockFork "processListLock" (processListLock world) (updateProcessList world')
        tryLockFork "interfaceListLock" (interfaceListLock world) (updateInterfaceList world')
        tryLockFork "checkWWANLock" (checkWWANLock world) (checkWWAN world')
        tryLockFork "checkWLANLock" (checkWLANLock world) (checkWLAN world')
        tryLockFork "checkVPNLock" (checkVPNLock world) (checkVPN world')
        tryLockFork "checkRoute" (checkRouteLock world) (checkRoute world')
        M.return ()

    _ <- D.delay $ secondsBetweenLoops * (10 :: Integer) ^ (6 :: Integer)
    M.return world'

cleanUp :: World -> IO ()
cleanUp world = do
    let logPrefix = "winot.cleanUp"
    L.infoM logPrefix "exiting"

    -- TODO: kill all the running threads, so we aren't fighting with ourself


    -- kill related processes that are not under our direct control
    run "pkill -f pppd"
    M.when (B.isJust vcomm) $ do
        L.debugM logPrefix $ T.unpack $ "this causes a crash: pkill -f " `T.append` B.fromJust vcomm
        {-system $ "pkill -f " `T.append` B.fromJust vcomm --crash!-}
        M.return ()

    -- clear out the route and interface config changes we made
    _ <- D.delay $ 1 * (10 :: Integer) ^ (6 :: Integer) -- wait for pppd to exit and free up wwanif
    run "route -qn flush"
    M.when (B.isJust vif) $ do
        run $ "ifconfig " `T.append` B.fromJust vif `T.append` " destroy"
        M.return ()
    M.when (B.isJust wlif) $ do
        run $ "ifconfig " `T.append` B.fromJust wlif `T.append` " -nwid -wpakey -inet down"
        M.return ()
    M.when (B.isJust wwif) $ do
        run $ "ifconfig " `T.append` B.fromJust wwif `T.append` " destroy"
        M.return ()
  where
    vcomm = vpnCommand world
    vif = vpnIf world
    wlif = wlanIf world
    wwif = wwanIf world

updateProcessList :: World -> IO ()
updateProcessList world = do
    list <- runRead "ps -axw"
    atomWrite (processList world) list
    M.return ()

updateInterfaceList :: World -> IO ()
updateInterfaceList world = do
    list <- runRead "ifconfig"
    atomWrite (interfaceList world) (parseInterfaceList list)
    M.return ()

recordLoopTimes :: World -> IO World
recordLoopTimes world = do
    let numOfTimestampsToKeep = 2
    time <- K.getTime K.Realtime
    let !newLoopTimes = reverse (take (numOfTimestampsToKeep-1) $ reverse (loopTimes world)) ++ [K.sec time]
    M.return world { loopTimes = newLoopTimes }

