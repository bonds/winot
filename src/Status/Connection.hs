{-# OPTIONS_GHC -Wall -Werror  #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Status.Connection where

import Protolude
import Util.Run
import Util.Misc
import Util.Log
-- import Util.Log
-- import qualified Text.Trifecta as Parse
import qualified Data.Text as T
import qualified Control.Monad.Logger as ML
-- import qualified GHC.Show
import qualified Control.Concurrent.Thread.Delay as D

default (Text, Integer, Double)

-- for rdomain 0
-- pf rules are correct
-- using the highest priority, ok rdomain if
-- can ping internet

-- for each interface

-- gets its own rdomain, is it in the right one?
-- has ip
-- pf rules are correct
-- ipforwarding enabled
-- can ping gateway
-- can ping internet

-- ipsec and ipcomp enabled via sysctl
-- vether if exists and has the right config
-- ipsec connection exists
-- can ping gateway

-- gre sysctl enabled
-- gre if exists and has the right config
-- gre connection exists
-- can ping gateway
-- can ping internet

-- idle :: World -> [Maybe Int] -> IO (Maybe Bool)
-- idle world l = if length l >= intervals then do
--                    let ln = B.catMaybes $ lastN intervals l
--                    if length ln == intervals then
--                        case F.maximumMay ln of
--                            Just m -> if m < imeans then
--                                    M.return $ Just True
--                                else
--                                    M.return $ Just False
--                            Nothing -> M.return Nothing
--                    else
--                        M.return Nothing
--                else
--                    M.return Nothing
--   where
--     intervals = B.maybe 30 (B.fromMaybe 30 . readMaybe . T.unpack) $ configString "IdleIntervalsBeforeIdle" world
--     imeans = B.maybe 1000 (B.fromMaybe 1000 . readMaybe . T.unpack) $ configString "IdleMeansLessThanXBytes" world

-- recordBandwidth :: World -> T.Text -> S.TVar [Maybe Int] -> IO ()
-- recordBandwidth world interface bl = do
--     bw <- bandwidth world interface
--     l <- atomRead bl
--     let !values = lastN (itemsToKeep-1) l <> B.maybe [B.Nothing] (\x -> [readMaybe $ T.unpack x]) bw
--     atomWrite bl values
--     l' <- atomRead bl
--     L.debugM (T.unpack logPrefix) $ T.unpack $ T.concat [interface, "bw: ", T.pack (show (lastN 5 l'))]
--   where
--     itemsToKeep = 100
--     logPrefix = T.concat ["winot.recordBandwidth.", interface]

-- bandwidth :: World -> T.Text -> IO (Maybe T.Text)
-- bandwidth world interface = do
--     stats <- atomRead (interfaceStats world)
--     M.return $ case U.find
--             (U.regex [U.Multiline] ("^" `T.append` interface `T.append` ".*"))
--             stats of
--         Just m -> T.words (B.fromJust $ U.group 0 m) `atMay` 6
--         Nothing -> Nothing

-- ping up to X times, if any are OK, stop, and return OK
ping :: Integer -> Text -> ML.LoggingT IO Bool
ping = pingVia 0

{-@ pingVia :: Nat -> count:Integer -> T.Text -> ML.LoggingT IO Bool / [count] @-}
pingVia :: Integer -> Integer -> Text -> ML.LoggingT IO Bool
pingVia rt count host
    | count < 1 = return False
    | otherwise = do
        ec <- runEC LRStatus $ T.concat ["ping -V ", T.pack (show rt), " -q -c 1 -w 1 ", host]
        if wasSuccess ec then return True else do
            liftIO $ D.delay $ (10::Integer)^(6::Integer) -- wait a second between ping attempts
            pingVia rt (count-1) host

