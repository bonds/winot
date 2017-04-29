{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lease where

import Protolude hiding (handle)
import Util.Run
import Util.Log
import Status.Interface
import qualified Control.Monad.Logger as ML

default (Text, Integer, Double)

getLease :: Interface -> ML.LoggingT IO Text
getLease i = runRead LRStatus $ "grep routers /var/db/dhclient.leases." <> interface i <> " | tail -n 1 | sed 's/.* \\(.*\\);/\\1/g')"

