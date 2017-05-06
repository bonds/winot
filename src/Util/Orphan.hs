{-# OPTIONS_GHC -Wall -Werror     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}

module Util.Orphan where

import Protolude
import qualified Control.Concurrent.STM as S
import qualified GHC.Show (show)

instance Show (S.TVar a) where
    show _ = "TVar"

instance Show (S.TMVar a) where
    show _ = "TMVar"
