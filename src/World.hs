{-# LANGUAGE NoImplicitPrelude #-}

module World where

import Protolude
import Status.Process
import Status.Interface
import qualified GHC.Int as G
import Data.UnixTime (UnixTime)
-- import Util.Misc

default (Text)

data World = World
    { woConfig            :: Config
    , woLoopTimes         :: [UnixTime]
    , woInterfaces        :: [Interface]
    , woProcesses         :: [Process]
    } deriving Show

data Config = Config
    { coWLANEnabled         :: Bool
    , coLastModified        :: G.Int64
    } deriving Show

initialWorld :: World
initialWorld = World
    { woConfig = Config
        { coWLANEnabled   = True
        , coLastModified  = 0
        }
    , woLoopTimes         = []
    , woInterfaces        = []
    , woProcesses         = []
    }
