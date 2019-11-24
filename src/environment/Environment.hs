{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : Environment
Description : Exposes type aliases for monad transformers, a reader monad
              Note that it requires the environment variable motoEnv=[dev|prod]
Copyright   : Marius-Florin Cristian
License     : GPL-3
Maintainer  : mfc@marius-cristian.com
Stability   : development
Portability : POSIX

This module exposes type aliases for our reader monad that holds information such as db connection pool,
configuration file, (maybe logger, etc.), and the monad transformers associated with it.
-}

module Environment where
import Config
import Control.Monad.Reader
import Data.Pool
import Data.Text.Lazy (Text)
import Web.Scotty.Trans
import Database.MongoDB

type MotoTransformer   = ReaderT Environment IO
type ScottyTransformer = ScottyT Text MotoTransformer
type ActionTransformer = ActionT Text MotoTransformer


getDbPool :: MotoTransformer (Pool Pipe)
getDbPool = asks dbPool

getConfig :: MotoTransformer Configuration
getConfig = asks config

data Environment = Environment{
      dbPool :: Pool Pipe
    , config :: Configuration
}
