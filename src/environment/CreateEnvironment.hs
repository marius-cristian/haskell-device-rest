{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : CreateEnvironment
Description : Exposes a method for creating an envrionment
              Note that it requires the environment variable motoEnv=[dev|prod]
Copyright   : Marius-Florin Cristian
License     : GPL-3
Maintainer  : mfc@marius-cristian.com
Stability   : development
Portability : POSIX

This module populates the reader monad with the config file and db connection pool
-}


module CreateEnvironment where

import Environment
import Config
import MongoUtils (getPool)

-- | Creates an environment.
createEnvironment :: IO Environment
createEnvironment = do
  conf <- configuration
  case conf of
    Nothing -> fail "No configuration found for MotoProject"
    Just c -> do
      pool <- getPool c
      return $ Environment pool c