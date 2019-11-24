{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : Config
Description : Loads the configuration files either from Kubernetes path, or local path.
              Note that it requires the environment variable motoEnv=[dev|prod]
Copyright   : Marius-Florin Cristian
License     : GPL-3
Maintainer  : mfc@marius-cristian.com
Stability   : development
Portability : POSIX

The module exposes a datatype for a Configuration file. The file must be in JSON format.
If the file does not exist the module will cause a runtime exception and crash the program.
Thus be sure that :set args is set to prod or dev, and the json file exists in the system path
./src/config/cofnig.json for dev.
-}

module Config(Configuration(..), configuration) where
import           Data.Aeson
import           GHC.Generics
import           System.Environment(getEnv)
import           Data.Text
import qualified Data.ByteString.Lazy as B

-- | Local file path, used only for development/ testing
localFile :: FilePath
localFile = "./src/config/config.json"

-- | If the file does not exist, this crashes the program
getConfigLocal :: IO B.ByteString
getConfigLocal = B.readFile localFile

-- | Needed to extract the datastructure from the file
mapDecode :: IO B.ByteString -> IO (Maybe Configuration)
mapDecode x = fmap decode $ x

-- | 
configuration :: IO (Maybe Configuration)
configuration = do
    env <- getEnv "motoEnv"
    x <- case env of
        "dev" -> mapDecode getConfigLocal
        _     -> mapDecode getConfigLocal -- subsequently for different environments
    return x

-- | Configuration data type
data Configuration = Configuration {
      dbName     :: Text
    , dbUrl      :: String
    , dbUser     :: String
    , dbPassword :: String   
} deriving (Show, Generic)

instance FromJSON Configuration

