{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Types where
import           Data.Aeson
import           Data.Bson.Generic
import           Data.Bson
import           GHC.Generics      (Generic)
import           Data.Data         (Typeable)

data Device = Device {
      radioId    :: Integer
    , name       :: String
    , loc        :: Maybe String
    , allowed    :: [String]
} deriving (Generic, Typeable, Show)

data Payload = Payload {
      alias              :: String
    , allowed_locations  :: [String]
} deriving (Generic, Typeable, Show)

data Location = Location {
    location :: String
} deriving (Generic, Typeable, Show)

instance FromJSON Payload
instance FromJSON Location

instance ToJSON Payload
instance ToJSON Location 

instance ToJSON Device
instance ToBSON Device

instance FromBSON Device
instance FromJSON Device