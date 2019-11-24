{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : DeviceApi
Description : Exposes CRUD methods for registering devices
              Note that it requires the environment variable motoEnv=[dev|prod]
Copyright   : Marius-Florin Cristian
License     : GPL-3
Maintainer  : mfc@marius-cristian.com
Stability   : development
Portability : POSIX

Insertion of new devices: id is unique
Update of location if its in the list of allowed location
Retrieval of a locaiton
-}

module DeviceApi (
      insertRadio
    , setLocation
    , getLocation
    ) where
import MongoUtils
import Database.MongoDB
import Environment (MotoTransformer)
import Types

findRadio :: String -> MotoTransformer (Maybe Device)
findRadio rid = getDoc $ findOne $ select ["radioId" =: rid] "devices"

insertRadio :: String -> Payload -> MotoTransformer (Maybe String)
insertRadio rid payload = do
    mradio <- findRadio rid
    case mradio of
        Just _ -> return Nothing
        Nothing -> do
            let newradio = ["radioId" =: rid, "name" =: (alias payload), "allowed" =: (allowed_locations payload)]
            _ <- run $ insert "devices" newradio
            return $ Just "ok"

setLocation :: String -> Location -> MotoTransformer (Maybe String)
setLocation rid nl = do
    radio <- getDoc $ findOne (select ["radioId" =: rid, "allowed" =: ["$in" := val (location nl)] ] "devices")
    case radio of
        Nothing -> return Nothing
        Just r -> do 
            _ <- updateDoc "devices" (r {loc = (Just $ location nl)})
            return $ Just "ok"


getLocation :: String -> MotoTransformer (Maybe Location)
getLocation rid = do
    radio <- findRadio rid
    -- ugly must refactor
    case radio of
        Nothing -> return Nothing
        Just r -> case loc r of
            Nothing -> return Nothing
            Just s -> return $ Just $ Location s
