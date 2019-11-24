{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : Rest
Description : Starts the rest endpoint
Copyright   : Marius-Florin Cristian
License     : GPL-3
Maintainer  : mfc@marius-cristian.com
Stability   : development
Portability : POSIX

This module exposes a method for starting the rest endpoint
-}

module Rest where
import           Environment
import           Web.Scotty.Trans
import           Data.String (fromString)
import           Data.Aeson (ToJSON, FromJSON)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Text.Lazy (Text)
import           CreateEnvironment
import           DeviceApi
import           Network.HTTP.Types.Status
import           Data.Aeson((.=), object)

-- | route for base endpoint
apiRoot :: String
apiRoot = "/radios/"

apiGet ::
  (MonadIO m, ScottyError e)
  => String
  -> (ActionT e m ())
  -> ScottyT e m ()
apiGet path f = get (fromString $ apiRoot ++ path) $ f

apiPost path f = post (fromString $ apiRoot ++ path) $ f

-- | Deserialize data from json string "{object}" to the appropriate data type
withJsonData :: FromJSON p => (p -> MotoTransformer a) -> ActionTransformer a
withJsonData f = do
  r <- jsonData
  lift $ f r

-- | Deserialize data from just param to the appropriate dataType
withStringParam :: Text -> (String -> MotoTransformer a) -> ActionTransformer a
withStringParam paramName f = do
  parameter <- param paramName
  lift $ f parameter

failure::ActionT Text MotoTransformer ()
failure = json ("{error:ok}")

stringParam p = param p:: ActionT Text MotoTransformer String

_getLocation :: ActionT Text MotoTransformer ()
_getLocation = do
  loc <- withStringParam "id" getLocation
  case loc of
    Nothing -> failure
    Just c -> json c

_addDevice :: ActionT Text MotoTransformer ()
_addDevice = do
  tagid <- stringParam "id"
  res <- withJsonData (insertRadio tagid)
  case res of
    Nothing -> failure
    Just _ -> text "ok"


_addLocation :: ActionT Text MotoTransformer ()
_addLocation = do
  tagid <- stringParam "id"
  res <- withJsonData (setLocation tagid)
  case res of
    Nothing-> failure
    Just _ -> text "ok"


-- | Starts the scotty server wit hthe appropriate registered routes
runRest :: IO ()
runRest = do
  print "Please beam me up Scotty"
  env <- createEnvironment
  let createReader r = runReaderT r env
  scottyT 80 createReader $ do
    defaultHandler h
    apiGet ":id/location" _getLocation
    apiPost ":id" _addDevice
    apiPost ":id/location" _addLocation
    where h e = json (object [ "error" .= show e ])
              >> status notFound404