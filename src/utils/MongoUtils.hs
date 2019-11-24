{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : MongoUtils
Description : Creates a Aonnection Pool, Runs an action against the DB
Copyright   : Marius-Florin Cristian
License     : GPL-3
Maintainer  : mfc@marius-cristian.com
Stability   : development
Portability : POSIX

This module exposes methods for creating a mongo connection pool, 
and for running actions against the db. 
-}
module MongoUtils where
import           Database.MongoDB
import           Config (Configuration(..))
import           Data.String.Conversions
import           Control.Monad.Reader (liftIO)
import           Data.Pool
import           Environment ( getDbPool
                             , getConfig
                             , getDbPool
                             , MotoTransformer)
import           Data.Bson.Generic ( FromBSON
                                   , ToBSON
                                   , toBSON
                                   , fromBSON)
-- needed for remote conenction to host, out of the scope of this assignment
import qualified Database.MongoDB.Transport.Tls as TLS

-- | Mongo must run
localPipe :: IO Pipe
localPipe = connect (host  "mongo")

-- | General pipe, can connect to local instance or to a cluster db
getPipe :: Configuration -> IO Pipe
getPipe conf =
    case conf of
        Configuration{dbUrl="local"} -> localPipe
        c ->  do
                  pipe <- TLS.connect ((dbUrl conf)) (PortNumber 27017)
                  _ <- access pipe master ("admin") (auth (convertString (dbUser c)) (convertString (dbPassword c)))
                  return pipe


-- | Connection pool
getPool :: Configuration->IO (Pool Pipe)
getPool conf = createPool (getPipe conf) close 1 300 5

-- | Run a querry against the db
run :: Action IO a -> MotoTransformer a
run a = do
  pool <- getDbPool
  conf <- getConfig
  liftIO $ withResource pool (\pipe -> access pipe master (dbName conf) a)

-- | helper to convert from DOC to appropriate Type
fromDoc :: FromBSON a => Document -> Maybe a
fromDoc x = fromBSON x

fromDocMaybe :: FromBSON a => Maybe Document -> Maybe a
fromDocMaybe  Nothing = Nothing
fromDocMaybe  (Just x) = fromDoc  x

getDoc :: FromBSON a => Action IO (Maybe Document) -> MotoTransformer (Maybe a)
getDoc q = do
            x<-run $ q
            return $ fromDocMaybe x

updateDoc :: ToBSON a => Collection -> a -> MotoTransformer String
updateDoc tablename x = do
  let doc = toBSON x
  run $ save tablename doc
  return "Ok"
