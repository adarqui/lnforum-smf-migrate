{-# LANGUAGE OverloadedStrings #-}

module LN.SMF.Migration.Control (
  module A,
  MigrateState (..),
  MigrateReader (..),
  MigrateWriter,
  MigrateRWST,
  rd,
  rw,
  rd',
  rw',
  ApiOptions (..),
  apiOpts
) where



import           Control.Exception
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.RWS
import           Control.Monad.Trans.RWS    as A
import           Data.ByteString            (ByteString)
import           Data.Text                  (Text)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Int (Int64)
import qualified Database.MySQL.Simple      as My
import qualified Database.Redis             as R
import           Haskell.Api.Helpers
import           Haskell.Api.Helpers.Shared

import           LN.T.Error                 (ApplicationError)



data MigrateState = MigrateState { runMigrateState :: Int }



data MigrateReader = MigrateReader {
  rSuperKey  :: ByteString,
  rOrgSid    :: Text,
  rRedisHost :: Text,
  rRedis     :: R.Connection,
  rMySQLHost :: Text,
  rMySQL     :: My.Connection,
  rApiHost   :: Text,
  rApiOpts   :: ApiOptions SpecificApiOptions,
  rLimit     :: Int
}



type MigrateWriter = ()



type MigrateRWST = RWST MigrateReader MigrateWriter MigrateState IO



rd
  :: (Monoid w, MonadIO m)
  => ReaderT (ApiOptions SpecificApiOptions) IO (Either (ApiError ApplicationError) a)
  -> RWST MigrateReader w s m (Either (ApiError ApplicationError) a)
rd actions = do
  opts      <- asks rApiOpts
  super_key <- asks rSuperKey
  liftIO $ runWith actions $ opts { apiKey = Just super_key }



rd'
  :: (Monoid w, MonadIO m)
  => ReaderT (ApiOptions SpecificApiOptions) IO (Either (ApiError ApplicationError) a)
  -> RWST MigrateReader w s m (Either SomeException (Either (ApiError ApplicationError) a))
rd' actions = do
  opts      <- asks rApiOpts
  super_key <- asks rSuperKey
  liftIO $ try (runWith actions $ opts { apiKey = Just super_key })



rw
  :: (Monoid w, MonadIO m)
  => ReaderT (ApiOptions SpecificApiOptions) IO (Either (ApiError ApplicationError) a)
  -> Int64
  -> RWST MigrateReader w s m (Either (ApiError ApplicationError) a)
rw actions user_id = do
  opts      <- asks rApiOpts
  super_key <- asks rSuperKey
  liftIO $ runWith actions $ opts { apiKeyHeader = Just "x-as-user", apiKey = Just (super_key <> (cs $ show user_id)) }



rw'
  :: (Monoid w, MonadIO m)
  => ReaderT (ApiOptions SpecificApiOptions) IO (Either (ApiError ApplicationError) a)
  -> Int64
  -> RWST MigrateReader w s m (Either SomeException (Either (ApiError ApplicationError) a))
rw' actions user_id = do
  opts      <- asks rApiOpts
  super_key <- asks rSuperKey
  liftIO $ try $ runWith actions $ opts { apiKeyHeader = Just "x-as-user", apiKey = Just (super_key <> (cs $ show user_id)) }



apiOpts :: ApiOptions SpecificApiOptions
apiOpts = ApiOptions {
  apiUrl       = "http://dev.adarq.org",
  apiPrefix    = "api",
  apiKey       = Nothing,
  apiKeyHeader = Just "x-api-authorization",
  apiOptions   = defaultSpecificApiOptions,
  apiDebug     = True
}
