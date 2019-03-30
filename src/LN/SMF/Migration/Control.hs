{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.SMF.Migration.Control (
  module A,
  MigrateState (..),
  MigrateReader (..),
  MigrateWriter,
  MigrateRWST,
  incStCounter,
  decStCounter,
  resetStCounter,
  rd,
  rw,
  rd',
  rw',
  ApiOptions (..),
  apiOpts,
  defaultForumId
) where



import           Control.Exception
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.RWS
import           Control.Monad.Trans.RWS    as A
import           Data.ByteString            (ByteString)
import           Data.Int                   (Int64)
import           Data.Monoid                ((<>))
import           Data.String.Conversions    (cs)
import           Data.Text                  (Text)
import qualified Database.MySQL.Simple      as My
import qualified Database.Redis             as R
import           Haskell.Api.Helpers
import           Haskell.Api.Helpers.Shared

import           LN.T.Error                 (ApplicationError)



data MigrateState = MigrateState {
  stCounter :: Int64
}



incStCounter :: MigrateRWST ()
incStCounter = do
  modify (\st@MigrateState{..} -> st { stCounter = stCounter + 1 })



decStCounter :: MigrateRWST ()
decStCounter = do
  modify (\st@MigrateState{..} -> st { stCounter = stCounter - 1 })



resetStCounter :: MigrateRWST ()
resetStCounter = do
  modify (\st@MigrateState{..} -> st { stCounter = 0 })



data MigrateReader = MigrateReader {
  rSuperKey         :: ByteString,
  rRedisHost        :: Text,
  rRedis            :: R.Connection,
  rMySQLHost        :: Text,
  rMySQL            :: My.Connection,
  rApiHost          :: Text,
  rApiOpts          :: ApiOptions SpecificApiOptions,
  rUsersLimit       :: Int,
  rBoardsLimit      :: Int,
  rThreadsLimit     :: Int,
  rThreadPostsLimit :: Int,
  rThreadPostLikesLimit :: Int
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
  => Int64
  -> ReaderT (ApiOptions SpecificApiOptions) IO (Either (ApiError ApplicationError) a)
  -> RWST MigrateReader w s m (Either (ApiError ApplicationError) a)
rw user_id actions = do
  opts      <- asks rApiOpts
  super_key <- asks rSuperKey
  liftIO $ runWith actions $ opts { apiKeyHeader = Just "x-as-user", apiKey = Just (super_key <> (cs $ show user_id)) }



rw'
  :: (Monoid w, MonadIO m)
  => Int64
  -> ReaderT (ApiOptions SpecificApiOptions) IO (Either (ApiError ApplicationError) a)
  -> RWST MigrateReader w s m (Either SomeException (Either (ApiError ApplicationError) a))
rw' user_id actions = do
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



defaultForumId :: Int64
defaultForumId = 1
