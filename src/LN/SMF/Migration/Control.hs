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
import qualified Database.MySQL.Simple      as My
import qualified Database.Redis             as R
import           Haskell.Api.Helpers
import           LN.T.Error                 (ApplicationError)



data MigrateState = MigrateState { runMigrateState :: Int }



data MigrateReader = MigrateReader {
  rRedisHost :: Text,
  rRedis     :: R.Connection,
  rMySQLHost :: Text,
  rMySQL     :: My.Connection,
  rApiHost   :: Text,
  rApiOpts   :: ApiOptions,
  rLimit     :: Int
}



type MigrateWriter = ()



type MigrateRWST = RWST MigrateReader MigrateWriter MigrateState IO



rd
  :: (Monoid w, MonadIO m)
  => ReaderT ApiOptions IO (Either (ApiError ApplicationError) a)
  -> RWST MigrateReader w s m (Either (ApiError ApplicationError) a)
rd actions = do
  opts <- asks rApiOpts
  liftIO $ runWith actions $ opts { apiKey = Just "1" }



rd'
  :: (Monoid w, MonadIO m)
  => ReaderT ApiOptions IO (Either (ApiError ApplicationError) a)
  -> RWST MigrateReader w s m (Either SomeException (Either (ApiError ApplicationError) a))
rd' actions = do
  opts <- asks rApiOpts
  liftIO $ try (runWith actions $ opts { apiKey = Just "1" })



rw
  :: (Monoid w, MonadIO m)
  => ReaderT ApiOptions IO (Either (ApiError ApplicationError) a)
  -> ByteString
  -> RWST MigrateReader w s m (Either (ApiError ApplicationError) a)
rw actions s = do
  opts <- asks rApiOpts
  liftIO $ runWith actions $ opts { apiKey = Just s }



rw'
  :: (Monoid w, MonadIO m)
  => ReaderT ApiOptions IO (Either (ApiError ApplicationError) a)
  -> ByteString
  -> RWST MigrateReader w s m (Either SomeException (Either (ApiError ApplicationError) a))
rw' actions s = do
  opts <- asks rApiOpts
  liftIO $ try $ runWith actions $ opts { apiKey = Just s }



apiOpts :: ApiOptions
apiOpts = ApiOptions {
  apiUrl = "https://leuro.adarq.org",
  apiPrefix = "api",
  apiKey = Nothing,
  apiKeyHeader = Just "z-authorization",
  apiWreqOptions = defaultWreqOptions,
  apiDebug = True
}
