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
  ApiOptions (..),
  apiOpts
) where



import Data.ByteString (ByteString)
import Control.Exception
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans
import Control.Monad.Trans.RWS
import           Control.Monad.Trans.RWS as A
import           Data.Text               (Text)
import qualified Database.MySQL.Simple   as My
import qualified Database.Redis          as R
import           Haskell.Api.Helpers



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



-- rd :: ReaderT ApiOptions m a -> m a
-- rd :: RWST MigrateReader
-- rd :: MigratwRWST
rd actions = do
--  opts <- asks rApiOpts
  let opts = apiOpts
  runWith actions $ opts { apiKey = Just "1" }

-- rw :: ReaderT ApiOptions m a -> ByteString -> m a
rw actions s = do
--  opts <- asks rApiOpts
  let opts = apiOpts
  runWith actions $ opts { apiKey = Just s }



rd'
  :: (Monoid w, MonadIO m)
  => ReaderT ApiOptions IO (Either ApiError a)
  -> RWST MigrateReader w s m (Either SomeException (Either ApiError a))
rd' actions = do
  opts <- asks rApiOpts
  liftIO $ (try (runWith actions $ opts { apiKey = Just "1" }))



apiOpts :: ApiOptions
apiOpts = ApiOptions {
  apiUrl = "https://leuro.adarq.org",
  apiPrefix = "api",
  apiKey = Nothing,
  apiKeyHeader = Just "z-authorization",
  apiWreqOptions = defaultWreqOptions,
  apiDebug = True
}
