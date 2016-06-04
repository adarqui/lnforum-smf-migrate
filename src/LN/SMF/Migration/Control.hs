module LN.SMF.Migration.Control (
  module A,
  MigrateState (..),
  MigrateReader (..),
  MigrateWriter,
  MigrateRWST,
  rd,
  rw
) where



import           Haskell.Api.Helpers
import           Control.Monad.Trans.RWS as A
import qualified Database.MySQL.Simple   as My
import qualified Database.Redis          as R



data MigrateState = MigrateState {
  stTest :: Bool
}



data MigrateReader = MigrateReader {
  rRedis :: R.Connection,
  rMySQL :: My.Connection,
  rLimit  :: Int
}



type MigrateWriter = ()



type MigrateRWST = RWST MigrateReader MigrateWriter MigrateState IO



rd actions = runWith actions apiOpts { apiKey = Just "1" }

rw actions s = runWith actions $ apiOpts { apiKey = Just s }


apiOpts = ApiOptions {
  apiUrl = "https://leuro.adarq.org",
  apiPrefix = "api",
  apiKey = Nothing,
  apiKeyHeader = Just "z-authorization",
  apiWreqOptions = defaultWreqOptions,
  apiDebug = True
}
