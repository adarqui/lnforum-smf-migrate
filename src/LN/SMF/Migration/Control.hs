module LN.SMF.Migration.Control (
  module A,
  MigrateState (..),
  MigrateReader (..),
  MigrateWriter,
  MigrateRWST
) where



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
