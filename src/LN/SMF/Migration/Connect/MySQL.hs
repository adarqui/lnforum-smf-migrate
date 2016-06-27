{-# LANGUAGE OverloadedStrings #-}

module LN.SMF.Migration.Connect.MySQL (
  connectMySQL
) where



import           Data.Text             (Text)
import qualified Data.Text             as T (unpack)
import           Database.MySQL.Simple



connectMySQL :: Text -> IO Connection
connectMySQL mysql_host = do
  connect info
  where
  info = defaultConnectInfo {
    connectHost     = T.unpack mysql_host,
    connectUser     = "root",
    connectPassword = "root",
    connectDatabase = "adarq_forum"
  }
