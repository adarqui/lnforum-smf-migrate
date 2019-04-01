{-# LANGUAGE OverloadedStrings #-}

module LN.SMF.Migration.Connect.MySQL (
  connectMySQL
) where



import           Data.Text             (Text)
import qualified Data.Text             as T (unpack)
import           Database.MySQL.Simple



connectMySQL :: Text -> Text -> Text -> Text -> IO Connection
connectMySQL mysql_host mysql_username mysql_password mysql_database = do
  connect info
  where
  info = defaultConnectInfo {
    connectHost     = T.unpack mysql_host,
    connectUser     = T.unpack mysql_username,
    connectPassword = T.unpack mysql_password,
    connectDatabase = T.unpack mysql_database
  }
