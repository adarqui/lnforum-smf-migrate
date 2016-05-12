{-# LANGUAGE OverloadedStrings #-}

module LN.SMF.Migration.Connect.MySQL (
  connectMySQL
) where



import           Database.MySQL.Simple



connectMySQL :: IO Connection
connectMySQL = do
  connect info
  where
  info = defaultConnectInfo {
    connectHost = "10.0.3.14",
    connectUser = "root",
    connectPassword = "root",
    connectDatabase = "adarq_forum"
  }
