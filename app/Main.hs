{-# LANGUAGE OverloadedStrings #-}

module Main where



import qualified Data.Text          as T
import           LN.SMF.Migration
import           System.Environment



usage :: IO ()
usage = putStrLn "ln-smf-migrate <super_key> <org_sid> <redis_host> <mysql_host> <api_host> [migrate <limit>|unmigrate]"


main :: IO ()
main = do
  argv <- (map T.pack) <$> getArgs
  case argv of
    [super_key, org_sid, redis_host, mysql_host, api_host, "migrate", n] -> migrateSMF super_key org_sid redis_host mysql_host api_host (read $ T.unpack n)
    [super_key, org_sid, redis_host, mysql_host, api_host, "unmigrate"]  -> unMigrateSMF super_key org_sid redis_host mysql_host api_host
    _              -> usage
