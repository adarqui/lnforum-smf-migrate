{-# LANGUAGE OverloadedStrings #-}

module Main where



import qualified Data.Text          as T
import           LN.SMF.Migration
import           System.Environment



usage :: IO ()
usage = putStrLn "ln-smf-migrate [migrate <limit>|unmigrate]"


main :: IO ()
main = do
  argv <- (map T.pack) <$> getArgs
  case argv of
    [redis_host, mysql_host, api_host, "migrate", n] -> migrateSMF redis_host mysql_host api_host (read $ T.unpack n)
    [redis_host, mysql_host, api_host, "unmigrate"]  -> unMigrateSMF redis_host mysql_host api_host
    _              -> usage
