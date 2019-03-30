{-# LANGUAGE OverloadedStrings #-}

module Main where



import qualified Data.Text          as T
import           LN.SMF.Migration
import           System.Environment



-- TODO FIXME: need to pass flags, optparse etc
usage :: IO ()
usage = putStrLn "ln-smf-migrate [migrate|unmigrate]"


main :: IO ()
main = do
  argv <- (map T.pack) <$> getArgs
  case argv of
    ["migrate"]   -> migrateSMF defaultMigrateConfig
    ["unmigrate"] -> unMigrateSMF defaultUnmigrateConfig
    _             -> usage
