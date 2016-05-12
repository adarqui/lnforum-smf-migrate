module Main where



import System.Environment
import           LN.SMF.Migration



usage :: IO ()
usage = putStrLn "ln-smf-migrate [migrate <limit>|unmigrate]"


main :: IO ()
main = do
  argv <- getArgs
  case argv of
    ["migrate", n] -> migrateSMF (read n)
    ["unmigrate"]  -> unMigrateSMF
    _              -> usage
