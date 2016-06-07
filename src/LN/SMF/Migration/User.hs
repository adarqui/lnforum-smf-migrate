{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LN.SMF.Migration.User (
  createSuperUser,
  removeSuperUser,
  createLegacyUsers,
  deleteLegacyUsers
) where



import           Haskell.Api.Helpers
import           Control.Exception
import           Control.Monad                  (forM_, void)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.RWS
import           Data.Int
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Database.MySQL.Simple
import           LN.T
import           LN.Api
import           LN.SMF.Migration.Connect.Redis
import           LN.SMF.Migration.Control
import           LN.SMF.Migration.Sanitize



-- | this is just for 'adarqui' which is user ID 1 on leuro AND on SMF
--
createSuperUser :: MigrateRWST ()
createSuperUser = do
  createRedisMap "usersName" 1 1



-- | this doesn't actually remove a user, just the redis key
--
removeSuperUser :: MigrateRWST ()
removeSuperUser = do
  deleteRedisMapByLnId "usersName" 1



--
-- We will need to:
-- create these users, associate their leuro ID with their SMF id.
-- later, fix createdAt timestamps etc in leuro
--
createLegacyUsers :: MigrateRWST ()
createLegacyUsers = do

  liftIO $ putStrLn "migrating users.."

  mysql <- asks rMySQL
  limit <- asks rLimit

  xs <- liftIO $ query mysql "select id_member, member_name, real_name, email_address, date_registered from smf_members LIMIT ?" (Only limit)

  smf_ids <- smfIds "usersName"

  -- filter out users who have already been added
  --
  forM_
    (filter (\(id_member, _, _, _, _) -> not $ id_member `elem` smf_ids) xs)
    (\(id_member :: Int64,
       member_name :: Text,
       real_name :: Text,
       email_address :: Text,
       date_registered :: Int
      ) -> do

      mresult <- findLnIdFromSmfId "usersName" id_member

      case mresult of

        (Just _) -> liftIO $ putStrLn $ (T.unpack member_name) <> " already exists. skipping"

        Nothing -> do

          liftIO $ putStrLn $ show [show id_member, T.unpack member_name, T.unpack real_name, T.unpack email_address, show date_registered]

          eresult <- liftIO (try (rd (postUser [UnixTimestamp $ fromIntegral date_registered] $
            UserRequest (fixDisplayNick member_name) real_name email_address "smf" (T.pack $ show id_member))) :: IO (Either SomeException (Either ApiError UserResponse)))

          case eresult of
            (Left err) -> liftIO $ putStrLn $ show err
            (Right (Left err)) -> liftIO $ putStrLn $ show err
            (Right (Right user_response)) -> do
              createRedisMap "usersName" id_member (userResponseId user_response)
              return ()

    ) -- forM_

  return ()




deleteLegacyUsers :: MigrateRWST ()
deleteLegacyUsers = do

  user_ids <- lnIds "usersName"

  forM_ user_ids

    (\user_id -> do

      if user_id == 1
        then return ()
        else do
          liftIO $ putStrLn $ show user_id
          void $ liftIO (try (rd (deleteUser' user_id)) :: IO (Either SomeException (Either ApiError ())))
          deleteRedisMapByLnId "usersName" user_id
    )

  return ()
