{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LN.SMF.Migration.User (
  createSmfUsers,
  deleteSmfUsers
) where



import           Control.Break                  (break, lift, loop)
import           Control.Monad                  (forM_, void, when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.RWS
import           Data.Int
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Database.MySQL.Simple
import           Prelude                        hiding (break)

import           LN.Api
import qualified LN.Api.String                  as ApiS
import           LN.Sanitize.Internal
import           LN.SMF.Migration.Connect.Redis
import           LN.SMF.Migration.Control
import           LN.SMF.Migration.Sanitize
import           LN.T



--
-- We will need to:
-- create these users, associate their leuro ID with their SMF id.
-- later, fix createdAt timestamps etc in leuro
--
createSmfUsers :: MigrateRWST ()
createSmfUsers = do

  liftIO $ putStrLn "migrating users.."

  mysql  <- asks rMySQL
  limit  <- asks rLimit
  org_id <- gets stOrgId

  xs <- liftIO $ query mysql "select id_member, member_name, real_name, email_address, date_registered from smf_members ORDER BY id_member ASC LIMIT ?" (Only limit)

  smf_ids <- smfIds "usersName"

  -- filter out users who have already been added
  --
  forM_
    (filter (\(id_member, _, _, _, _) -> not $ id_member `elem` smf_ids) xs)
    $ \(id_member      :: Int64,
       member_name     :: Text,
       real_name       :: Text,
       email_address   :: Text,
       date_registered :: Int
      ) -> do

      let
        real_name' = Text.filter (\c -> all (/= c) ("#$" :: String)) real_name

      mresult <- findLnIdFromSmfId "usersName" id_member

      case mresult of

        Just _  -> liftIO $ putStrLn $ (Text.unpack member_name) <> " already exists. skipping"

        Nothing -> do

          -- does this user already exist? lookup by email.
          --
          lr <- rd' $ getUserPacks_ByEmail' email_address
          case lr of
            Left err -> error $ show err
            Right (Right (UserPackResponses (UserPackResponse{..}:_))) -> do
              -- Since this user already exists, simply create a mapping for future use
              --
              void $ rw userPackResponseUserId $ postTeamMember_ByOrganizationId [UnixTimestamp $ fromIntegral date_registered] org_id (TeamMemberRequest 0)
              -- case lr' of
              --   Left err -> pure ()
              --   Right _  -> do
              createRedisMap "usersName" id_member userPackResponseUserId
            -- Fatal error
            --
            Right (Left err) -> error $ show err

            -- We need to try and add the user, loop through several times in an attempt to add them
            --
            Right (Right (UserPackResponses [])) -> do

              resetStCounter

              loop $ do

                lift $ incStCounter
                unique_id <- lift $ gets stCounter

                when (unique_id == 10) (break ())

                -- Hacky stuff
                -- Just getting all cases to pass:
                -- Redhawk#23
                -- $ick3nin.v3nd3tta
                --
                let
                  member_name' = Text.filter (\c -> all (/= c) ("#$" :: String)) $ sanitizeLine $ if unique_id == 1 then member_name else (member_name <> (Text.pack $ show unique_id))
                  safe_name'   = toSafeName member_name'

                liftIO $ putStrLn $ show [show id_member, Text.unpack member_name, Text.unpack real_name, Text.unpack email_address, show date_registered]

                -- Attempt to add this user
                --
                e_result <- lift $ rd' (postUser [UnixTimestamp $ fromIntegral date_registered] $
                  UserRequest member_name' real_name' email_address "smf" Nothing)

                case e_result of
                  Left err                    -> error $ show err
                  Right (Left err)            -> do
                    liftIO $ putStrLn $ show err
                    liftIO $ putStrLn "continuing.."
                  Right (Right user_response) -> do
                    lr <- lift $ rw (userResponseId user_response) $ postTeamMember_ByOrganizationId' org_id $ TeamMemberRequest 0
                    case lr of
                      Left err -> error $ show err
                      Right _  -> do
                        lift $ createRedisMap "usersName" id_member (userResponseId user_response)
                        break ()

  pure ()




deleteSmfUsers :: MigrateRWST ()
deleteSmfUsers = do

  user_ids <- lnIds "usersName"

  forM_ user_ids $ \user_id -> do

      if user_id == 1
        then pure ()
        else do
          liftIO $ putStrLn $ show user_id
          void $ rd $ deleteUser' user_id
          deleteRedisMapByLnId "usersName" user_id

  pure ()
