{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LN.SMF.Migration.Pm (
  createLegacyPms,
  deleteLegacyPms
) where



import           Haskell.Api.Helpers
import           Data.Monoid ((<>))
import           Control.Exception              (SomeException (..), try)
import           Control.Monad                  (forM_)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.RWS
import qualified Data.ByteString.Char8          as BSC
import           Data.Int
import           Data.Text                      (Text)
import           Database.MySQL.Simple
import           LN.Api
import           LN.SMF.Migration.Connect.Redis
import           LN.SMF.Migration.Control
import           LN.SMF.Migration.Sanitize
import           LN.T



createLegacyPms :: MigrateRWST ()
createLegacyPms = do

  liftIO $ putStrLn "migrating personal messages.."

  mysql <- asks rMySQL
  limit <- asks rLimit

  pms <- liftIO $ query mysql "select smf_personal_messages.id_pm, smf_personal_messages.id_member_from, smf_personal_messages.deleted_by_sender, smf_personal_messages.msgtime, smf_personal_messages.subject, smf_personal_messages.body, smf_pm_recipients.id_member, smf_pm_recipients.bcc, smf_pm_recipients.is_read, smf_pm_recipients.deleted, smf_pm_recipients.is_new from smf_personal_messages INNER JOIN smf_pm_recipients ON smf_personal_messages.id_pm=smf_pm_recipients.id_pm LIMIT ?" (Only limit)

  pm_ids <- smfIds "pmsName"

--  forum_ids <- head <$> lnIds "forums"

  forM_
    (filter (\(id_pm, _, _, _, _, _, _, _, _, _, _) -> not $ id_pm `elem` pm_ids) pms)
    (\(
        id_pm :: Int64,
        id_member_from :: Int64,
        deleted_by_sender :: Bool,
        msgtime :: Int64,
        subject :: Text,
        body :: Text,
        id_member :: Int64,
        bcc :: Int64,
        is_read :: Bool,
        is_deleted :: Bool,
        is_new :: Bool
      ) -> do

        liftIO $ print (id_pm, id_member_from, deleted_by_sender, msgtime, subject, body, id_member, bcc, is_read, is_deleted, is_new)

        muser_from <- findLnIdFromSmfId "usersName" id_member_from
        muser_to <- findLnIdFromSmfId "usersName" id_member

        liftIO $ print (muser_from, muser_to)

        case (muser_from, muser_to) of

          (Nothing, _) -> return ()

          (_, Nothing) -> return ()

          ((Just user_from), (Just user_to)) -> do
            -- doesn't exist, created it
            --
            eresult <- liftIO $ rw (postPm_ByUserId [UnixTimestamp $ fromIntegral msgtime] user_to $
              PmRequest (sanitizeHtml subject) (sanitizeHtml body) 0) (BSC.pack $ show user_from)

            case eresult of
              (Left err) -> liftIO $ print err
              (Right pm_response) -> do
                createRedisMap "pmsName" id_pm (pmResponseId pm_response)
                createRedisMap ("pmsName" <> "_users") (pmResponseId pm_response) (pmResponseUserId pm_response)


          (_, _) -> return ()

    )

  return ()



deleteLegacyPms :: MigrateRWST ()
deleteLegacyPms = do

  pm_ids <- lnIds "pmsName"

  forM_ pm_ids
    (\pm_id -> do

      liftIO $ putStrLn $ show pm_id

      mpm_user_id <- getId ("pmsName" <> "_users") pm_id
      case mpm_user_id of
        Nothing -> return ()
        Just pm_user_id -> do

          del_result <- liftIO (try (rw (deletePm' pm_id) (BSC.pack $ show pm_user_id)) :: IO (Either SomeException (Either ApiError ())))
          case del_result of
            Left err -> liftIO $ putStrLn $ show err
            Right _ -> do
              deleteRedisMapByLnId "pmsName" pm_id
              deleteRedisMapByLnId ("pmsName" <> "_users") pm_id
      )

  return ()
