{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LN.SMF.Migration.Thread (
  createLegacyThreads,
  deleteLegacyThreads
) where



import           Control.Break                  (loop, break, lift)
import           Control.Monad                  (forM_, when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.RWS
import qualified Data.ByteString.Char8          as BSC
import           Data.Int
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text)
import qualified Data.Text                      as T (pack)
import           Database.MySQL.Simple
import           Prelude               hiding   (break)
import           LN.Api
import           LN.SMF.Migration.Connect.Redis
import           LN.SMF.Migration.Control
import           LN.SMF.Migration.Sanitize
import           LN.T



createLegacyThreads :: MigrateRWST ()
createLegacyThreads = do

  liftIO $ putStrLn "migrating threads.."

  mysql <- asks rMySQL
  limit <- asks rLimit

--  [Only threads_count] <- liftIO $ query_ mysql "select count(*) from smf_topics.id_topic, smf_topics.is_sticky, smf_topics.id_board, smf_topics.id_member_started, smf_topics.locked, smf_messages.subject, smf_messages.poster_time, smf_messages.poster_ip from smf_topics INNER JOIN smf_messages ON smf_topics.id_first_msg=smf_messages.id_msg where smf_topics.id_board != 79"
  [Only threads_count] <- liftIO $ query_ mysql "select count(*) from smf_topics"

  let
    max_limit = min limit threads_count


  forM_ [0..(max_limit `div` 50)] (\off -> do

    threads <- liftIO $ query mysql "select smf_topics.id_topic, smf_topics.is_sticky, smf_topics.id_board, smf_topics.id_member_started, smf_topics.locked, smf_messages.subject, smf_messages.poster_time, smf_messages.poster_ip from smf_topics INNER JOIN smf_messages ON smf_topics.id_first_msg=smf_messages.id_msg where smf_topics.id_board != 79 LIMIT ? OFFSET ?" (50 :: Int, (50 * off) :: Int)

    thread_ids <- smfIds "threadsName"

  --  forum_ids <- head <$> lnIds "forums"

    forM_
      (filter (\(id_topic, _, _, _, _, _, _, _) -> not $ id_topic `elem` thread_ids) threads)
      (\(
          id_topic :: Int64,
          is_sticky :: Bool,
          id_board :: Int64,
          id_member_started :: Int64,
          locked :: Bool,
          subject :: Text,
          poster_time :: Int64,
          poster_ip :: Text
        ) -> do

          liftIO $ print (id_topic, is_sticky, id_board, id_member_started, locked, subject, poster_time, poster_ip)

          mboard <- findLnIdFromSmfId "boardsName" id_board
          mtopic <- findLnIdFromSmfId "threadsName" id_topic
          muser  <- findLnIdFromSmfId "usersName" id_member_started

          liftIO $ print (mboard, mtopic, muser)

          put $ MigrateState 0

          loop $ do

            lift $ modify (\(MigrateState n) -> MigrateState (n+1))
            unique_id <- lift $ gets runMigrateState

            when (unique_id == 10) (break ())

            let subject' = if unique_id == 1 then subject else (subject <> (T.pack $ show unique_id))

            case (mboard, mtopic, muser) of

              (Nothing, _, _) -> break ()

              (_, _, Nothing) -> break ()

              ((Just board), Nothing, (Just user)) -> do
                -- doesn't exist, created it
                --
                e_result <- lift $ rw' (postThread_ByBoardId [UnixTimestamp $ fromIntegral poster_time] board $
                  ThreadRequest (sanitizeHtml subject') Nothing is_sticky locked Nothing Nothing [] 0 Nothing) (BSC.pack $ show user)

                case e_result of
                  (Left err)                      -> liftIO $ print err
                  (Right (Left err))              -> liftIO $ print err
                  (Right (Right thread_response)) -> do
                    lift $ createRedisMap "threadsName" id_topic (threadResponseId thread_response)
                    break ()

              (_, _, _) -> break ()

      )
   )

  return ()



deleteLegacyThreads :: MigrateRWST ()
deleteLegacyThreads = do

  thread_ids <- lnIds "threadsName"

  forM_ thread_ids
    (\thread_id -> do

      liftIO $ putStrLn $ show thread_id

      get_result <- rd' $ getThread' thread_id
      case get_result of
        Left err -> liftIO $ print err
        Right (Left err) ->  liftIO $ print err
        Right (Right thread_response) -> do

          del_result <- rw' (deleteThread' thread_id) (BSC.pack $ show $ threadResponseUserId thread_response)
          case del_result of
            Left err -> liftIO $ print err
            Right _ -> do
              deleteRedisMapByLnId "threadsName" thread_id

      )

  return ()
