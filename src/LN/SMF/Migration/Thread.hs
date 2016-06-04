{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LN.SMF.Migration.Thread (
  createLegacyThreads,
  deleteLegacyThreads
) where



import           Haskell.Api.Helpers
import           Control.Exception              (SomeException (..), try)
import           Control.Monad                  (forM_)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.RWS
import qualified Data.ByteString.Char8          as BSC
import           Data.Int
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text)
import qualified Data.Text                      as T (pack)
import           Database.MySQL.Simple
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

  threads <- liftIO $ query mysql "select smf_topics.id_topic, smf_topics.is_sticky, smf_topics.id_board, smf_topics.id_member_started, smf_topics.locked, smf_messages.subject, smf_messages.poster_time, smf_messages.poster_ip from smf_topics INNER JOIN smf_messages ON smf_topics.id_first_msg=smf_messages.id_msg where smf_topics.id_board != 79 LIMIT ?" (Only limit)

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

        case (mboard, mtopic, muser) of

          (Nothing, _, _) -> return ()

          (_, _, Nothing) -> return ()

          ((Just board), Nothing, (Just user)) -> do
            -- doesn't exist, created it
            --
            eresult <- liftIO $ rw (postThread_ByBoardId [UnixTimestamp $ fromIntegral poster_time] board $
              ThreadRequest (sanitizeHtml subject) Nothing is_sticky locked Nothing Nothing []) (BSC.pack $ show user)

            case eresult of
              (Left err) -> do
                -- HACKJOB, try and create the thread again, but this time with a modified title
                liftIO $ print err
                eresult <- liftIO $ rw (postThread_ByBoardId [UnixTimestamp $ fromIntegral poster_time] board $
                  ThreadRequest (sanitizeHtml $ subject <> (T.pack $ show poster_time)) Nothing is_sticky locked Nothing Nothing []) (BSC.pack $ show user)

                case eresult of
                  (Left err) -> liftIO $ print err
                  (Right thread_response) -> do
                    createRedisMap "threadsName" id_topic (threadResponseId thread_response)

              (Right thread_response) -> do
                createRedisMap "threadsName" id_topic (threadResponseId thread_response)

          (_, _, _) -> return ()

    )

  return ()



deleteLegacyThreads :: MigrateRWST ()
deleteLegacyThreads = do

  thread_ids <- lnIds "threadsName"

  forM_ thread_ids
    (\thread_id -> do

      liftIO $ putStrLn $ show thread_id

      get_result <- liftIO (try (rd (getThread' thread_id)) :: IO (Either SomeException (Either ApiError ThreadResponse)))
      case get_result of
        Left err -> liftIO $ print err
        Right (Left err) ->  liftIO $ print err
        Right (Right thread_response) -> do

          del_result <- liftIO (try (rw (deleteThread' thread_id) (BSC.pack $ show $ threadResponseUserId thread_response)) :: IO (Either SomeException (Either ApiError ())))
          case del_result of
            Left err -> liftIO $ print err
            Right _ -> do
              deleteRedisMapByLnId "threadsName" thread_id

      )

  return ()
