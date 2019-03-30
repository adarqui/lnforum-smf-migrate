{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LN.SMF.Migration.ThreadPost (
  createSmfThreadPosts,
  deleteSmfThreadPosts
) where



import           Control.Monad                  (forM_, void)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.RWS
import qualified Data.ByteString.Char8          as BSC
import           Data.Int
import           Data.Text                      (Text)
import           Database.MySQL.Simple

import           LN.Api
import           LN.Sanitize.HTML               (sanitizeHtml)
import           LN.SMF.Migration.Connect.Redis
import           LN.SMF.Migration.Control
import           LN.SMF.Migration.Sanitize
import           LN.T



createSmfThreadPosts :: MigrateRWST ()
createSmfThreadPosts = do

  liftIO $ putStrLn "migrating thread posts.."

  mysql <- asks rMySQL
  limit <- asks rThreadPostsLimit

  [Only thread_posts_count] <- liftIO $ query_ mysql "select count(*) from smf_messages"

  let
    max_limit = min limit thread_posts_count


  forM_ [0..(max_limit `div` 50)] $ \off -> do

    thread_posts <- liftIO $ query mysql "select id_msg, id_topic, poster_time, id_member, subject, body, poster_ip from smf_messages ORDER BY id_msg ASC LIMIT ? OFFSET ?" (50 :: Int, (50 * off) :: Int)

    thread_post_ids <- smfIds "threadPostsName"

    forM_
      (filter (\(id_msg, _, _, _, _, _, _) -> not $ id_msg `elem` thread_post_ids) thread_posts)
      $ \(id_msg     :: Int64,
         id_topic    :: Int64,
         poster_time :: Int64,
         id_member   :: Int64,
         subject     :: Text,
         body        :: Text,
         poster_ip   :: Text
        ) -> do

          let
            body' = sanitizeHtml body

          liftIO $ print $ (id_msg, id_topic, poster_time, id_member, subject, poster_ip)

          mtopic <- findLnIdFromSmfId "threadsName" id_topic
          muser  <- findLnIdFromSmfId "usersName" id_member

          case (mtopic, muser) of
            (Just topic, Just user) -> do
              e_result <- rw user $ postThreadPost_ByThreadId [UnixTimestamp poster_time] topic $
                ThreadPostRequest (Just subject) (PostDataBBCode body') [] [] 0 Nothing Nothing

              case e_result of
                Left err                   -> error $ show err
                Right thread_post_response -> do
                  createRedisMap "threadPostsName" id_msg (threadPostResponseId thread_post_response)

            _ -> do
              liftIO $ print $ "thread_posts: not found: mtopic=" ++ show mtopic ++ ", muser=" ++ show muser
              pure ()

  pure ()



deleteSmfThreadPosts :: MigrateRWST ()
deleteSmfThreadPosts = do

  thread_post_ids <- lnIds "threadPostsName"

  forM_ thread_post_ids $ \thread_post_id -> do

      liftIO $ putStrLn $ show thread_post_id

      e_result <- rd $ getThreadPost' thread_post_id
      case e_result of
        Left err                     -> error $ show err
        Right ThreadPostResponse{..} -> do

          void $ rw threadPostResponseUserId (deleteThreadPost' thread_post_id)
          deleteRedisMapByLnId "threadPostsName" thread_post_id

  pure ()
