{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LN.SMF.Migration.Forum (
  createLegacyForum,
  deleteLegacyForum
) where



import           Control.Exception              (SomeException (..), try)
import           Control.Monad                  (void)
import           Control.Monad.IO.Class         (liftIO)
import           LN.Api                         (createForum, deleteForum',
                                                 runDefault)
import           LN.SMF.Migration.Connect.Redis
import           LN.SMF.Migration.Control
import           LN.T



createLegacyForum :: MigrateRWST ()
createLegacyForum = do

  liftIO $ putStrLn "migrating forums.."

  org_ids <- lnIds organizationsName

  case org_ids of
    []         -> liftIO $ putStrLn "unable to create forum"
    (org_id:_) -> do

      forum_ids <- lnIds forumsName

      case forum_ids of
        [] -> do
           eresult <- liftIO $ runDefault (createForum [("unix_ts", "1240177678")] org_id $ ForumRequest "adarq-legacy" (Just "Legacy adarq.org forum"))
           case eresult of
             (Left _) -> return ()
             (Right forum_response) -> do
                createRedisMap forumsName 0 (forumResponseId forum_response)
                return ()

        _  -> return ()




deleteLegacyForum :: MigrateRWST ()
deleteLegacyForum = do

  forum_ids <- lnIds forumsName

  case forum_ids of
    (org_id:_) -> do

       void $ liftIO (try (runDefault (deleteForum' org_id)) :: IO (Either SomeException ()))
       deleteRedisMapByLnId forumsName org_id

    _  -> return ()
