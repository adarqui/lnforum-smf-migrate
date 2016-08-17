{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LN.SMF.Migration.Forum (
  createSmfForum,
  deleteSmfForum
) where



import           Control.Monad                  (void)
import           Control.Monad.IO.Class         (liftIO)

import           LN.Api
import           LN.SMF.Migration.Connect.Redis
import           LN.SMF.Migration.Control
import           LN.T



createSmfForum :: MigrateRWST ()
createSmfForum = do

  liftIO $ putStrLn "migrating forums.."

  org_ids <- lnIds "organizationsName"

  case org_ids of
    []         -> error "Unable to create forum"
    (org_id:_) -> do

      forum_ids <- lnIds "forumsName"

      case forum_ids of
        [] -> do
           e_result <- rd (postForum_ByOrganizationId [UnixTimestamp $ read "1240177678"] org_id $
             ForumRequest "migrate" (Just "SMF migrated forum") 20 20 10 10 10 Nothing [] Public 0 Nothing)
           case e_result of
             Left err                -> error $ show err
             Right ForumResponse{..} -> do
                createRedisMap "forumsName" 0 forumResponseId
                pure ()

        _  -> pure ()



deleteSmfForum :: MigrateRWST ()
deleteSmfForum = do

  forum_ids <- lnIds "forumsName"

  case forum_ids of
    (org_id:_) -> do

       void $ rd (deleteForum' org_id)
       deleteRedisMapByLnId "forumsName" org_id

    _  -> pure ()
