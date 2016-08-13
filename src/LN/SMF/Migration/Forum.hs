{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LN.SMF.Migration.Forum (
  createLegacyForum,
  deleteLegacyForum
) where



import           Control.Monad                  (void)
import           Control.Monad.IO.Class         (liftIO)
import           LN.Api
import           LN.SMF.Migration.Connect.Redis
import           LN.SMF.Migration.Control
import           LN.T



createLegacyForum :: MigrateRWST ()
createLegacyForum = do

  liftIO $ putStrLn "migrating forums.."

  org_ids <- lnIds "organizationsName"

  case org_ids of
    []         -> liftIO $ putStrLn "unable to create forum"
    (org_id:_) -> do

      forum_ids <- lnIds "forumsName"

      case forum_ids of
        [] -> do
           e_result <- rd (postForum_ByOrganizationId [UnixTimestamp $ read "1240177678"] org_id $
             ForumRequest "adarq-legacy" (Just "Legacy adarq.org forum") 20 20 10 10 10 Nothing [] Public 0 Nothing)
           case e_result of
             (Left _) -> return ()
             (Right forum_response) -> do
                createRedisMap "forumsName" 0 (forumResponseId forum_response)
                return ()

        _  -> return ()




deleteLegacyForum :: MigrateRWST ()
deleteLegacyForum = do

  forum_ids <- lnIds "forumsName"

  case forum_ids of
    (org_id:_) -> do

       void $ rd (deleteForum' org_id)
       deleteRedisMapByLnId "forumsName" org_id

    _  -> return ()
