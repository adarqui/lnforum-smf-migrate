{-# LANGUAGE RankNTypes #-}

module LN.SMF.Migration (
  module A,
  migrateSMF,
  unMigrateSMF,
  migrateRWST
) where



import           Control.Monad                   (void)
import           Control.Monad.Trans.RWS
import           Data.ByteString                 (ByteString)
import           Data.String.Conversions         (cs)
import           Data.Text                       (Text)

import           LN.SMF.Migration.Board          as A
import           LN.SMF.Migration.Connect        as A
import           LN.SMF.Migration.Control        as A
import           LN.SMF.Migration.Forum          as A
import           LN.SMF.Migration.Organization   as A
import           LN.SMF.Migration.Pm             as A
import           LN.SMF.Migration.Sanitize       as A
import           LN.SMF.Migration.Thread         as A
import           LN.SMF.Migration.ThreadPost     as A
import           LN.SMF.Migration.ThreadPostLike as A
import           LN.SMF.Migration.User           as A



-- | Migrate an smf forum to leuro
--
-- We should be able to run this multiple times without problem.
--
migrateSMF :: Text -> Text -> Text -> Text -> Text ->  Int -> IO ()
migrateSMF super_key org_sid redis_host mysql_host api_host limit = migrateRWST super_key org_sid redis_host mysql_host api_host limit go
  where
  go = do
    createSuperUser
    createSmfOrganization
    createSmfForum
    createSmfBoards
    -- createUserProfiles
    -- createUserSettings
    createSmfUsers
    createSmfThreads
    createSmfThreadPosts
--    createSmfThreadPostLikes
--    createSmfPms




-- | Un-migrate smf data from leuro
--
-- We should be able to run this multiple times without problem.
--
unMigrateSMF :: Text -> Text -> Text -> Text -> Text -> IO ()
unMigrateSMF super_key org_sid redis_host mysql_host api_host = migrateRWST super_key org_sid redis_host mysql_host api_host 0 go
  where
  go = do
    -- deleteUserProfiles
    -- deleteUserSettings
--    deleteSmfPms
--    deleteSmfThreadPostLikes
    deleteSmfThreadPosts
    deleteSmfThreads
    deleteSmfBoards
    deleteSmfForum
    deleteSmfOrganization
    removeSuperUser
    deleteSmfUsers



migrateRWST :: forall w a. Text -> Text -> Text -> Text -> Text -> Int -> RWST MigrateReader w MigrateState IO a -> IO ()
migrateRWST super_key org_sid redis_host mysql_host api_host limit go = do
  mysql <- connectMySQL mysql_host
  redis <- connectRedis redis_host
  let api_opts = apiOpts { apiUrl = api_host, apiKey = Just $ cs super_key }
  void $ evalRWST go (MigrateReader (cs super_key) org_sid redis_host redis mysql_host mysql api_host api_opts limit) (MigrateState 0)
  return ()
