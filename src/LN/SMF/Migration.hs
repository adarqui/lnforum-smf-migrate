{-# LANGUAGE RankNTypes #-}

module LN.SMF.Migration (
  module A,
  MigrateConfig(..),
  defaultMigrateConfig,
  defaultUnmigrateConfig,
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
import           LN.SMF.Migration.Sanitize       as A
import           LN.SMF.Migration.Thread         as A
import           LN.SMF.Migration.ThreadPost     as A
import           LN.SMF.Migration.ThreadPostLike as A
import           LN.SMF.Migration.User           as A



data MigrateConfig = MigrateConfig {
  mSuperKey         :: Text,
  mRedisHost        :: Text,
  mMysqlHost        :: Text,
  mApiHost          :: Text,
  mUsersLimit       :: Int,
  mBoardsLimit      :: Int,
  mThreadsLimit     :: Int,
  mThreadPostsLimit :: Int,
  mThreadPostLikesLimit :: Int
}

defaultMigrateConfig :: MigrateConfig
defaultMigrateConfig = MigrateConfig {
  mSuperKey = "x",
  mRedisHost = "127.0.0.1",
  mMysqlHost = "127.0.0.1",
  mApiHost = "http://local.adarq.org:1682",
  mUsersLimit = 1000000,
  mBoardsLimit = 1000000,
  mThreadsLimit = 1000000,
  mThreadPostsLimit = 1000000,
  mThreadPostLikesLimit = 1000000
}

defaultUnmigrateConfig :: MigrateConfig
defaultUnmigrateConfig = defaultMigrateConfig



-- | Migrate an smf forum to leuro
--
-- We should be able to run this multiple times without problem.
--
migrateSMF :: MigrateConfig -> IO ()
migrateSMF migrate_config = migrateRWST migrate_config go
  where
  go = do
    createSmfUsers
    createSmfBoards
    -- createUserProfiles
    -- createUserSettings
    createSmfThreads
    createSmfThreadPosts
--    createSmfThreadPostLikes
--    createSmfPms




-- | Un-migrate smf data from leuro
--
-- We should be able to run this multiple times without problem.
--
unMigrateSMF :: MigrateConfig -> IO ()
unMigrateSMF migrate_config = migrateRWST migrate_config go
  where
  go = do
    -- deleteUserProfiles
    -- deleteUserSettings
--    deleteSmfPms
--    deleteSmfThreadPostLikes
    deleteSmfThreadPosts
    deleteSmfThreads
    deleteSmfBoards



migrateRWST :: forall w a. MigrateConfig -> RWST MigrateReader w MigrateState IO a -> IO ()
migrateRWST MigrateConfig{..} go = do
  mysql <- connectMySQL mMysqlHost
  redis <- connectRedis mRedisHost
  let api_opts = apiOpts { apiUrl = mApiHost, apiKey = Just $ cs mSuperKey }
  let migrate_reader = MigrateReader {
    rSuperKey = cs mSuperKey,
    rRedisHost = mRedisHost,
    rRedis = redis,
    rMySQLHost = mMysqlHost,
    rMySQL = mysql,
    rApiHost = mApiHost,
    rApiOpts = api_opts,
    rUsersLimit = mUsersLimit,
    rBoardsLimit = mBoardsLimit,
    rThreadsLimit = mThreadsLimit,
    rThreadPostsLimit = mThreadPostsLimit,
    rThreadPostLikesLimit = mThreadPostLikesLimit
  }
  void $ evalRWST go migrate_reader (MigrateState 0)
  return ()
