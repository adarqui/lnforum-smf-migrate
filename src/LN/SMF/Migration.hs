{-# LANGUAGE RankNTypes #-}

module LN.SMF.Migration (
  module A,
  migrateSMF,
  unMigrateSMF,
  migrateRWST
) where



import           Control.Monad                   (void)
import           Control.Monad.Trans.RWS
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
migrateSMF :: Int -> IO ()
migrateSMF limit = migrateRWST limit go
  where
  go = do
    createSuperUser
    createLegacyOrganization
    createLegacyForum
    createLegacyBoards
    -- createUserProfiles
    -- createUserSettings
    createLegacyUsers
    createLegacyThreads
    createLegacyThreadPosts
--    createLegacyThreadPostLikes
--    createLegacyPms




-- | Un-migrate smf data from leuro
--
-- We should be able to run this multiple times without problem.
--
unMigrateSMF :: IO ()
unMigrateSMF = migrateRWST 0 go
  where
  go = do
    -- deleteUserProfiles
    -- deleteUserSettings
--    deleteLegacyPms
--    deleteLegacyThreadPostLikes
    deleteLegacyThreadPosts
    deleteLegacyThreads
    deleteLegacyBoards
    deleteLegacyForum
    deleteLegacyOrganization
    removeSuperUser
    deleteLegacyUsers



migrateRWST :: forall w a. Int -> RWST MigrateReader w MigrateState IO a -> IO ()
migrateRWST limit go = do
  mysql <- connectMySQL
  redis <- connectRedis
  void $ evalRWST go (MigrateReader redis mysql limit) (MigrateState False)
  return ()
