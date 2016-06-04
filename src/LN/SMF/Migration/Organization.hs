{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LN.SMF.Migration.Organization (
  createLegacyOrganization,
  deleteLegacyOrganization
) where



import           Haskell.Api.Helpers
import           Control.Monad.IO.Class         (liftIO)
import           LN.Api
import           LN.SMF.Migration.Connect.Redis
import           LN.SMF.Migration.Control
import           LN.T



createLegacyOrganization :: MigrateRWST ()
createLegacyOrganization = do

  liftIO $ putStrLn "migrating organizations.."

  ln_ids <- lnIds "organizationsName"

  case ln_ids of
    (_:_) -> liftIO $ putStrLn "unable to add organization"
    [] -> do

      eresult <- liftIO $ rd (postOrganization [UnixTimestamp $ read "1240177678"] $ OrganizationRequest "legacy" (Just "Legacy Forum") "ADARQ" "FL" "andrew.darqui@gmail.com" Membership_Join [] Nothing Public)
      case eresult of
        (Left err) -> liftIO $ print err
        (Right org_response) -> do
          createRedisMap "organizationsName" 0 (organizationResponseId org_response)
          return ()



deleteLegacyOrganization :: MigrateRWST ()
deleteLegacyOrganization = do

  ln_ids <- lnIds "organizationsName"

  case ln_ids of
    [] -> return ()
    (x:_) -> do

      liftIO $ rd (deleteOrganization' x)
      deleteRedisMapByLnId "organizationsName" x
      return ()
