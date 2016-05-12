{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LN.SMF.Migration.Organization (
  createLegacyOrganization,
  deleteLegacyOrganization
) where



import           Control.Monad.IO.Class         (liftIO)
import           LN.Api                         (createOrganization,
                                                 deleteOrganization',
                                                 runDefault)
import           LN.SMF.Migration.Connect.Redis
import           LN.SMF.Migration.Control
import           LN.T



createLegacyOrganization :: MigrateRWST ()
createLegacyOrganization = do

  liftIO $ putStrLn "migrating organizations.."

  ln_ids <- lnIds organizationsName

  case ln_ids of
    (_:_) -> liftIO $ putStrLn "unable to add organization"
    [] -> do

      eresult <- liftIO $ runDefault (createOrganization [("unix_ts", "1240177678")] $ OrganizationRequest "legacy" (Just "Legacy Forum") "ADARQ" "FL" "andrew.darqui@gmail.com")
      case eresult of
        (Left err) -> liftIO $ putStrLn err
        (Right org_response) -> do
          createRedisMap organizationsName 0 (organizationResponseId org_response)
          return ()



deleteLegacyOrganization :: MigrateRWST ()
deleteLegacyOrganization = do

  ln_ids <- lnIds organizationsName

  case ln_ids of
    [] -> return ()
    (x:_) -> do

      liftIO $ runDefault (deleteOrganization' x)
      deleteRedisMapByLnId organizationsName x
      return ()
