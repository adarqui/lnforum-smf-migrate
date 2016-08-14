{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LN.SMF.Migration.Organization (
  createSmfOrganization,
  deleteSmfOrganization
) where



import           Control.Monad                  (void)
import           Control.Monad.IO.Class         (liftIO)
import           LN.Api
import           LN.SMF.Migration.Connect.Redis
import           LN.SMF.Migration.Control
import           LN.T



createSmfOrganization :: MigrateRWST ()
createSmfOrganization = do

  liftIO $ putStrLn "migrating organizations.."

  org_sid <- asks rOrgSid

  m_org_id <- findLnIdFromSmfId "organizationsName" 0
  case m_org_id of
    Just org_id -> modify (\st->st{ stOrgId = org_id })
    Nothing -> do

      e_result <- rd $ postOrganization [UnixTimestamp $ read "1240177678"] $
        OrganizationRequest org_sid (Just "Smf Forum") "Company" "Location" "andrew.darqui@gmail.com" Membership_Join [] Nothing Public 0 Nothing
      case e_result of
        Left err                       -> error $ show err
        Right OrganizationResponse{..} -> do
          createRedisMap "organizationsName" 0 organizationResponseId
          modify (\st->st{ stOrgId = organizationResponseId})
          return ()



deleteSmfOrganization :: MigrateRWST ()
deleteSmfOrganization = do

  ln_ids <- lnIds "organizationsName"

  case ln_ids of
    [] -> return ()
    (x:_) -> do

      void $ rd $ deleteOrganization' x
      deleteRedisMapByLnId "organizationsName" x
      return ()
