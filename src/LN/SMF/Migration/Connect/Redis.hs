{-# LANGUAGE OverloadedStrings #-}

module LN.SMF.Migration.Connect.Redis (
  connectRedis,
  createRedisMap,
  deleteRedisMapByLnId,
  smfKey,
  lnKey,
  notKey,
  smfKeys,
  smfIds,
  lnKeys,
  lnIds,
  findSmfIdFromLnId,
  findLnIdFromSmfId,
  getId
) where



import qualified Data.Text as T (unpack)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as BSC
import Data.String.Conversions (cs)
import           Data.Int
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import           Database.Redis           hiding (get)
import qualified Database.Redis           as R
import           LN.SMF.Migration.Control



connectRedis :: Text -> IO Connection
connectRedis redis_host = do
  connect info'
  where
  info' = defaultConnectInfo {
    connectHost = T.unpack redis_host
  }



keyPrefix :: ByteString
keyPrefix = "migrate"



smfKey :: ByteString
smfKey = "smf"



lnKey :: ByteString
lnKey = "ln"



notKey :: ByteString -> ByteString
notKey "ln" = "smf"
notKey "smf" = "ln"
notKey _ = error "key should be smf or ln"



buildSmfKey :: [ByteString] -> ByteString
buildSmfKey = buildKey smfKey



buildLnKey :: [ByteString] -> ByteString
buildLnKey = buildKey lnKey



buildKey :: ByteString -> [ByteString] -> ByteString
buildKey key segments = BSC.intercalate ":" (keyPrefix : key : segments)



createRedisMap :: ByteString -> Int64 -> Int64 -> MigrateRWST ()
createRedisMap route smf_id ln_id = do

  redis   <- asks rRedis
  org_sid <- asks rOrgSid

  void $ liftIO $ runRedis redis $
    set
      (buildSmfKey [cs org_sid, route, cs $ show smf_id])
      (BSC.pack $ show ln_id)

  void $ liftIO $ runRedis redis $
    set
      (buildLnKey [cs org_sid, route, cs $ show ln_id])
      (BSC.pack $ show smf_id)

  return ()



deleteRedisMapByLnId :: ByteString -> Int64 -> MigrateRWST ()
deleteRedisMapByLnId route ln_id = do

  redis   <- asks rRedis
  org_sid <- asks rOrgSid

  esmf_id <- liftIO $ runRedis redis $
    R.get
      (buildLnKey [cs org_sid, route, cs $ show ln_id])

  case esmf_id of
    (Left _) -> return ()
    (Right Nothing) -> return ()
    (Right (Just smf_id)) -> do
      void $ liftIO $ runRedis redis $ do
        del [ (buildLnKey [cs org_sid, route, cs $ show ln_id])
            , (buildSmfKey [cs org_sid, route, smf_id])
            ]

  return ()



smfKeys :: ByteString -> MigrateRWST [ByteString]
smfKeys _ = undefined



smfIds :: ByteString -> MigrateRWST [Int64]
smfIds = findIds smfKey



lnKeys :: ByteString -> MigrateRWST [ByteString]
lnKeys _ = undefined



lnIds :: ByteString -> MigrateRWST [Int64]
lnIds = findIds lnKey




findIds :: ByteString -> ByteString -> MigrateRWST [Int64]
findIds ln_or_smf route = do

  redis   <- asks rRedis
  org_sid <- asks rOrgSid

  eresult <- liftIO $ runRedis redis $
    keys (buildKey ln_or_smf [cs org_sid, route, "*"])

  case eresult of
    (Left _) -> return []
    (Right xs) -> return $ map getId xs

  where
  getId key = let (_,b) = BSC.breakEnd (== ':') key in read $ BSC.unpack b




findSmfIdFromLnId :: ByteString -> Int64 -> MigrateRWST (Maybe Int64)
findSmfIdFromLnId = findIdFrom lnKey



findLnIdFromSmfId :: ByteString -> Int64 -> MigrateRWST (Maybe Int64)
findLnIdFromSmfId = findIdFrom smfKey



findIdFrom :: ByteString -> ByteString -> Int64 -> MigrateRWST (Maybe Int64)
findIdFrom ln_or_smf route some_id = do

  redis   <- asks rRedis
  org_sid <- asks rOrgSid

  eresult <- liftIO $ runRedis redis $
    R.get (buildKey ln_or_smf [cs org_sid, route, cs $ show some_id])

  case eresult of
    (Left _) -> return Nothing
    (Right Nothing) -> return Nothing
    (Right (Just v)) -> return $ Just $ read $ BSC.unpack v



getId :: ByteString -> Int64 -> MigrateRWST (Maybe Int64)
getId route some_id = do
  redis   <- asks rRedis
  org_sid <- asks rOrgSid
  eresult <- liftIO $ runRedis redis $ R.get (buildKey "none" [cs org_sid, route, cs $ show some_id])
  case eresult of
    (Left _) -> return Nothing
    (Right Nothing) -> return Nothing
    (Right (Just v)) -> return $ Just $ read $ BSC.unpack v
