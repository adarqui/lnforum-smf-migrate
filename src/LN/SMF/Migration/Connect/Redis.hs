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
import qualified Data.ByteString.Char8    as B
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
keyPrefix = "migrate:"



smfKey :: ByteString
smfKey = "smf"



lnKey :: ByteString
lnKey = "ln"



notKey :: ByteString -> ByteString
notKey "ln" = "smf"
notKey "smf" = "ln"
notKey _ = error "key should be smf or ln"



createRedisMap :: ByteString -> Int64 -> Int64 -> MigrateRWST ()
createRedisMap route smf_id ln_id = do

  redis <- asks rRedis

  void $ liftIO $ runRedis redis $
    set
      (keyPrefix <> smfKey <> ":" <> route <> ":" <> (B.pack $ show smf_id))
      (B.pack $ show ln_id)

  void $ liftIO $ runRedis redis $
    set
      (keyPrefix <> lnKey <> ":" <> route <> ":" <> (B.pack $ show ln_id))
      (B.pack $ show smf_id)

  return ()



deleteRedisMapByLnId :: ByteString -> Int64 -> MigrateRWST ()
deleteRedisMapByLnId route ln_id = do

  redis <- asks rRedis

  esmf_id <- liftIO $ runRedis redis $
    R.get
      (keyPrefix <> lnKey <> ":" <> route <> ":" <> (B.pack $ show ln_id))

  case esmf_id of
    (Left _) -> return ()
    (Right Nothing) -> return ()
    (Right (Just smf_id)) -> do
      void $ liftIO $ runRedis redis $ do
        del [
          (keyPrefix <> lnKey <> ":" <> route <> ":" <> (B.pack $ show ln_id)),
          (keyPrefix <> smfKey <> ":" <> route <> ":" <> smf_id)]

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

  redis <- asks rRedis

  eresult <- liftIO $ runRedis redis $
    keys (keyPrefix <> ln_or_smf <> ":" <> route <> ":*")

  case eresult of
    (Left _) -> return []
    (Right xs) -> return $ map getId xs

  where
  getId key = let (_,b) = B.breakEnd (== ':') key in read $ B.unpack b




findSmfIdFromLnId :: ByteString -> Int64 -> MigrateRWST (Maybe Int64)
findSmfIdFromLnId = findIdFrom lnKey



findLnIdFromSmfId :: ByteString -> Int64 -> MigrateRWST (Maybe Int64)
findLnIdFromSmfId = findIdFrom smfKey



findIdFrom :: ByteString -> ByteString -> Int64 -> MigrateRWST (Maybe Int64)
findIdFrom ln_or_smf route some_id = do

  redis <- asks rRedis

  eresult <- liftIO $ runRedis redis $
    R.get (keyPrefix <> ln_or_smf <> ":" <> route <> ":" <> (B.pack $ show some_id))

  case eresult of
    (Left _) -> return Nothing
    (Right Nothing) -> return Nothing
    (Right (Just v)) -> return $ Just $ read $ B.unpack v



getId :: ByteString -> Int64 -> MigrateRWST (Maybe Int64)
getId route some_id = do
  redis <- asks rRedis
  eresult <- liftIO $ runRedis redis $ R.get (keyPrefix <> route <> ":" <> (B.pack $ show some_id))
  case eresult of
    (Left _) -> return Nothing
    (Right Nothing) -> return Nothing
    (Right (Just v)) -> return $ Just $ read $ B.unpack v
