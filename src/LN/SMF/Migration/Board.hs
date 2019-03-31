{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LN.SMF.Migration.Board (
  createSmfBoards,
  deleteSmfBoards
) where



import           Control.Monad                  (forM_, void)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.RWS
import           Data.Int
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Database.MySQL.Simple

import           LN.Api
import qualified LN.Api.String as ApiS
import           LN.Sanitize.HTML               (sanitizeHtml)
import LN.Sanitize.Internal
import           LN.SMF.Migration.Connect.Redis
import           LN.SMF.Migration.Control
import           LN.SMF.Migration.Sanitize
import           LN.T



createSmfBoards :: MigrateRWST ()
createSmfBoards = do

  liftIO $ putStrLn "migrating boards.."

  mysql <- asks rMySQL

  categories <- liftIO $ query_ mysql "select id_cat, name from smf_categories ORDER BY id_cat ASC"

  board_ids <- smfIds "boardsName"

  liftIO $ putStrLn $ show categories
  liftIO $ putStrLn $ show board_ids

  forM_
    (filter (\(id_cat, _) -> not $ id_cat `elem` board_ids) categories)
    $ \(id_cat :: Int64, name :: Text) -> do

      liftIO $ print $ (id_cat, name)

      lr <- rd' $ ApiS.getBoardPack' $ toSafeUrl name
      case lr of
        Left err -> error $ show err
        Right (Right BoardPackResponse{..}) -> do
          let BoardResponse{..} = boardPackResponseBoard
          createRedisMap "boardsName" id_cat boardResponseId

        Right _ -> do
          e_result <- rd (postBoard_ByForumId [UnixTimestamp $ read "1240177678"] defaultForumId $ BoardRequest {
            boardRequestDisplayName = name,
            boardRequestDescription = Nothing,
            boardRequestBoardType = FixMe,
            boardRequestActive = True,
            boardRequestIsAnonymous = False,
            boardRequestCanCreateBoards = False,
            boardRequestCanCreateThreads = True,
            boardRequestVisibility = Public,
            boardRequestIcon = Nothing,
            boardRequestTags = [],
            boardRequestGuard = 0
          })
          case e_result of
            Left err                -> error $ show err
            Right BoardResponse{..} -> do
              createRedisMap "boardsName" id_cat boardResponseId


  forM_
    categories
    $ \(cat_id :: Int64, _) -> do

      boards <- liftIO $ query mysql "select id_board, id_parent, id_cat, name, description from smf_boards where id_cat = ?" (Only cat_id)

      forM_
        (filter (\(id_board, _, _, _, _) -> not $ id_board `elem` board_ids) boards)
        $ \(id_board  :: Int64,
           id_parent  :: Int64,
           id_cat     :: Int64,
           board_name :: Text,
           board_desc :: Text
          ) -> do

          let desc = if board_desc == ""
                        then Nothing
                        else Just $ sanitizeHtml $ Text.take 132 board_desc

          liftIO $ print (id_board, board_name, board_desc)

          mresult <- findLnIdFromSmfId "boardsName" id_board

          lr <- rd' $ ApiS.getBoardPack' $ toSafeUrl board_name
          case lr of
            Left err -> error $ show err
            Right (Right BoardPackResponse{..}) -> do
              let BoardResponse{..} = boardPackResponseBoard
              createRedisMap "boardsName" id_board boardResponseId

            Right _ -> do
              e_result <- rd (postBoard [UnixTimestamp $ read "1240177678"] $
                BoardRequest {
                  boardRequestDisplayName = sanitizeHtml board_name,
                  boardRequestDescription = desc,
                  boardRequestBoardType = FixMe,
                  boardRequestActive = True,
                  boardRequestIsAnonymous = False,
                  boardRequestCanCreateBoards = False,
                  boardRequestCanCreateThreads = True,
                  boardRequestVisibility = Public,
                  boardRequestIcon = Nothing,
                  boardRequestTags = [],
                  boardRequestGuard = 0
                })

              case e_result of
                Left err -> error $ show err
                Right child_board_response -> do
                  createRedisMap "boardsName" id_board (boardResponseId child_board_response)

  pure ()



deleteSmfBoards :: MigrateRWST ()
deleteSmfBoards = do

  board_ids <- lnIds "boardsName"

  forM_ board_ids $ \board_id -> do
    liftIO $ putStrLn $ show board_id
    void $ rd' (deleteBoard' board_id)
    deleteRedisMapByLnId "boardsName" board_id

  pure ()
