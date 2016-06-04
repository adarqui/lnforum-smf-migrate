{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LN.SMF.Migration.Board (
  createLegacyBoards,
  deleteLegacyBoards
) where



import           Haskell.Api.Helpers
import           Control.Exception              (SomeException (..), try)
import           Control.Monad                  (forM_, void)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.RWS
import           Data.Int
import           Data.Text                      (Text)
import           Database.MySQL.Simple
import           LN.Api
import           LN.SMF.Migration.Connect.Redis
import           LN.SMF.Migration.Control
import           LN.SMF.Migration.Sanitize
import           LN.T



createLegacyBoards :: MigrateRWST ()
createLegacyBoards = do

  liftIO $ putStrLn "migrating boards.."

  mysql <- asks rMySQL

  categories <- liftIO $ query_ mysql "select id_cat, name from smf_categories"

  board_ids <- smfIds "boardsName"

  forum_ids <- lnIds "forumsName"

  case forum_ids of
    [] -> liftIO $ putStrLn "Forum does not exist."
    (forum_id:_) -> do

      forM_
        (filter (\(id_cat, _) -> not $ id_cat `elem` board_ids) categories)
        (\(id_cat :: Int64, name :: Text) -> do

          liftIO $ print $ (id_cat, name)

          eresult <- liftIO $ rd (postBoard_ByForumId [UnixTimestamp $ read "1240177678"] forum_id $ BoardRequest name Nothing Nothing [])
          case eresult of
            (Left err)             -> liftIO $ print err
            (Right board_response) -> do
              createRedisMap "boardsName" id_cat (boardResponseId board_response)
        )


      forM_
        categories
        (\(id_cat :: Int64, _) -> do

          boards <- liftIO $ query mysql "select id_board, id_cat, name, description from smf_boards where id_cat = ?" (Only id_cat)

          forM_
            (filter (\(id_board, _, _, _) -> not $ id_board `elem` board_ids) boards)
            (\(id_board :: Int64,
               id_parent :: Int64,
               board_name :: Text,
               board_desc :: Text
              ) -> do

              liftIO $ print (id_board, id_parent, board_name, board_desc)

              mresult <- findLnIdFromSmfId "boardsName" id_parent

              case mresult of
                Nothing -> return () -- doesn't exist??
                (Just parent) -> do

                  eresult <- liftIO $ rd (postBoard_ByBoardId [UnixTimestamp $ read "1240177678"] parent $
                    BoardRequest (sanitizeHtml board_name) (Just $ sanitizeHtml board_desc) Nothing [])

                  case eresult of
                    (Left err) -> liftIO $ print err
                    (Right child_board_response) -> do
                      createRedisMap "boardsName" id_board (boardResponseId child_board_response)

            )
        )

  return ()



deleteLegacyBoards :: MigrateRWST ()
deleteLegacyBoards = do

  board_ids <- lnIds "boardsName"

  forM_ board_ids
    (\board_id -> do

      liftIO $ putStrLn $ show board_id

      void $ liftIO (try (rd (deleteBoard' board_id)) :: IO (Either SomeException (Either ApiError ())))
      deleteRedisMapByLnId "boardsName" board_id
    )

  return ()
