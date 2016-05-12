{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LN.SMF.Migration.Board (
  createLegacyBoards,
  deleteLegacyBoards
) where



import           Control.Exception              (SomeException (..), try)
import           Control.Monad                  (forM_, void)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.RWS
import           Data.Int
import           Data.Text                      (Text)
import           Database.MySQL.Simple
import           LN.Api                         (createBoard, createChildBoard,
                                                 deleteBoard', runDefault)
import           LN.SMF.Migration.Connect.Redis
import           LN.SMF.Migration.Control
import           LN.SMF.Migration.Sanitize
import           LN.T
import           Text.HTML.TagSoup.Fast



createLegacyBoards :: MigrateRWST ()
createLegacyBoards = do

  liftIO $ putStrLn "migrating boards.."

  mysql <- asks rMySQL

  categories <- liftIO $ query_ mysql "select id_cat, name from smf_categories"

  board_ids <- smfIds boardsName

  forum_ids <- lnIds forumsName

  case forum_ids of
    [] -> liftIO $ putStrLn "Forum does not exist."
    (forum_id:_) -> do

      forM_
        (filter (\(id_cat, _) -> not $ id_cat `elem` board_ids) categories)
        (\(id_cat :: Int64, name :: Text) -> do

          liftIO $ print $ (id_cat, name)

          eresult <- liftIO $ runDefault (createBoard [("unix_ts", "1240177678")] forum_id $ BoardRequest name Nothing)
          case eresult of
            (Left err)             -> liftIO $ putStrLn err
            (Right board_response) -> do
              createRedisMap boardsName id_cat (boardResponseId board_response)
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

              mresult <- findLnIdFromSmfId boardsName id_parent

              case mresult of
                Nothing -> return () -- doesn't exist??
                (Just parent) -> do

                  eresult <- liftIO $ runDefault (createChildBoard [("unix_ts", "1240177678")] forum_id parent $
                    BoardRequest (sanitizeHtml board_name) (Just $ sanitizeHtml board_desc))

                  case eresult of
                    (Left err) -> liftIO $ putStrLn err
                    (Right child_board_response) -> do
                      createRedisMap boardsName id_board (boardResponseId child_board_response)

            )
        )

  return ()



deleteLegacyBoards :: MigrateRWST ()
deleteLegacyBoards = do

  board_ids <- lnIds boardsName

  forM_ board_ids
    (\board_id -> do

      liftIO $ putStrLn $ show board_id

      void $ liftIO (try (runDefault (deleteBoard' board_id)) :: IO (Either SomeException ()))
      deleteRedisMapByLnId boardsName board_id
    )

  return ()
