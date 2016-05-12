{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ViewPatterns              #-}

module LN.SMF.Migration.Sanitize (
  fixNick,
  replaceNickChar,
  fixDisplayNick,
  replaceDisplayNickChar,
  titleify,
  sanitizeHtml
) where



import           Data.Char
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.ICU.Replace
import           Text.HTML.TagSoup.Fast



replaceNickChar :: Char -> Maybe Char
replaceNickChar ((==) '$' -> True) = Just 'S'
replaceNickChar (not . isAlphaNum -> True) = Nothing
replaceNickChar c = Just c



fixNick :: Text -> Text
fixNick = T.pack . catMaybes . map replaceNickChar . T.unpack . T.toLower



replaceDisplayNickChar :: Char -> Maybe Char
replaceDisplayNickChar (isControl -> True) = Nothing
replaceDisplayNickChar c = Just c



fixDisplayNick :: Text -> Text
fixDisplayNick = T.pack . catMaybes . map replaceDisplayNickChar . T.unpack



titleify :: Text -> Text
titleify s =
  case fields of
    []       -> s
    [x]      -> x
    (x:rest) -> T.concat $ x : map T.toTitle rest
  where
  fields = T.splitOn " " s



sanitizeHtml :: Text -> Text
sanitizeHtml = replaceAll "<br />" "\n" . unescapeHtmlT
