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
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as Lazy
import qualified Data.Text.Lazy.Builder as Lazy
import           HTMLEntities.Decoder



replaceNickChar :: Char -> Maybe Char
replaceNickChar ((==) '$' -> True) = Just 'S'
replaceNickChar (not . isAlphaNum -> True) = Nothing
replaceNickChar c = Just c



fixNick :: Text -> Text
fixNick = Text.pack . catMaybes . map replaceNickChar . Text.unpack . Text.toLower



replaceDisplayNickChar :: Char -> Maybe Char
replaceDisplayNickChar (isControl -> True) = Nothing
replaceDisplayNickChar c = Just c



fixDisplayNick :: Text -> Text
fixDisplayNick = Text.pack . catMaybes . map replaceDisplayNickChar . Text.unpack



titleify :: Text -> Text
titleify s =
  case fields of
    []       -> s
    [x]      -> x
    (x:rest) -> Text.concat $ x : map Text.toTitle rest
  where
  fields = Text.splitOn " " s



sanitizeHtml :: Text -> Text
sanitizeHtml = Text.replace "<br />" "\n" . Lazy.toStrict . Lazy.toLazyText . htmlEncodedText
