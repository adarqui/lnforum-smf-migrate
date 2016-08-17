{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ViewPatterns              #-}

module LN.SMF.Migration.Sanitize (
  fixNick,
  replaceNickChar,
  fixDisplayNick,
  replaceDisplayNickChar
) where



import           Data.Char
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as Text



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
