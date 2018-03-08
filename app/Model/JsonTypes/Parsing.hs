{-# LANGUAGE OverloadedStrings #-}

module Model.JsonTypes.Parsing
    ( always
    , lengthBetween
    , matchesRegex
    , nonEmptyList
    , nonEmptyString
    , nonZero
    , nonZeroPositive
    , parseIf
    , parseMaybeIf
    , positive
    , validEmail
    )
where

import           Data.Aeson          ((.:), (.:?))
import           Data.Aeson.Types    (Parser, Object, FromJSON)
import           Data.String         (IsString)
import           Data.Text           (Text, unpack)
import           Text.Regex.PCRE     ((=~))

nonEmptyString :: (IsString a, Eq a, FromJSON a) => Text -> a -> Parser a
nonEmptyString field val
  | val == "" = fail $ unpack field ++ " cannot be an empty string"
  | otherwise = return val

nonEmptyList :: (FromJSON a) => Text -> [a] -> Parser [a]
nonEmptyList field [] = fail $ unpack field ++ " cannot be empty"
nonEmptyList _ val    = return val

nonZero :: (Num a, Eq a, FromJSON a) => Text -> a -> Parser a
nonZero field 0 = fail $ unpack field ++ " must not be zero"
nonZero _ val   = return val

positive :: (Num a, Ord a, FromJSON a) => Text -> a -> Parser a
positive field val
  | val < 0   = fail $ unpack field ++ " must not be negative"
  | otherwise = return val

nonZeroPositive :: (Num a, Ord a, FromJSON a) => Text -> a -> Parser a
nonZeroPositive field val = positive field val >> nonZero field val

validEmail :: Text -> Text -> Parser Text
validEmail = matchesRegex "^.+@.+\\..+$"

lengthBetween :: (Show a, Ord a, Num a, FromJSON a)
              => (a, a) -> Text -> a -> Parser a
lengthBetween (left, right) field val
  | left <= val && val <= right = return val
  | otherwise = fail $ unpack field ++ " is not between "
                       ++ show left ++ " and " ++ show right

matchesRegex :: String -> Text -> Text -> Parser Text
matchesRegex regex field val
  | matches   = return val -- TODO implement
  | otherwise = fail $ unpack field ++ " does not match required regex."
  where matches :: Bool
        matches = unpack val =~ regex

parseIf :: (FromJSON a) => (Text -> a -> Parser b) -> Text -> Object -> Parser b
parseIf predicate field o = o .: field >>= predicate field

always :: (FromJSON a) => Text -> a -> Parser a
always _ = return

parseMaybeIf :: (FromJSON a)
             => (Text -> a -> Parser b) -> Text -> Object -> Parser (Maybe b)
parseMaybeIf predicate field o = do  -- TODO simplify
  mVal <- o .:? field
  case mVal of
    Nothing  -> return Nothing
    Just val -> Just <$> predicate field val
