--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2018 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.JSON (parseTabulaJSON) where

--------------------------------------------------------------------------------

import GHC.Generics

import Data.Aeson
import Data.Aeson.Types
import Data.Char

--------------------------------------------------------------------------------

jsonOpts :: Options
jsonOpts = defaultOptions {
    fieldLabelModifier = toFieldName
}

toFieldName :: String -> String
toFieldName = while isUpper toLower . dropWhile isLower
    where while p f [] = []
          while p f (x:xs)
            | p x       = f x : while p f xs
            | otherwise = x : xs

-- | Formatting options for field names.
parseTabulaJSON :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
parseTabulaJSON = genericParseJSON jsonOpts

formatTabulaJSON :: (Generic a, GToJSON Zero (Rep a)) => a -> Value
formatTabulaJSON = genericToJSON jsonOpts

--------------------------------------------------------------------------------
