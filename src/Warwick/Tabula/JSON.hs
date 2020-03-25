--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright (c) Michael B. Gale (m.gale@warwick.ac.uk)                       --
--------------------------------------------------------------------------------

module Warwick.Tabula.JSON (
    compactObject,
    (.=?),
    parseTabulaJSON, 
    formatTabulaJSON
) where

--------------------------------------------------------------------------------

import GHC.Generics

import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Data.HashMap.Lazy as HM
import Data.Hashable
import Data.Text (Text)

--------------------------------------------------------------------------------

-- | 'compactObject' @objects@ is similar to Aeson's `object` function, except
-- that empty entries are omitted.
compactObject :: [Object] -> Value 
compactObject es = Object $ HM.unions es

-- | @key .=? val@ constructs a `HashMap` with a single entry or an empty 
-- hashmap if @val@ is `Nothing`.
(.=?) :: ToJSON v => Text -> Maybe v -> Object
(.=?) _ Nothing = HM.empty 
(.=?) key (Just val) = HM.singleton key (toJSON val)

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
