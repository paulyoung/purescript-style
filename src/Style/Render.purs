module Style.Render where

import Prelude

import Data.Array (intercalate)
import Data.Number.Format (toString)
import Data.Variant (match)
import Style.Property (Property(..))
import Style.Property.Name (Name(..))
import Style.Property.Value (Value)

name :: Name -> String
name = case _ of
  Height -> "height"
  Width -> "width"

value :: Value -> String
value = match
  { auto: const "auto"
  , em: \n -> toString n <> "em"
  , pct: \n -> toString n <> "%"
  , px: \n -> toString n <> "px"
  }

property :: Property -> String
property (Property n v) = name n <> ": " <> value v <> ";"

inline :: Array Property -> String
inline = intercalate " " <<< map property
