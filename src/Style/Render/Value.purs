module Style.Render.Value where

import Prelude

import Color (cssStringRGBA)
import Data.Number.Format (toString)
import Data.Variant (match)
import Style.Property.Value (Value)

value :: Value -> String
value = match
  { auto: const "auto"
  , color: cssStringRGBA
  , em: \n -> toString n <> "em"
  , pct: \n -> toString n <> "%"
  , px: \n -> toString n <> "px"
  , zero: const "0"
  }
