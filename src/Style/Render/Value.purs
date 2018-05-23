module Style.Render.Value where

import Prelude

import Color (cssStringRGBA)
import Data.Number.Format (toString)
import Data.Variant (match)
import Style.Declaration.Value (Value)

value :: Value -> String
value = match
  { auto: const "auto"
  , center: const "center"
  , color: cssStringRGBA
  , em: \n -> toString n <> "em"
  , justified: const "justified"
  , left: const "left"
  , pct: \n -> toString n <> "%"
  , px: \n -> toString n <> "px"
  , right: const "right"
  , zero: const "0"
  }
