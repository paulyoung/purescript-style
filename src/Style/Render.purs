module Style.Render where

import Prelude

import Data.Number.Format (toString)
import Data.Variant (match)
import Style.Property (Property(..))
import Style.Value (Value)

name :: Property -> String
name = case _ of
  Height _ -> "height"
  Width _ -> "width"

value :: Value -> String
value = match
  { auto: const "auto"
  , pct: \n -> toString n <> "%"
  , px: \n -> toString n <> "px"
  }

-- property :: Property -> String
-- property = 
