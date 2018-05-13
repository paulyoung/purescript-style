module Style.Property where

import Prelude

import Data.Variant (Variant)
import Style.Property.Name (Name(..))
import Style.Property.Value (Auto, Pct, Px, Value)
import Type.Row (type (+))

data Property = Property Name Value

derive instance eqProperty :: Eq Property
derive instance ordProperty :: Ord Property

instance showProperty :: Show Property where
  show (Property n v) = "(Property " <> show n <> " " <> show v <> ")"


type HeightValue =
  Variant
    ( Auto
    + Pct
    + Px
    + ()
    )

height :: HeightValue -> Property
height = Property Height


type WidthValue =
  Variant
    ( Auto
    + Pct
    + Px
    + ()
    )

width :: WidthValue -> Property
width = Property Width
