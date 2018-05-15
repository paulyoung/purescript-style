module Style.Property where

import Prelude

import Data.Variant (Variant, expand)
import Style.Property.Name (Name(..))
import Style.Property.Value (Auto, Em, Pct, Px, Value)
import Type.Row (type (+))

data Property = Property Name Value

derive instance eqProperty :: Eq Property
derive instance ordProperty :: Ord Property

instance showProperty :: Show Property where
  show (Property n v) = "(Property " <> show n <> " " <> show v <> ")"


type FontSizeValue =
  Variant
    ( Px
    + ()
    )

fontSize :: FontSizeValue -> Property
fontSize = Property FontSize <<< expand


type HeightValue =
  Variant
    ( Auto
    + Em
    + Pct
    + Px
    + ()
    )

height :: HeightValue -> Property
height = Property Height <<< expand


type MarginValue =
  Variant
    ( Auto
    + Em
    + Pct
    + Px
    + ()
    )

margin :: MarginValue -> Property
margin = Property Margin <<< expand


type PaddingValue =
  Variant
    ( Auto
    + Em
    + Pct
    + Px
    + ()
    )

padding :: PaddingValue -> Property
padding = Property Padding <<< expand


type WidthValue =
  Variant
    ( Auto
    + Em
    + Pct
    + Px
    + ()
    )

width :: WidthValue -> Property
width = Property Width <<< expand
