module Style.Property where

import Prelude

import Color as C
import Data.Variant (Variant, expand)
import Style.Property.Name (Name(..))
import Style.Property.Value (Auto, Center, Color, Em, Justified, Left, Pct, Px, Value, Zero, Right)
import Style.Property.Value (color) as V
import Style.Render.Value (value)
import Type.Row (type (+))

data Property = Property Name Value

derive instance eqProperty :: Eq Property

instance ordProperty :: Ord Property where
  compare (Property n1 v1) (Property n2 v2) =
    n1 `compare` n2 <> value v1 `compare` value v2

instance showProperty :: Show Property where
  show (Property n v) = "(Property " <> show n <> " " <> show v <> ")"


type BackgroundColorValue =
  Variant
    ( Color
    + ()
    )

backgroundColor :: C.Color -> Property
backgroundColor = backgroundColor' <<< V.color
  where
  backgroundColor' :: BackgroundColorValue -> Property
  backgroundColor' = Property BackgroundColor <<< expand


type ColorValue =
  Variant
    ( Color
    + ()
    )

color :: C.Color -> Property
color = color' <<< V.color
  where
  color' :: ColorValue -> Property
  color' = Property Color <<< expand


type FontSizeValue =
  Variant
    ( Px
    + Zero
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
    + Zero
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
    + Zero
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
    + Zero
    + ()
    )

padding :: PaddingValue -> Property
padding = Property Padding <<< expand


type TextAlignValue =
  Variant
    ( Center
    + Justified
    + Left
    + Right
    + ()
    )

textAlign :: TextAlignValue -> Property
textAlign = Property TextAlign <<< expand


type WidthValue =
  Variant
    ( Auto
    + Em
    + Pct
    + Px
    + Zero
    + ()
    )

width :: WidthValue -> Property
width = Property Width <<< expand
