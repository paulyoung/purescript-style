module Style.Declaration where

import Prelude

import Color as C
import Data.Variant (Variant, expand)
import Style.Declaration.Property (Property(..))
import Style.Declaration.Property as Property
import Style.Declaration.Value (Auto, Center, Color, Em, Justified, Left, Pct, Px, Value, Zero, Right)
import Style.Declaration.Value as Value
import Type.Row (type (+))

data Declaration = Declaration Property Value

derive instance eqDeclaration :: Eq Declaration

instance ordDeclaration :: Ord Declaration where
  compare (Declaration p1 v1) (Declaration p2 v2) =
    p1 `compare` p2 <> Value.render v1 `compare` Value.render v2

instance showDeclaration :: Show Declaration where
  show (Declaration p v) = "(Declaration " <> show p <> " " <> show v <> ")"

render :: Declaration -> String
render (Declaration p v) = Property.render p <> ": " <> Value.render v <> ";"


type BackgroundColorValue =
  Variant
    ( Color
    + ()
    )

backgroundColor :: C.Color -> Declaration
backgroundColor = backgroundColor' <<< Value.color
  where
  backgroundColor' :: BackgroundColorValue -> Declaration
  backgroundColor' = Declaration BackgroundColor <<< expand


type ColorValue =
  Variant
    ( Color
    + ()
    )

color :: C.Color -> Declaration
color = color' <<< Value.color
  where
  color' :: ColorValue -> Declaration
  color' = Declaration Color <<< expand


type FontSizeValue =
  Variant
    ( Em
    + Px
    + Zero
    + ()
    )

fontSize :: FontSizeValue -> Declaration
fontSize = Declaration FontSize <<< expand


type HeightValue =
  Variant
    ( Auto
    + Em
    + Pct
    + Px
    + Zero
    + ()
    )

height :: HeightValue -> Declaration
height = Declaration Height <<< expand


type MarginValue =
  Variant
    ( Auto
    + Em
    + Pct
    + Px
    + Zero
    + ()
    )

margin :: MarginValue -> Declaration
margin = Declaration Margin <<< expand


type PaddingValue =
  Variant
    ( Auto
    + Em
    + Pct
    + Px
    + Zero
    + ()
    )

padding :: PaddingValue -> Declaration
padding = Declaration Padding <<< expand


type TextAlignValue =
  Variant
    ( Center
    + Justified
    + Left
    + Right
    + ()
    )

textAlign :: TextAlignValue -> Declaration
textAlign = Declaration TextAlign <<< expand


type WidthValue =
  Variant
    ( Auto
    + Em
    + Pct
    + Px
    + Zero
    + ()
    )

width :: WidthValue -> Declaration
width = Declaration Width <<< expand
