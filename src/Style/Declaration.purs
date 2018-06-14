module Style.Declaration where

import Prelude

import Color as C
import Data.Variant (Variant, expand)
import Style.Declaration.Property (Property(..))
import Style.Declaration.Property as Property
import Style.Declaration.Value (Value)
import Style.Declaration.Value as V
import Type.Row (type (+))

data Declaration = Declaration Property Value

derive instance eqDeclaration :: Eq Declaration

instance ordDeclaration :: Ord Declaration where
  compare (Declaration p1 v1) (Declaration p2 v2) =
    p1 `compare` p2 <> V.render v1 `compare` V.render v2

instance showDeclaration :: Show Declaration where
  show (Declaration p v) = "(Declaration " <> show p <> " " <> show v <> ")"

render :: Declaration -> String
render (Declaration p v) = Property.render p <> ": " <> V.render v <> ";"


type BackgroundColorValue =
  Variant
    ( V.Color
    + V.Global
    + ()
    )

backgroundColor :: C.Color -> Declaration
backgroundColor = backgroundColor' <<< V.color
  where
  backgroundColor' :: BackgroundColorValue -> Declaration
  backgroundColor' = Declaration BackgroundColor <<< expand


type BorderRadiusValue =
  Variant
    ( V.Global
    + V.Length
    + V.Pct
    + ()
    )

borderRadius :: BorderRadiusValue -> Declaration
borderRadius = Declaration BorderRadius <<< expand


type ColorValue =
  Variant
    ( V.Color
    + V.Global
    + ()
    )

color :: C.Color -> Declaration
color = color' <<< V.color
  where
  color' :: ColorValue -> Declaration
  color' = Declaration Color <<< expand


type FontSizeValue =
  Variant
    ( V.Global
    + V.Length
    + V.Pct
    + V.Zero

    -- Absolute size values
    + V.XxSmall
    + V.XSmall
    + V.Small
    + V.Medium
    + V.Large
    + V.XLarge
    + V.XxLarge

    -- Relative size values
    + V.Smaller
    + V.Larger

    + ()
    )

fontSize :: FontSizeValue -> Declaration
fontSize = Declaration FontSize <<< expand


type HeightValue =
  Variant
    ( V.Auto
    + V.Global
    + V.Length
    + V.Pct
    + V.Zero
    + ()
    )

height :: HeightValue -> Declaration
height = Declaration Height <<< expand


type MarginValue =
  Variant
    ( V.Auto
    + V.Global
    + V.Length
    + V.Pct
    + V.Zero
    + ()
    )

margin :: MarginValue -> Declaration
margin = Declaration Margin <<< expand


type PaddingValue =
  Variant
    ( V.Global
    + V.Length
    + V.Pct
    + V.Zero
    + ()
    )

padding :: PaddingValue -> Declaration
padding = Declaration Padding <<< expand


type TextAlignValue =
  Variant
    ( V.Center
    + V.Global
    + V.Justify
    + V.JustifyAll
    + V.Left
    + V.Right
    + ()
    )

textAlign :: TextAlignValue -> Declaration
textAlign = Declaration TextAlign <<< expand


type WidthValue =
  Variant
    ( V.Auto
    + V.Global
    + V.Length
    + V.Pct
    + V.Zero
    + ()
    )

width :: WidthValue -> Declaration
width = Declaration Width <<< expand
