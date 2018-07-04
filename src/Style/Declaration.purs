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

backgroundColor' :: BackgroundColorValue -> Declaration
backgroundColor' = Declaration BackgroundColor <<< expand

backgroundColor :: C.Color -> Declaration
backgroundColor = backgroundColor' <<< V.color_


type BorderRadiusValue =
  Variant
    ( V.Global
    + V.Length
    + V.Pct
    + V.Zero
    + ()
    )

borderRadius
  :: BorderRadiusValue
  -> BorderRadiusValue
  -> BorderRadiusValue
  -> BorderRadiusValue
  -> Array Declaration
borderRadius tl tr br bl =
  [ borderTopLeftRadius tl
  , borderTopRightRadius tr
  , borderBottomRightRadius br
  , borderBottomLeftRadius bl
  ]

borderBottomLeftRadius :: BorderRadiusValue -> Declaration
borderBottomLeftRadius = Declaration BorderBottomLeftRadius <<< expand

borderBottomRightRadius :: BorderRadiusValue -> Declaration
borderBottomRightRadius = Declaration BorderBottomRightRadius <<< expand

borderTopLeftRadius :: BorderRadiusValue -> Declaration
borderTopLeftRadius = Declaration BorderTopLeftRadius <<< expand

borderTopRightRadius :: BorderRadiusValue -> Declaration
borderTopRightRadius = Declaration BorderTopRightRadius <<< expand


type BoxShadowValue =
  Variant
    ( V.BoxShadow
    + V.Global
    + ()
    )

boxShadow :: Array V.BoxShadow_ -> Declaration
boxShadow =
  Declaration BoxShadow
    <<< (expand :: BoxShadowValue -> Value)
    <<< V.boxShadow


type ColorValue =
  Variant
    ( V.Color
    + V.Global
    + ()
    )

color' :: ColorValue -> Declaration
color' = Declaration Color <<< expand

color :: C.Color -> Declaration
color = color' <<< V.color_


type FontSizeValue =
  Variant
    ( V.AbsoluteSize
    + V.Global
    + V.Length
    + V.Pct
    + V.RelativeSize
    + V.Zero
    + ()
    )

fontSize :: FontSizeValue -> Declaration
fontSize = Declaration FontSize <<< expand


type FontWeightValue =
  Variant
    ( V.FontWeightKeyword
    + V.FontWeightKeywordRelative
    + V.Global
    + V.Number_
    + ()
    )

fontWeight :: FontWeightValue -> Declaration
fontWeight = Declaration FontWeight <<< expand


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

margin
  :: MarginValue
  -> MarginValue
  -> MarginValue
  -> MarginValue
  -> Array Declaration
margin t r b l =
  [ marginTop t
  , marginRight r
  , marginBottom b
  , marginLeft l
  ]

marginBottom :: MarginValue -> Declaration
marginBottom = Declaration MarginBottom <<< expand

marginLeft :: MarginValue -> Declaration
marginLeft = Declaration MarginLeft <<< expand

marginRight :: MarginValue -> Declaration
marginRight = Declaration MarginRight <<< expand

marginTop :: MarginValue -> Declaration
marginTop = Declaration MarginTop <<< expand


outline'
  :: OutlineWidthValue
  -> OutlineStyleValue
  -> OutlineColorValue
  -> Array Declaration
outline' w s c =
  [ outlineWidth w
  , outlineStyle s
  , outlineColor' c
  ]

outline
  :: OutlineWidthValue
  -> OutlineStyleValue
  -> C.Color
  -> Array Declaration
outline w s = outline' w s <<< V.color_

type OutlineColorValue =
  Variant
    ( V.Color
    + V.Global
    + V.Invert
    + ()
    )

outlineColor' :: OutlineColorValue -> Declaration
outlineColor' = Declaration OutlineColor <<< expand

outlineColor :: C.Color -> Declaration
outlineColor = outlineColor' <<< V.color_


type OutlineStyleValue =
  Variant
    ( V.Auto
    + V.Dashed
    + V.Dotted
    + V.Double
    + V.Global
    + V.Groove
    + V.Inset
    + V.None
    + V.Outset
    + V.Ridge
    + V.Solid
    + ()
    )

outlineStyle :: OutlineStyleValue -> Declaration
outlineStyle = Declaration OutlineStyle <<< expand


type OutlineWidthValue =
  Variant
    ( V.Global
    + V.Length
    + V.OutlineWidthKeyword
    + V.Zero
    + ()
    )

outlineWidth :: OutlineWidthValue -> Declaration
outlineWidth = Declaration OutlineWidth <<< expand


type PaddingValue =
  Variant
    ( V.Global
    + V.Length
    + V.Pct
    + V.Zero
    + ()
    )

padding
  :: PaddingValue
  -> PaddingValue
  -> PaddingValue
  -> PaddingValue
  -> Array Declaration
padding t r b l =
  [ paddingTop t
  , paddingRight r
  , paddingBottom b
  , paddingLeft l
  ]

paddingBottom :: PaddingValue -> Declaration
paddingBottom = Declaration PaddingBottom <<< expand

paddingLeft :: PaddingValue -> Declaration
paddingLeft = Declaration PaddingLeft <<< expand

paddingRight :: PaddingValue -> Declaration
paddingRight = Declaration PaddingRight <<< expand

paddingTop :: PaddingValue -> Declaration
paddingTop = Declaration PaddingTop <<< expand


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
