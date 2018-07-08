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


wsc
  :: forall w s c
   . Property
  -> ({ width :: w, style :: s, color :: c } -> Value)
  -> w
  -> s
  -> c
  -> Declaration
wsc p v w s c = Declaration p $ v { width: w, style: s, color: c }

trbl
  :: forall a
   . Property
  -> ({ top :: a, right :: a, bottom :: a, left :: a } -> Value)
  -> a
  -> a
  -> a
  -> a
  -> Declaration
trbl p v t r b l =
  Declaration p $ v { top: t, right: r, bottom: b, left: l }


type BackgroundColorValue =
  Variant
    ( V.ColorFields
    + V.GlobalFields
    + ()
    )

backgroundColor' :: BackgroundColorValue -> Declaration
backgroundColor' = Declaration BackgroundColor <<< expand

backgroundColor :: C.Color -> Declaration
backgroundColor = backgroundColor' <<< V.color_


border'
  :: V.BorderWidthValue
  -> V.BorderStyleValue
  -> V.BorderColorValue
  -> Declaration
border' = wsc Border V.border

border
  :: V.BorderWidthValue
  -> V.BorderStyleValue
  -> C.Color
  -> Declaration
border w s = border' w s <<< V.color_


borderTop'
  :: V.BorderWidthValue
  -> V.BorderStyleValue
  -> V.BorderColorValue
  -> Declaration
borderTop' = wsc BorderTop V.border

borderTop
  :: V.BorderWidthValue
  -> V.BorderStyleValue
  -> C.Color
  -> Declaration
borderTop w s = borderTop' w s <<< V.color_


borderRight'
  :: V.BorderWidthValue
  -> V.BorderStyleValue
  -> V.BorderColorValue
  -> Declaration
borderRight' = wsc BorderRight V.border

borderRight
  :: V.BorderWidthValue
  -> V.BorderStyleValue
  -> C.Color
  -> Declaration
borderRight w s = borderRight' w s <<< V.color_


borderBottom'
  :: V.BorderWidthValue
  -> V.BorderStyleValue
  -> V.BorderColorValue
  -> Declaration
borderBottom' = wsc BorderBottom V.border

borderBottom
  :: V.BorderWidthValue
  -> V.BorderStyleValue
  -> C.Color
  -> Declaration
borderBottom w s = borderBottom' w s <<< V.color_


borderLeft'
  :: V.BorderWidthValue
  -> V.BorderStyleValue
  -> V.BorderColorValue
  -> Declaration
borderLeft' = wsc BorderLeft V.border

borderLeft
  :: V.BorderWidthValue
  -> V.BorderStyleValue
  -> C.Color
  -> Declaration
borderLeft w s = borderLeft' w s <<< V.color_


borderColor'
  :: V.BorderColorValue
  -> V.BorderColorValue
  -> V.BorderColorValue
  -> V.BorderColorValue
  -> Declaration
borderColor' = trbl BorderColor V.borderColor

borderColor
  :: C.Color
  -> C.Color
  -> C.Color
  -> C.Color
  -> Declaration
borderColor t r b l =
  borderColor' (V.color_ t) (V.color_ r) (V.color_ b) (V.color_ l)


borderTopColor' :: V.BorderColorValue -> Declaration
borderTopColor' = Declaration BorderTopColor <<< expand

borderTopColor :: C.Color -> Declaration
borderTopColor = borderTopColor' <<< V.color_


borderRightColor' :: V.BorderColorValue -> Declaration
borderRightColor' = Declaration BorderRightColor <<< expand

borderRightColor :: C.Color -> Declaration
borderRightColor = borderRightColor' <<< V.color_


borderBottomColor' :: V.BorderColorValue -> Declaration
borderBottomColor' = Declaration BorderBottomColor <<< expand

borderBottomColor :: C.Color -> Declaration
borderBottomColor = borderBottomColor' <<< V.color_


borderLeftColor' :: V.BorderColorValue -> Declaration
borderLeftColor' = Declaration BorderLeftColor <<< expand

borderLeftColor :: C.Color -> Declaration
borderLeftColor = borderLeftColor' <<< V.color_


borderStyle
  :: V.BorderStyleValue
  -> V.BorderStyleValue
  -> V.BorderStyleValue
  -> V.BorderStyleValue
  -> Declaration
borderStyle = trbl BorderStyle V.borderStyle

borderTopStyle :: V.BorderStyleValue -> Declaration
borderTopStyle = Declaration BorderTopStyle <<< expand

borderRightStyle :: V.BorderStyleValue -> Declaration
borderRightStyle = Declaration BorderRightStyle <<< expand

borderBottomStyle :: V.BorderStyleValue -> Declaration
borderBottomStyle = Declaration BorderBottomStyle <<< expand

borderLeftStyle :: V.BorderStyleValue -> Declaration
borderLeftStyle = Declaration BorderLeftStyle <<< expand


borderWidth
  :: V.BorderWidthValue
  -> V.BorderWidthValue
  -> V.BorderWidthValue
  -> V.BorderWidthValue
  -> Declaration
borderWidth = trbl BorderWidth V.borderWidth

borderTopWidth :: V.BorderWidthValue -> Declaration
borderTopWidth = Declaration BorderTopWidth <<< expand

borderRightWidth :: V.BorderWidthValue -> Declaration
borderRightWidth = Declaration BorderRightWidth <<< expand

borderBottomWidth :: V.BorderWidthValue -> Declaration
borderBottomWidth = Declaration BorderBottomWidth <<< expand

borderLeftWidth :: V.BorderWidthValue -> Declaration
borderLeftWidth = Declaration BorderLeftWidth <<< expand


type BorderRadiusValue =
  Variant
    ( V.GlobalFields
    + V.LengthFields
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
    + V.GlobalFields
    + ()
    )

boxShadow :: Array V.BoxShadowRep -> Declaration
boxShadow =
  Declaration BoxShadow
    <<< (expand :: BoxShadowValue -> Value)
    <<< V.boxShadow


type ColorValue =
  Variant
    ( V.ColorFields
    + V.GlobalFields
    + ()
    )

color' :: ColorValue -> Declaration
color' = Declaration Color <<< expand

color :: C.Color -> Declaration
color = color' <<< V.color_


type FontSizeValue =
  Variant
    ( V.AbsoluteSizeFields
    + V.GlobalFields
    + V.LengthFields
    + V.Pct
    + V.RelativeSizeFields
    + V.Zero
    + ()
    )

fontSize :: FontSizeValue -> Declaration
fontSize = Declaration FontSize <<< expand


type FontWeightValue =
  Variant
    ( V.FontWeightKeywordFields
    + V.FontWeightKeywordRelativeFields
    + V.GlobalFields
    + V.Number_
    + ()
    )

fontWeight :: FontWeightValue -> Declaration
fontWeight = Declaration FontWeight <<< expand


type HeightValue =
  Variant
    ( V.Auto
    + V.GlobalFields
    + V.LengthFields
    + V.Pct
    + V.Zero
    + ()
    )

height :: HeightValue -> Declaration
height = Declaration Height <<< expand


margin
  :: V.MarginValue
  -> V.MarginValue
  -> V.MarginValue
  -> V.MarginValue
  -> Declaration
margin = trbl Margin V.margin

marginBottom :: V.MarginValue -> Declaration
marginBottom = Declaration MarginBottom <<< expand

marginLeft :: V.MarginValue -> Declaration
marginLeft = Declaration MarginLeft <<< expand

marginRight :: V.MarginValue -> Declaration
marginRight = Declaration MarginRight <<< expand

marginTop :: V.MarginValue -> Declaration
marginTop = Declaration MarginTop <<< expand


outline'
  :: V.OutlineWidthValue
  -> V.OutlineStyleValue
  -> V.OutlineColorValue
  -> Declaration
outline' w s c = Declaration Outline $ V.outline { width: w, style: s, color: c }


outline
  :: V.OutlineWidthValue
  -> V.OutlineStyleValue
  -> C.Color
  -> Declaration
outline w s = outline' w s <<< V.color_


outlineColor' :: V.OutlineColorValue -> Declaration
outlineColor' = Declaration OutlineColor <<< expand

outlineColor :: C.Color -> Declaration
outlineColor = outlineColor' <<< V.color_

outlineStyle :: V.OutlineStyleValue -> Declaration
outlineStyle = Declaration OutlineStyle <<< expand

outlineWidth :: V.OutlineWidthValue -> Declaration
outlineWidth = Declaration OutlineWidth <<< expand


padding
  :: V.PaddingValue
  -> V.PaddingValue
  -> V.PaddingValue
  -> V.PaddingValue
  -> Declaration
padding = trbl Padding V.padding

paddingBottom :: V.PaddingValue -> Declaration
paddingBottom = Declaration PaddingBottom <<< expand

paddingLeft :: V.PaddingValue -> Declaration
paddingLeft = Declaration PaddingLeft <<< expand

paddingRight :: V.PaddingValue -> Declaration
paddingRight = Declaration PaddingRight <<< expand

paddingTop :: V.PaddingValue -> Declaration
paddingTop = Declaration PaddingTop <<< expand


type TextAlignValue =
  Variant
    ( V.Center
    + V.GlobalFields
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
    + V.GlobalFields
    + V.LengthFields
    + V.Pct
    + V.Zero
    + ()
    )

width :: WidthValue -> Declaration
width = Declaration Width <<< expand
