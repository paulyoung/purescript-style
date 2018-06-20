module Style.Declaration.Property where

import Prelude

data Property
  = BackgroundColor
  | BorderBottomLeftRadius
  | BorderBottomRightRadius
  | BorderTopLeftRadius
  | BorderTopRightRadius
  | Color
  | FontSize
  | FontWeight
  | Height
  | MarginBottom
  | MarginLeft
  | MarginRight
  | MarginTop
  | OutlineColor
  | OutlineStyle
  | OutlineWidth
  | PaddingBottom
  | PaddingLeft
  | PaddingRight
  | PaddingTop
  | TextAlign
  | Width

derive instance eqProperty :: Eq Property
derive instance ordProperty :: Ord Property

instance showProperty :: Show Property where
  show = case _ of
    BackgroundColor -> "BackgroundColor"
    BorderBottomLeftRadius -> "BorderBottomLeftRadius"
    BorderBottomRightRadius -> "BorderBottomRightRadius"
    BorderTopLeftRadius -> "BorderTopLeftRadius"
    BorderTopRightRadius -> "BorderTopRightRadius"
    Color -> "Color"
    FontSize -> "FontSize"
    FontWeight -> "FontWeight"
    Height -> "Height"
    MarginBottom -> "MarginBottom"
    MarginLeft -> "MarginLeft"
    MarginRight -> "MarginRight"
    MarginTop -> "MarginTop"
    OutlineColor -> "OutlineColor"
    OutlineStyle -> "OutlineStyle"
    OutlineWidth -> "OutlineWidth"
    PaddingBottom -> "PaddingBottom"
    PaddingLeft -> "PaddingLeft"
    PaddingRight -> "PaddingRight"
    PaddingTop -> "PaddingTop"
    TextAlign -> "TextAlign"
    Width -> "Width"

render :: Property -> String
render = case _ of
  BackgroundColor -> "background-color"
  BorderBottomLeftRadius -> "border-bottom-left-radius"
  BorderBottomRightRadius -> "border-bottom-right-radius"
  BorderTopLeftRadius -> "border-top-left-radius"
  BorderTopRightRadius -> "border-top-right-radius"
  Color -> "color"
  FontSize -> "font-size"
  FontWeight -> "font-weight"
  Height -> "height"
  MarginBottom -> "margin-bottom"
  MarginLeft -> "margin-left"
  MarginRight -> "margin-right"
  MarginTop -> "margin-top"
  OutlineColor -> "outline-color"
  OutlineStyle -> "outline-style"
  OutlineWidth -> "outline-width"
  PaddingBottom -> "padding-bottom"
  PaddingLeft -> "padding-left"
  PaddingRight -> "padding-right"
  PaddingTop -> "padding-top"
  TextAlign -> "text-align"
  Width -> "width"
