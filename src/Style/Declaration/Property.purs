module Style.Declaration.Property where

import Prelude

data Property
  = BackgroundColor
  | BorderRadius
  | Color
  | FontSize
  | FontWeight
  | Height
  | Margin
  | MarginBottom
  | MarginLeft
  | MarginRight
  | MarginTop
  | Padding
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
    BorderRadius -> "BorderRadius"
    Color -> "Color"
    FontSize -> "FontSize"
    FontWeight -> "FontWeight"
    Height -> "Height"
    Margin -> "Margin"
    MarginBottom -> "MarginBottom"
    MarginLeft -> "MarginLeft"
    MarginRight -> "MarginRight"
    MarginTop -> "MarginTop"
    Padding -> "Padding"
    PaddingBottom -> "PaddingBottom"
    PaddingLeft -> "PaddingLeft"
    PaddingRight -> "PaddingRight"
    PaddingTop -> "PaddingTop"
    TextAlign -> "TextAlign"
    Width -> "Width"

render :: Property -> String
render = case _ of
  BackgroundColor -> "background-color"
  BorderRadius -> "border-radius"
  Color -> "color"
  FontSize -> "font-size"
  FontWeight -> "font-weight"
  Height -> "height"
  Margin -> "margin"
  MarginBottom -> "margin-bottom"
  MarginLeft -> "margin-left"
  MarginRight -> "margin-right"
  MarginTop -> "margin-top"
  Padding -> "padding"
  PaddingBottom -> "padding-bottom"
  PaddingLeft -> "padding-left"
  PaddingRight -> "padding-right"
  PaddingTop -> "padding-top"
  TextAlign -> "text-align"
  Width -> "width"
