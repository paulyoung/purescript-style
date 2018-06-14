module Style.Declaration.Property where

import Prelude

data Property
  = BackgroundColor
  | BorderRadius
  | Color
  | FontSize
  | Height
  | Margin
  | Padding
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
    Height -> "Height"
    Margin -> "Margin"
    Padding -> "Padding"
    TextAlign -> "TextAlign"
    Width -> "Width"

render :: Property -> String
render = case _ of
  BackgroundColor -> "background-color"
  BorderRadius -> "border-radius"
  Color -> "color"
  FontSize -> "font-size"
  Height -> "height"
  Margin -> "margin"
  Padding -> "padding"
  TextAlign -> "text-align"
  Width -> "width"
