module Style.Render where

import Prelude

import Data.Array (intercalate)
import Style.Property (Property(..))
import Style.Property.Name (Name(..))
import Style.Render.Value (value)

name :: Name -> String
name = case _ of
  BackgroundColor -> "background-color"
  Color -> "color"
  FontSize -> "font-size"
  Height -> "height"
  Margin -> "margin"
  Padding -> "padding"
  TextAlign -> "text-align"
  Width -> "width"

property :: Property -> String
property (Property n v) = name n <> ": " <> value v <> ";"

inline :: Array Property -> String
inline = intercalate " " <<< map property
