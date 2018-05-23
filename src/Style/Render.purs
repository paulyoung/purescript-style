module Style.Render where

import Prelude

import Data.Array (intercalate)
import Style.Declaration (Declaration(..))
import Style.Declaration.Property (Property(..))
import Style.Render.Value (value)

property :: Property -> String
property = case _ of
  BackgroundColor -> "background-color"
  Color -> "color"
  FontSize -> "font-size"
  Height -> "height"
  Margin -> "margin"
  Padding -> "padding"
  TextAlign -> "text-align"
  Width -> "width"

declaration :: Declaration -> String
declaration (Declaration p v) = property p <> ": " <> value v <> ";"

inline :: Array Declaration -> String
inline = intercalate " " <<< map declaration
