module Style.Property.Name where

import Prelude

data Name
  = FontSize
  | Height
  | Margin
  | Padding
  | Width

derive instance eqName :: Eq Name
derive instance ordName :: Ord Name

instance showName :: Show Name where
  show = case _ of
    FontSize -> "FontSize"
    Height -> "Height"
    Margin -> "Margin"
    Padding -> "Padding"
    Width -> "Width"
