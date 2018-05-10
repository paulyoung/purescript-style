module Style.Property.Name where

import Prelude

data Name
  = Height
  | Width

derive instance eqName :: Eq Name
derive instance ordName :: Ord Name

instance showName :: Show Name where
  show = case _ of
    Height -> "Height"
    Width -> "Width"
