module Style.Property where

import Prelude

import Data.Variant (Variant)
import Style.Value (Auto, Percent, Pixels)
import Type.Row (type (+))


data Property
  = Height HeightValue
  | Width WidthValue

derive instance eqProperty :: Eq Property
derive instance ordProperty :: Ord Property

instance showProperty :: Show Property where
  show = case _ of
    Height v -> "(Height " <> show v <> ")"
    Width v -> "(Width " <> show v <> ")"


type HeightValue =
  Variant
    ( Auto
    + Percent
    + Pixels
    + ()
    )

height :: HeightValue -> Property
height = Height


type WidthValue =
  Variant
    ( Auto
    + Percent
    + Pixels
    + ()
    )

width :: WidthValue -> Property
width = Width
