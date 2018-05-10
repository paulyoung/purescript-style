module Style.Property where

import Prelude

import Data.Variant (Variant)


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
    ( auto :: Unit
    , pct :: Number
    , px :: Number
    )

height :: HeightValue -> Property
height = Height


type WidthValue =
  Variant
    ( auto :: Unit
    , pct :: Number
    , px :: Number
    )

width :: WidthValue -> Property
width = Width
