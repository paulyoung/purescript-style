module Style.Property where

import Prelude

import Data.Variant (Variant, case_, on)
import Style.Value (Value(..), _auto, _pct, _px)


data Property
  = Height Value
  | Width Value

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
height = Height <<< value
  where
  value :: HeightValue -> Value
  value = case_
    # on _auto (const Auto)
    # on _pct Percent
    # on _px Pixels


type WidthValue =
  Variant
    ( auto :: Unit
    , pct :: Number
    , px :: Number
    )

width :: WidthValue -> Property
width = Width <<< value
  where
  value :: WidthValue -> Value
  value = case_
    # on _auto (const Auto)
    # on _pct Percent
    # on _px Pixels
