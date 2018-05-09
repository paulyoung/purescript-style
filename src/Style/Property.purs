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
  ( auto :: Unit
  , pct :: Number
  , px :: Number
  )

height :: Variant HeightValue -> Property
height = Height <<< value
  where
  value :: Variant HeightValue -> Value
  value = case_
    # on _auto (const Auto)
    # on _pct Percent
    # on _px Pixels


type WidthValue =
  ( auto :: Unit
  , pct :: Number
  , px :: Number
  )

width :: Variant WidthValue -> Property
width = Width <<< value
  where
  value :: Variant WidthValue -> Value
  value = case_
    # on _auto (const Auto)
    # on _pct Percent
    # on _px Pixels
