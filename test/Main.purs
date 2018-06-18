module Test.Main where

import Prelude hiding (zero)

import Color (black, rgb)
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Style.Declaration (Declaration, backgroundColor, borderRadius, color, fontSize, height, margin, padding, textAlign, width)
import Style.Declaration.Value (auto, center, em, pct, px, zero)
import Style.Render (inline)
import Style.Ruleset (Ruleset(..))
import Style.Ruleset as Ruleset
import Style.Selector (Selector(..))

unsafeFromArray :: Array ~> NonEmptyArray
unsafeFromArray = unsafePartial fromJust <<< fromArray

selectors :: NonEmptyArray Selector
selectors = unsafeFromArray
  [ TypeSelector "body"
  , ClassSelector "foo"
  , IDSelector "bar"
  ]

declarations :: NonEmptyArray Declaration
declarations = unsafeFromArray
  [ backgroundColor $ rgb 127 127 127
  , borderRadius $ 8.0 # px
  , color black
  , fontSize $ 16.0 # px
  , height $ 100.0 # pct
  , margin zero
  , padding $ 2.0 # em
  , textAlign center
  , width auto
  ]

ruleset :: Ruleset
ruleset = Ruleset selectors declarations

main :: Effect Unit
main = do
  log ""
  log $ inline declarations
  log ""
  log $ Ruleset.render ruleset
  log ""
