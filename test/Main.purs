module Test.Main where

import Prelude hiding (zero)

import Color (black, rgb)
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Maybe (Maybe, maybe)
import Effect (Effect)
import Effect.Console (log)
import Style.Declaration (Declaration, backgroundColor, borderRadius, color, fontSize, fontWeight, height, margin, padding, textAlign, width)
import Style.Declaration.Value (auto, center, bold, em, number, pct, px, zero)
import Style.Render (inline)
import Style.Ruleset (Ruleset(..))
import Style.Ruleset as Ruleset
import Style.Selector (Selector(..))

selectors :: Maybe (NonEmptyArray Selector)
selectors = fromArray
  [ TypeSelector "body"
  , ClassSelector "foo"
  , IDSelector "bar"
  ]

declarations :: Maybe (NonEmptyArray Declaration)
declarations = fromArray
  [ backgroundColor $ rgb 127 127 127
  , borderRadius $ 8.0 # px
  , color black
  , fontSize $ 16.0 # px
  , fontWeight $ number 300.0
  , height $ 100.0 # pct
  , margin zero
  , padding $ 2.0 # em
  , textAlign center
  , width auto
  ]

ruleset :: Maybe Ruleset
ruleset = Ruleset <$> selectors <*> declarations

main :: Effect Unit
main = do
  log ""
  log $ maybe "" inline declarations
  log ""
  log $ maybe "" Ruleset.render ruleset
  log ""
