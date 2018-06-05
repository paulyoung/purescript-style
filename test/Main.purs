module Test.Main where

import Prelude hiding (zero)

import Color (black, rgb)
import Effect (Effect)
import Effect.Console (log)
import Style.Declaration (Declaration, backgroundColor, color, fontSize, height, margin, padding, textAlign, width)
import Style.Declaration.Value (auto, center, em, pct, px, zero)
import Style.Render (inline)
import Style.Ruleset (Ruleset(..))
import Style.Ruleset as Ruleset
import Style.Selector (Selector(..))

selectors :: Array Selector
selectors =
  [ TypeSelector "body"
  , ClassSelector "foo"
  , IDSelector "bar"
  ]

declarations :: Array Declaration
declarations =
  [ backgroundColor $ rgb 127 127 127
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
