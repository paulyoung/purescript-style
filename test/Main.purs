module Test.Main where

import Prelude hiding (zero)

import Color (black, rgb)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Style.Property (backgroundColor, color, fontSize, height, margin, padding, width)
import Style.Property.Value (auto, em, pct, px, zero)
import Style.Render (inline)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $
inline
  [ backgroundColor $ rgb 127 127 127
  , color black
  , fontSize $ 16.0 # px
  , height $ 100.0 # pct
  , margin zero
  , padding $ 2.0 # em
  , width auto
  ]
