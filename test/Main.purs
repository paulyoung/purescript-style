module Test.Main where

import Prelude hiding (zero)

import Color (black, rgb, white)
import Color.Scheme.HTML (blue, green, red, silver, yellow)
import Color.Scheme.X11 (gold, orange)
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Maybe (Maybe, maybe)
import Effect (Effect)
import Effect.Console (log)
import Style.Declaration (Declaration)
import Style.Declaration as CSS
import Style.Declaration.Value (auto, boxShadow_, boxShadow_', center, currentColor, dashed, dotted, double, em, groove, hidden, in_, inset, invert, mm, none, number_, outset, pct, px, rem, ridge, solid, zero)
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
declarations = fromArray $
  [ CSS.backgroundColor $ rgb 127 127 127
  , CSS.border' (1.0 # px) solid currentColor
  , CSS.border (1.0 # px) solid gold
  , CSS.borderTop (2.0 # px) dashed silver
  , CSS.borderRight (3.0 # px) dotted red
  , CSS.borderBottom (4.0 # px) double green
  , CSS.borderLeft (5.0 # px) groove blue
  , CSS.borderWidth (6.0 # px) (2.0 # px) (3.0 # px) (4.0 # px)
  , CSS.borderStyle hidden inset none outset
  , CSS.borderColor red orange yellow green
  , CSS.borderTopWidth $ 7.0 # px
  , CSS.borderTopStyle ridge
  , CSS.borderTopColor white
  , CSS.boxShadow
      [ boxShadow_ false (3.0 # px) (3.0 # px) zero zero red
      , boxShadow_' true (-1.0 # em) zero zero (0.4 # em) currentColor
      ]
  , CSS.color black
  , CSS.fontSize $ 16.0 # px
  , CSS.fontWeight $ number_ 300.0
  , CSS.height $ 100.0 # pct
  , CSS.margin auto (8.0 # px) (50.0 # pct) zero
  , CSS.marginTop auto
  , CSS.marginRight $ 8.0 # px
  , CSS.marginBottom $ 50.0 # pct
  , CSS.marginLeft zero
  , CSS.outline' (1.0 # px) solid invert
  , CSS.outline (1.0 # px) solid gold
  , CSS.outlineWidth $ 1.0 # px
  , CSS.outlineStyle solid
  , CSS.outlineColor gold
  , CSS.textAlign center
  , CSS.width auto
  ]
  <> CSS.borderRadius (8.0 # px) (8.0 # px) (8.0 # px) (8.0 # px)
  <> CSS.padding (1.0 # em) (2.0 # in_) (3.0 # mm) (4.0 # rem)

ruleset :: Maybe Ruleset
ruleset = Ruleset <$> selectors <*> declarations

main :: Effect Unit
main = do
  log ""
  log $ maybe "" inline declarations
  log ""
  log $ maybe "" Ruleset.render ruleset
  log ""
