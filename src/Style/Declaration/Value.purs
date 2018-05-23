module Style.Declaration.Value where

import Prelude

import Color as C
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Type.Row (type (+))

type Value =
  Variant
   ( Auto
   + Center
   + Color
   + Em
   + Justified
   + Left
   + Pct
   + Px
   + Right
   + Zero
   + ()
   )


type Auto v = (auto :: Unit | v)

_auto = SProxy :: SProxy "auto"

auto :: forall v. Variant (Auto v)
auto = inj _auto unit


type Center v = (center :: Unit | v)

_center = SProxy :: SProxy "center"

center :: forall v. Variant (Center v)
center = inj _center unit


type Color v = (color :: C.Color | v)

_color = SProxy :: SProxy "color"

color :: forall v. C.Color -> Variant (Color v)
color = inj _color


type Em v = (em :: Number | v)

_em = SProxy :: SProxy "em"

em :: forall v. Number -> Variant (Em v)
em = inj _em


type Justified v = (justified :: Unit | v)

_justified = SProxy :: SProxy "justified"

justified :: forall v. Variant (Justified v)
justified = inj _justified unit


type Left v = (left :: Unit | v)

_left = SProxy :: SProxy "left"

left :: forall v. Variant (Left v)
left = inj _left unit


type Pct v = (pct :: Number | v)

_pct = SProxy :: SProxy "pct"

pct :: forall v. Number -> Variant (Pct v)
pct = inj _pct


type Px v = (px :: Number | v)

_px = SProxy :: SProxy "px"

px :: forall v. Number -> Variant (Px v)
px = inj _px


type Right v = (right :: Unit | v)

_right = SProxy :: SProxy "right"

right :: forall v. Variant (Right v)
right = inj _right unit


type Zero v = (zero :: Unit | v)

_zero = SProxy :: SProxy "zero"

zero :: forall v. Variant (Zero v)
zero = inj _zero unit
