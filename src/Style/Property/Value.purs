module Style.Property.Value where

import Prelude

import Color as C
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Type.Row (type (+))

type Value =
  Variant
   ( Auto
   + Color
   + Em
   + Pct
   + Px
   + Zero
   + ()
   )


type Auto v = (auto :: Unit | v)

_auto = SProxy :: SProxy "auto"

auto :: forall v. Variant (Auto v)
auto = inj _auto unit


type Color v = (color :: C.Color | v)

_color = SProxy :: SProxy "color"

color :: forall v. C.Color -> Variant (Color v)
color = inj _color


type Em v = (em :: Number | v)

_em = SProxy :: SProxy "em"

em :: forall v. Number -> Variant (Em v)
em = inj _em


type Pct v = (pct :: Number | v)

_pct = SProxy :: SProxy "pct"

pct :: forall v. Number -> Variant (Pct v)
pct = inj _pct


type Px v = (px :: Number | v)

_px = SProxy :: SProxy "px"

px :: forall v. Number -> Variant (Px v)
px = inj _px


type Zero v = (zero :: Unit | v)

_zero = SProxy :: SProxy "zero"

zero :: forall v. Variant (Zero v)
zero = inj _zero unit
