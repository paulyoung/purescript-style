module Style.Declaration.Value where

import Prelude

import Color (cssStringRGBA)
import Color as C
import Data.Number.Format as Number
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, case_, inj, on)
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

render :: Value -> String
render =
  case_
    # renderAuto
    >>> renderCenter
    >>> renderColor
    >>> renderEm
    >>> renderJustified
    >>> renderLeft
    >>> renderPct
    >>> renderPx
    >>> renderRight
    >>> renderZero

type Auto v = (auto :: Unit | v)

_auto = SProxy :: SProxy "auto"

auto :: forall v. Variant (Auto v)
auto = inj _auto unit

renderAuto :: forall v. (Variant v -> String) -> Variant (Auto v) -> String
renderAuto = on _auto $ const "auto"


type Center v = (center :: Unit | v)

_center = SProxy :: SProxy "center"

center :: forall v. Variant (Center v)
center = inj _center unit

renderCenter :: forall v. (Variant v -> String) -> Variant (Center v) -> String
renderCenter = on _center $ const "center"


type Color v = (color :: C.Color | v)

_color = SProxy :: SProxy "color"

color :: forall v. C.Color -> Variant (Color v)
color = inj _color

renderColor :: forall v. (Variant v -> String) -> Variant (Color v) -> String
renderColor = on _color cssStringRGBA


type Em v = (em :: Number | v)

_em = SProxy :: SProxy "em"

em :: forall v. Number -> Variant (Em v)
em = inj _em

renderEm :: forall v. (Variant v -> String) -> Variant (Em v) -> String
renderEm = on _em \n -> Number.toString n <> "em"


type Justified v = (justified :: Unit | v)

_justified = SProxy :: SProxy "justified"

justified :: forall v. Variant (Justified v)
justified = inj _justified unit

renderJustified :: forall v. (Variant v -> String) -> Variant (Justified v) -> String
renderJustified = on _justified $ const "justified"


type Left v = (left :: Unit | v)

_left = SProxy :: SProxy "left"

left :: forall v. Variant (Left v)
left = inj _left unit

renderLeft :: forall v. (Variant v -> String) -> Variant (Left v) -> String
renderLeft = on _left $ const "left"


type Pct v = (pct :: Number | v)

_pct = SProxy :: SProxy "pct"

pct :: forall v. Number -> Variant (Pct v)
pct = inj _pct

renderPct :: forall v. (Variant v -> String) -> Variant (Pct v) -> String
renderPct = on _pct \n -> Number.toString n <> "pct"


type Px v = (px :: Number | v)

_px = SProxy :: SProxy "px"

px :: forall v. Number -> Variant (Px v)
px = inj _px

renderPx :: forall v. (Variant v -> String) -> Variant (Px v) -> String
renderPx = on _px \n -> Number.toString n <> "px"


type Right v = (right :: Unit | v)

_right = SProxy :: SProxy "right"

right :: forall v. Variant (Right v)
right = inj _right unit

renderRight :: forall v. (Variant v -> String) -> Variant (Right v) -> String
renderRight = on _right $ const "right"


type Zero v = (zero :: Unit | v)

_zero = SProxy :: SProxy "zero"

zero :: forall v. Variant (Zero v)
zero = inj _zero unit

renderZero :: forall v. (Variant v -> String) -> Variant (Zero v) -> String
renderZero = on _zero $ const "zero"
