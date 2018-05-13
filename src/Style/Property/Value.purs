module Style.Property.Value where

import Prelude

import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Type.Row (type (+))

type Value =
  Variant
   ( Auto
   + Em
   + Pct
   + Px
   + ()
   )


type Auto v = (auto :: Unit | v)

_auto = SProxy :: SProxy "auto"

auto :: forall v. Variant (Auto v)
auto = inj _auto unit


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
px x = inj _px x
