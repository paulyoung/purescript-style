module Style.Property.Value where

import Prelude

import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Type.Row (type (+))

type Value =
  Variant
   ( Auto
   + Percent
   + Pixels
   + ()
   )


type Auto v = (auto :: Unit | v)

_auto = SProxy :: SProxy "auto"

auto :: forall v. Variant (Auto v)
auto = inj _auto unit


type Percent v = (pct :: Number | v)

_pct = SProxy :: SProxy "pct"

pct :: forall v. Number -> Variant (Percent v)
pct = inj _pct


type Pixels v = (px :: Number | v)

_px = SProxy :: SProxy "px"

px :: forall v. Number -> Variant (Pixels v)
px x = inj _px x
