module Style.Value where

import Prelude

import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)


_auto = SProxy :: SProxy "auto"

auto :: forall v. Variant (auto :: Unit | v)
auto = inj _auto unit


_pct = SProxy :: SProxy "pct"

pct :: forall v. Number -> Variant (pct :: Number | v)
pct = inj _pct


_px = SProxy :: SProxy "px"

px :: forall v. Number -> Variant (px :: Number | v)
px x = inj _px x
