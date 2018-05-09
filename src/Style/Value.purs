module Style.Value where

import Prelude

import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)


data Value
  = Auto
  | Percent Number
  | Pixels Number

derive instance eqValue :: Eq Value
derive instance ordValue :: Ord Value

instance showValue :: Show Value where
  show = case _ of
    Auto -> "Auto"
    Percent n -> "(Percent " <> show n <> ")"
    Pixels n -> "(Pixels " <> show n <> ")"


_auto = SProxy :: SProxy "auto"

auto :: forall v. Variant (auto :: Unit | v)
auto = inj _auto unit


_pct = SProxy :: SProxy "pct"

pct :: forall v. Number -> Variant (pct :: Number | v)
pct = inj _pct


_px = SProxy :: SProxy "px"

px :: forall v. Number -> Variant (px :: Number | v)
px x = inj _px x
