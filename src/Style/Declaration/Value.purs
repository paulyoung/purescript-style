module Style.Declaration.Value where

import Prelude

import Color (cssStringRGBA)
import Color as C
import Data.Array as Array
import Data.Number.Format as Number
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, case_, inj, on)
import Type.Row (type (+))

type Value =
  Variant
   ( Auto
   + Bold
   + Bolder
   + BoxShadow
   + Center
   + Ch
   + Cm
   + Color_
   + CurrentColor
   + Dashed
   + Dotted
   + Double
   + Em
   + Ex
   + Groove
   + Hidden
   + In
   + Inherit
   + Initial
   + Inset
   + Invert
   + Justify
   + JustifyAll
   + Large
   + Larger
   + Left
   + Lighter
   + Medium
   + Mm
   + None
   + Normal
   + Number_
   + Outset
   + Pc
   + Pct
   + Pt
   + Px
   + Rem
   + Ridge
   + Right
   + Small
   + Smaller
   + Solid
   + Thick
   + Thin
   + Transparent
   + Unset
   + Vh
   + Vmax
   + Vmin
   + Vw
   + XLarge
   + XSmall
   + XxLarge
   + XxSmall
   + Zero
   + ()
   )

render :: Value -> String
render =
  case_
    # renderAuto
    >>> renderBold
    >>> renderBolder
    >>> renderBoxShadow
    >>> renderCenter
    >>> renderCh
    >>> renderCm
    >>> renderColor_
    >>> renderCurrentColor
    >>> renderDashed
    >>> renderDotted
    >>> renderDouble
    >>> renderEm
    >>> renderEx
    >>> renderGroove
    >>> renderHidden
    >>> renderIn
    >>> renderInherit
    >>> renderInitial
    >>> renderInset
    >>> renderJustify
    >>> renderJustifyAll
    >>> renderInvert
    >>> renderLarge
    >>> renderLarger
    >>> renderLeft
    >>> renderLighter
    >>> renderMedium
    >>> renderMm
    >>> renderNone
    >>> renderNormal
    >>> renderNumber_
    >>> renderOutset
    >>> renderPc
    >>> renderPct
    >>> renderPt
    >>> renderPx
    >>> renderRem
    >>> renderRidge
    >>> renderRight
    >>> renderSmall
    >>> renderSmaller
    >>> renderSolid
    >>> renderThick
    >>> renderThin
    >>> renderTransparent
    >>> renderUnset
    >>> renderVh
    >>> renderVmax
    >>> renderVmin
    >>> renderVw
    >>> renderXLarge
    >>> renderXSmall
    >>> renderXxLarge
    >>> renderXxSmall
    >>> renderZero

type AbsoluteLength r =
  ( Cm
  + In
  + Mm
  + Pc
  + Pt
  + Px
  + r
  )

renderAbsoluteLength
  :: forall v
   . (Variant v -> String)
  -> Variant (AbsoluteLength v)
  -> String
renderAbsoluteLength =
  renderCm
    >>> renderIn
    >>> renderMm
    >>> renderPc
    >>> renderPt
    >>> renderPx


type AbsoluteSize r =
  ( XxSmall
  + XSmall
  + Small
  + Medium
  + Large
  + XLarge
  + XxLarge
  + r
  )

renderAbsoluteSize
  :: forall v
   . (Variant v -> String)
  -> Variant (AbsoluteSize v)
  -> String
renderAbsoluteSize =
  renderXxSmall
    >>> renderXSmall
    >>> renderSmall
    >>> renderMedium
    >>> renderLarge
    >>> renderXLarge
    >>> renderXxLarge


type BorderWidthKeyword r =
  ( Medium
  + Thick
  + Thin
  + r
  )

renderBorderWidthKeyword
  :: forall v
   . (Variant v -> String)
  -> Variant (BorderWidthKeyword v)
  -> String
renderBorderWidthKeyword =
  renderMedium
    >>> renderThick
    >>> renderThin


type Color r =
  ( Color_
  + CurrentColor
  + Transparent
  + r
  )

renderColor
  :: forall v
   . (Variant v -> String)
  -> Variant (Color v)
  -> String
renderColor =
  renderColor_
    >>> renderCurrentColor
    >>> renderTransparent


type FontRelativeLength r =
  ( Ch
  + Em
  + Ex
  + Rem
  + r
  )

renderFontRelativeLength
  :: forall v
   . (Variant v -> String)
  -> Variant (FontRelativeLength v)
  -> String
renderFontRelativeLength =
  renderCh
    >>> renderEm
    >>> renderEx
    >>> renderRem


type FontWeightKeyword r =
  ( Bold
  + Normal
  + r
  )

renderFontWeightKeyword
  :: forall v
   . (Variant v -> String)
  -> Variant (FontWeightKeyword v)
  -> String
renderFontWeightKeyword = renderBold >>> renderNormal


type FontWeightKeywordRelative r =
  ( Bolder
  + Lighter
  + r
  )

renderFontWeightKeywordRelative
  :: forall v
   . (Variant v -> String)
  -> Variant (FontWeightKeywordRelative v)
  -> String
renderFontWeightKeywordRelative = renderBolder >>> renderLighter


type Global r =
  ( Inherit
  + Initial
  + Unset
  + r
  )

renderGlobal
  :: forall v
   . (Variant v -> String)
  -> Variant (Global v)
  -> String
renderGlobal =
  renderInherit
    >>> renderInitial
    >>> renderUnset


type Length r =
  ( AbsoluteLength
  + FontRelativeLength
  + ViewportPercentageLength
  + r
  )

renderLength
  :: forall v
   . (Variant v -> String)
  -> Variant (Length v)
  -> String
renderLength =
  renderAbsoluteLength
    >>> renderFontRelativeLength
    >>> renderViewportPercentageLength


type Length_ r =
  ( Length
  + Zero
  + r
  )

renderLength_
  :: forall v
   . (Variant v -> String)
  -> Variant (Length_ v)
  -> String
renderLength_ = renderLength >>> renderZero


type OutlineWidthKeyword r =
  ( Medium
  + Thick
  + Thin
  + r
  )

renderOutlineWidthKeyword
  :: forall v
   . (Variant v -> String)
  -> Variant (OutlineWidthKeyword v)
  -> String
renderOutlineWidthKeyword =
  renderMedium
    >>> renderThick
    >>> renderThin


type RelativeSize r =
  ( Smaller
  + Larger
  + r
  )

renderRelativeSize
  :: forall v
   . (Variant v -> String)
  -> Variant (RelativeSize v)
  -> String
renderRelativeSize = renderLarger >>> renderSmaller


type ViewportPercentageLength r =
  ( Vh
  + Vmax
  + Vmin
  + Vw
  + r
  )

renderViewportPercentageLength
  :: forall v
   . (Variant v -> String)
  -> Variant (ViewportPercentageLength v)
  -> String
renderViewportPercentageLength =
  renderVh
    >>> renderVmax
    >>> renderVmin
    >>> renderVw


type Auto v = (auto :: Unit | v)

_auto = SProxy :: SProxy "auto"

auto :: forall v. Variant (Auto v)
auto = inj _auto unit

renderAuto :: forall v. (Variant v -> String) -> Variant (Auto v) -> String
renderAuto = on _auto $ const "auto"


type Bold v = (bold :: Unit | v)

_bold = SProxy :: SProxy "bold"

bold :: forall v. Variant (Bold v)
bold = inj _bold unit

renderBold :: forall v. (Variant v -> String) -> Variant (Bold v) -> String
renderBold = on _bold $ const "bold"


type Bolder v = (bolder :: Unit | v)

_bolder = SProxy :: SProxy "bolder"

bolder :: forall v. Variant (Bolder v)
bolder = inj _bolder unit

renderBolder :: forall v. (Variant v -> String) -> Variant (Bolder v) -> String
renderBolder = on _bolder $ const "bolder"


type BoxShadow_ =
  { inset :: Boolean
  , offsetX :: Variant (Length_ ())
  , offsetY :: Variant (Length_ ())
  , blurRadius :: Variant (Length_ ())
  , spreadRadius :: Variant (Length_ ())
  , color :: Variant (Color ())
  }

boxShadow_'
  :: Boolean
  -> Variant (Length_ ())
  -> Variant (Length_ ())
  -> Variant (Length_ ())
  -> Variant (Length_ ())
  -> Variant (Color ())
  -> BoxShadow_
boxShadow_' =
  { inset: _
  , offsetX: _
  , offsetY: _
  , blurRadius: _
  , spreadRadius: _
  , color: _
  }

boxShadow_
  :: Boolean
  -> Variant (Length_ ())
  -> Variant (Length_ ())
  -> Variant (Length_ ())
  -> Variant (Length_ ())
  -> C.Color
  -> BoxShadow_
boxShadow_ i x y b s = boxShadow_' i x y b s <<< color_


type BoxShadow v = (boxShadow :: Array BoxShadow_ | v)

_boxShadow = SProxy :: SProxy "boxShadow"

boxShadow :: forall v. Array BoxShadow_ -> Variant (BoxShadow v)
boxShadow = inj _boxShadow

renderBoxShadow
  :: forall v
   . (Variant v -> String)
  -> Variant (BoxShadow v)
  -> String
renderBoxShadow = on _boxShadow renderShadows
  where

  renderShadows :: Array BoxShadow_ -> String
  renderShadows = Array.intercalate ", " <<< map renderShadow

  renderShadow :: BoxShadow_ -> String
  renderShadow shadow =
    shadowInset <> Array.intercalate " "
      [ renderLength_ case_ $ shadow.offsetX
      , renderLength_ case_ $ shadow.offsetY
      , renderLength_ case_ $ shadow.blurRadius
      , renderLength_ case_ $ shadow.spreadRadius
      , renderColor case_ $ shadow.color
      ]

    where

    shadowInset :: String
    shadowInset
      | shadow.inset = "inset "
      | otherwise = ""


type Center v = (center :: Unit | v)

_center = SProxy :: SProxy "center"

center :: forall v. Variant (Center v)
center = inj _center unit

renderCenter :: forall v. (Variant v -> String) -> Variant (Center v) -> String
renderCenter = on _center $ const "center"


type Color_ v = (color_ :: C.Color | v)

_color_ = SProxy :: SProxy "color_"

color_ :: forall v. C.Color -> Variant (Color v)
color_ = inj _color_

renderColor_ :: forall v. (Variant v -> String) -> Variant (Color_ v) -> String
renderColor_ = on _color_ cssStringRGBA


type CurrentColor v = (currentColor :: Unit | v)

_currentColor = SProxy :: SProxy "currentColor"

currentColor :: forall v. Variant (CurrentColor v)
currentColor = inj _currentColor unit

renderCurrentColor :: forall v. (Variant v -> String) -> Variant (CurrentColor v) -> String
renderCurrentColor = on _currentColor $ const "currentColor"


type Ch v = (ch :: Number | v)

_ch = SProxy :: SProxy "ch"

ch :: forall v. Number -> Variant (Ch v)
ch = inj _ch

renderCh :: forall v. (Variant v -> String) -> Variant (Ch v) -> String
renderCh = on _ch \n -> Number.toString n <> "ch"


type Cm v = (cm :: Number | v)

_cm = SProxy :: SProxy "cm"

cm :: forall v. Number -> Variant (Cm v)
cm = inj _cm

renderCm :: forall v. (Variant v -> String) -> Variant (Cm v) -> String
renderCm = on _cm \n -> Number.toString n <> "cm"


type Dashed v = (dashed :: Unit | v)

_dashed = SProxy :: SProxy "dashed"

dashed :: forall v. Variant (Dashed v)
dashed = inj _dashed unit

renderDashed :: forall v. (Variant v -> String) -> Variant (Dashed v) -> String
renderDashed = on _dashed $ const "dashed"


type Dotted v = (dotted :: Unit | v)

_dotted = SProxy :: SProxy "dotted"

dotted :: forall v. Variant (Dotted v)
dotted = inj _dotted unit

renderDotted :: forall v. (Variant v -> String) -> Variant (Dotted v) -> String
renderDotted = on _dotted $ const "dotted"


type Double v = (double :: Unit | v)

_double = SProxy :: SProxy "double"

double :: forall v. Variant (Double v)
double = inj _double unit

renderDouble :: forall v. (Variant v -> String) -> Variant (Double v) -> String
renderDouble = on _double $ const "double"


type Em v = (em :: Number | v)

_em = SProxy :: SProxy "em"

em :: forall v. Number -> Variant (Em v)
em = inj _em

renderEm :: forall v. (Variant v -> String) -> Variant (Em v) -> String
renderEm = on _em \n -> Number.toString n <> "em"


type Ex v = (ex :: Number | v)

_ex = SProxy :: SProxy "ex"

ex :: forall v. Number -> Variant (Ex v)
ex = inj _ex

renderEx :: forall v. (Variant v -> String) -> Variant (Ex v) -> String
renderEx = on _ex \n -> Number.toString n <> "ex"


type In v = (in :: Number | v)

_in = SProxy :: SProxy "in"

in_ :: forall v. Number -> Variant (In v)
in_ = inj _in

renderIn :: forall v. (Variant v -> String) -> Variant (In v) -> String
renderIn = on _in \n -> Number.toString n <> "in"


type Inherit v = (inherit :: Unit | v)

_inherit = SProxy :: SProxy "inherit"

inherit :: forall v. Variant (Inherit v)
inherit = inj _inherit unit

renderInherit :: forall v. (Variant v -> String) -> Variant (Inherit v) -> String
renderInherit = on _inherit $ const "inherit"


type Initial v = (initial :: Unit | v)

_initial = SProxy :: SProxy "initial"

initial :: forall v. Variant (Initial v)
initial = inj _initial unit

renderInitial :: forall v. (Variant v -> String) -> Variant (Initial v) -> String
renderInitial = on _initial $ const "initial"


type Invert v = (invert :: Unit | v)

_invert = SProxy :: SProxy "invert"

invert :: forall v. Variant (Invert v)
invert = inj _invert unit

renderInvert :: forall v. (Variant v -> String) -> Variant (Invert v) -> String
renderInvert = on _invert $ const "invert"


type Justify v = (justify :: Unit | v)

_justify = SProxy :: SProxy "justify"

justify :: forall v. Variant (Justify v)
justify = inj _justify unit

renderJustify :: forall v. (Variant v -> String) -> Variant (Justify v) -> String
renderJustify = on _justify $ const "justify"


type JustifyAll v = (justifyAll :: Unit | v)

_justifyAll = SProxy :: SProxy "justifyAll"

justifyAll :: forall v. Variant (JustifyAll v)
justifyAll = inj _justifyAll unit

renderJustifyAll :: forall v. (Variant v -> String) -> Variant (JustifyAll v) -> String
renderJustifyAll = on _justifyAll $ const "justify-all"


type Groove v = (groove :: Unit | v)

_groove = SProxy :: SProxy "groove"

groove :: forall v. Variant (Groove v)
groove = inj _groove unit

renderGroove :: forall v. (Variant v -> String) -> Variant (Groove v) -> String
renderGroove = on _groove $ const "groove"


type Hidden v = (hidden :: Unit | v)

_hidden = SProxy :: SProxy "hidden"

hidden :: forall v. Variant (Hidden v)
hidden = inj _hidden unit

renderHidden :: forall v. (Variant v -> String) -> Variant (Hidden v) -> String
renderHidden = on _hidden $ const "hidden"


type Inset v = (inset :: Unit | v)

_inset = SProxy :: SProxy "inset"

inset :: forall v. Variant (Inset v)
inset = inj _inset unit

renderInset :: forall v. (Variant v -> String) -> Variant (Inset v) -> String
renderInset = on _inset $ const "inset"


type Large v = (large :: Unit | v)

_large = SProxy :: SProxy "large"

large :: forall v. Variant (Large v)
large = inj _large unit

renderLarge :: forall v. (Variant v -> String) -> Variant (Large v) -> String
renderLarge = on _large $ const "large"


type Larger v = (larger :: Unit | v)

_larger = SProxy :: SProxy "larger"

larger :: forall v. Variant (Larger v)
larger = inj _larger unit

renderLarger :: forall v. (Variant v -> String) -> Variant (Larger v) -> String
renderLarger = on _larger $ const "larger"


type Left v = (left :: Unit | v)

_left = SProxy :: SProxy "left"

left :: forall v. Variant (Left v)
left = inj _left unit

renderLeft :: forall v. (Variant v -> String) -> Variant (Left v) -> String
renderLeft = on _left $ const "left"


type Lighter v = (lighter :: Unit | v)

_lighter = SProxy :: SProxy "lighter"

lighter :: forall v. Variant (Lighter v)
lighter = inj _lighter unit

renderLighter :: forall v. (Variant v -> String) -> Variant (Lighter v) -> String
renderLighter = on _lighter $ const "lighter"


type Medium v = (medium :: Unit | v)

_medium = SProxy :: SProxy "medium"

medium :: forall v. Variant (Medium v)
medium = inj _medium unit

renderMedium :: forall v. (Variant v -> String) -> Variant (Medium v) -> String
renderMedium = on _medium $ const "medium"


type Mm v = (mm :: Number | v)

_mm = SProxy :: SProxy "mm"

mm :: forall v. Number -> Variant (Mm v)
mm = inj _mm

renderMm :: forall v. (Variant v -> String) -> Variant (Mm v) -> String
renderMm = on _mm \n -> Number.toString n <> "mm"


type None v = (none :: Unit | v)

_none = SProxy :: SProxy "none"

none :: forall v. Variant (None v)
none = inj _none unit

renderNone :: forall v. (Variant v -> String) -> Variant (None v) -> String
renderNone = on _none $ const "none"


type Normal v = (normal :: Unit | v)

_normal = SProxy :: SProxy "normal"

normal :: forall v. Variant (Normal v)
normal = inj _normal unit

renderNormal :: forall v. (Variant v -> String) -> Variant (Normal v) -> String
renderNormal = on _normal $ const "normal"


type Number_ v = (number_ :: Number | v)

_number_ = SProxy :: SProxy "number_"

number_ :: forall v. Number -> Variant (Number_ v)
number_ = inj _number_

renderNumber_ :: forall v. (Variant v -> String) -> Variant (Number_ v) -> String
renderNumber_ = on _number_ Number.toString


type Outset v = (outset :: Unit | v)

_outset = SProxy :: SProxy "outset"

outset :: forall v. Variant (Outset v)
outset = inj _outset unit

renderOutset :: forall v. (Variant v -> String) -> Variant (Outset v) -> String
renderOutset = on _outset $ const "outset"


type Pc v = (pc :: Number | v)

_pc = SProxy :: SProxy "pc"

pc :: forall v. Number -> Variant (Pc v)
pc = inj _pc

renderPc :: forall v. (Variant v -> String) -> Variant (Pc v) -> String
renderPc = on _pc \n -> Number.toString n <> "pc"


type Pct v = (pct :: Number | v)

_pct = SProxy :: SProxy "pct"

pct :: forall v. Number -> Variant (Pct v)
pct = inj _pct

renderPct :: forall v. (Variant v -> String) -> Variant (Pct v) -> String
renderPct = on _pct \n -> Number.toString n <> "pct"


type Pt v = (pt :: Number | v)

_pt = SProxy :: SProxy "pt"

pt :: forall v. Number -> Variant (Pt v)
pt = inj _pt

renderPt :: forall v. (Variant v -> String) -> Variant (Pt v) -> String
renderPt = on _pt \n -> Number.toString n <> "pt"


type Px v = (px :: Number | v)

_px = SProxy :: SProxy "px"

px :: forall v. Number -> Variant (Px v)
px = inj _px

renderPx :: forall v. (Variant v -> String) -> Variant (Px v) -> String
renderPx = on _px \n -> Number.toString n <> "px"


type Rem v = (rem :: Number | v)

_rem = SProxy :: SProxy "rem"

rem :: forall v. Number -> Variant (Rem v)
rem = inj _rem

renderRem :: forall v. (Variant v -> String) -> Variant (Rem v) -> String
renderRem = on _rem \n -> Number.toString n <> "rem"


type Ridge v = (ridge :: Unit | v)

_ridge = SProxy :: SProxy "ridge"

ridge :: forall v. Variant (Ridge v)
ridge = inj _ridge unit

renderRidge :: forall v. (Variant v -> String) -> Variant (Ridge v) -> String
renderRidge = on _ridge $ const "ridge"


type Right v = (right :: Unit | v)

_right = SProxy :: SProxy "right"

right :: forall v. Variant (Right v)
right = inj _right unit

renderRight :: forall v. (Variant v -> String) -> Variant (Right v) -> String
renderRight = on _right $ const "right"


type Small v = (small :: Unit | v)

_small = SProxy :: SProxy "small"

small :: forall v. Variant (Small v)
small = inj _small unit

renderSmall :: forall v. (Variant v -> String) -> Variant (Small v) -> String
renderSmall = on _small $ const "small"


type Smaller v = (smaller :: Unit | v)

_smaller = SProxy :: SProxy "smaller"

smaller :: forall v. Variant (Smaller v)
smaller = inj _smaller unit

renderSmaller :: forall v. (Variant v -> String) -> Variant (Smaller v) -> String
renderSmaller = on _smaller $ const "smaller"


type Solid v = (solid :: Unit | v)

_solid = SProxy :: SProxy "solid"

solid :: forall v. Variant (Solid v)
solid = inj _solid unit

renderSolid :: forall v. (Variant v -> String) -> Variant (Solid v) -> String
renderSolid = on _solid $ const "solid"


type Thick v = (thick :: Unit | v)

_thick = SProxy :: SProxy "thick"

thick :: forall v. Variant (Thick v)
thick = inj _thick unit

renderThick :: forall v. (Variant v -> String) -> Variant (Thick v) -> String
renderThick = on _thick $ const "thick"


type Thin v = (thin :: Unit | v)

_thin = SProxy :: SProxy "thin"

thin :: forall v. Variant (Thin v)
thin = inj _thin unit

renderThin :: forall v. (Variant v -> String) -> Variant (Thin v) -> String
renderThin = on _thin $ const "thin"


type Transparent v = (transparent :: Unit | v)

_transparent = SProxy :: SProxy "transparent"

transparent :: forall v. Variant (Transparent v)
transparent = inj _transparent unit

renderTransparent :: forall v. (Variant v -> String) -> Variant (Transparent v) -> String
renderTransparent = on _transparent $ const "transparent"


type Unset v = (unset :: Unit | v)

_unset = SProxy :: SProxy "unset"

unset :: forall v. Variant (Unset v)
unset = inj _unset unit

renderUnset :: forall v. (Variant v -> String) -> Variant (Unset v) -> String
renderUnset = on _unset $ const "unset"


type Vh v = (vh :: Number | v)

_vh = SProxy :: SProxy "vh"

vh :: forall v. Number -> Variant (Vh v)
vh = inj _vh

renderVh :: forall v. (Variant v -> String) -> Variant (Vh v) -> String
renderVh = on _vh \n -> Number.toString n <> "vh"


type Vmax v = (vmax :: Number | v)

_vmax = SProxy :: SProxy "vmax"

vmax :: forall v. Number -> Variant (Vmax v)
vmax = inj _vmax

renderVmax :: forall v. (Variant v -> String) -> Variant (Vmax v) -> String
renderVmax = on _vmax \n -> Number.toString n <> "vmax"


type Vmin v = (vmin :: Number | v)

_vmin = SProxy :: SProxy "vmin"

vmin :: forall v. Number -> Variant (Vmin v)
vmin = inj _vmin

renderVmin :: forall v. (Variant v -> String) -> Variant (Vmin v) -> String
renderVmin = on _vmin \n -> Number.toString n <> "vmin"


type Vw v = (vw :: Number | v)

_vw = SProxy :: SProxy "vw"

vw :: forall v. Number -> Variant (Vw v)
vw = inj _vw

renderVw :: forall v. (Variant v -> String) -> Variant (Vw v) -> String
renderVw = on _vw \n -> Number.toString n <> "vw"


type XLarge v = (xLarge :: Unit | v)

_xLarge = SProxy :: SProxy "xLarge"

xLarge :: forall v. Variant (XLarge v)
xLarge = inj _xLarge unit

renderXLarge :: forall v. (Variant v -> String) -> Variant (XLarge v) -> String
renderXLarge = on _xLarge $ const "x-large"


type XSmall v = (xSmall :: Unit | v)

_xSmall = SProxy :: SProxy "xSmall"

xSmall :: forall v. Variant (XSmall v)
xSmall = inj _xSmall unit

renderXSmall :: forall v. (Variant v -> String) -> Variant (XSmall v) -> String
renderXSmall = on _xSmall $ const "x-small"


type XxLarge v = (xxLarge :: Unit | v)

_xxLarge = SProxy :: SProxy "xxLarge"

xxLarge :: forall v. Variant (XxLarge v)
xxLarge = inj _xxLarge unit

renderXxLarge :: forall v. (Variant v -> String) -> Variant (XxLarge v) -> String
renderXxLarge = on _xxLarge $ const "xx-large"


type XxSmall v = (xxSmall :: Unit | v)

_xxSmall = SProxy :: SProxy "xxSmall"

xxSmall :: forall v. Variant (XxSmall v)
xxSmall = inj _xxSmall unit

renderXxSmall :: forall v. (Variant v -> String) -> Variant (XxSmall v) -> String
renderXxSmall = on _xxSmall $ const "xx-small"


type Zero v = (zero :: Unit | v)

_zero = SProxy :: SProxy "zero"

zero :: forall v. Variant (Zero v)
zero = inj _zero unit

renderZero :: forall v. (Variant v -> String) -> Variant (Zero v) -> String
renderZero = on _zero $ const "0"
