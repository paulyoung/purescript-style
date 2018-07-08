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
   + Border
   + BorderColor
   + BorderStyle
   + BorderWidth
   + BorderRadius
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
   + Margin
   + Medium
   + Mm
   + None
   + Normal
   + Number_
   + Outline
   + Outset
   + Padding
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
    >>> renderBorder
    >>> renderBorderColor
    >>> renderBorderStyle
    >>> renderBorderWidth
    >>> renderBorderRadius
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
    >>> renderMargin
    >>> renderMedium
    >>> renderMm
    >>> renderNone
    >>> renderNormal
    >>> renderNumber_
    >>> renderOutline
    >>> renderOutset
    >>> renderPadding
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


type AbsoluteLengthFields r =
  ( Cm
  + In
  + Mm
  + Pc
  + Pt
  + Px
  + r
  )

renderAbsoluteLengthFields
  :: forall v
   . (Variant v -> String)
  -> Variant (AbsoluteLengthFields v)
  -> String
renderAbsoluteLengthFields =
  renderCm
    >>> renderIn
    >>> renderMm
    >>> renderPc
    >>> renderPt
    >>> renderPx


type AbsoluteSizeFields r =
  ( XxSmall
  + XSmall
  + Small
  + Medium
  + Large
  + XLarge
  + XxLarge
  + r
  )

renderAbsoluteSizeFields
  :: forall v
   . (Variant v -> String)
  -> Variant (AbsoluteSizeFields v)
  -> String
renderAbsoluteSizeFields =
  renderXxSmall
    >>> renderXSmall
    >>> renderSmall
    >>> renderMedium
    >>> renderLarge
    >>> renderXLarge
    >>> renderXxLarge


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


type BorderRep =
  { width :: BorderWidthValue
  , style :: BorderStyleValue
  , color :: BorderColorValue
  }

type Border v = (border :: BorderRep | v)

_border = SProxy :: SProxy "border"

border :: forall v. BorderRep -> Variant (Border v)
border = inj _border

renderBorder
  :: forall v
   . (Variant v -> String)
  -> Variant (Border v)
  -> String
renderBorder = on _border \b ->
  Array.intercalate " "
    [ renderBorderWidthFields case_ $ b.width
    , renderBorderStyleFields case_ $ b.style
    , renderBorderColorFields case_ $ b.color
    ]


type BorderColorFields r =
  ( ColorFields
  + GlobalFields
  + r
  )

type BorderColorValue = Variant (BorderColorFields ())

type BorderColorRep =
  { top :: BorderColorValue
  , right :: BorderColorValue
  , bottom :: BorderColorValue
  , left :: BorderColorValue
  }

type BorderColor v = (borderColor :: BorderColorRep | v)

_borderColor = SProxy :: SProxy "borderColor"

borderColor :: forall v. BorderColorRep -> Variant (BorderColor v)
borderColor = inj _borderColor

renderBorderColorFields
  :: forall v
   . (Variant v -> String)
  -> Variant (BorderColorFields v)
  -> String
renderBorderColorFields = renderColorFields >>> renderGlobalFields

renderBorderColor
  :: forall v
   . (Variant v -> String)
  -> Variant (BorderColor v)
  -> String
renderBorderColor = on _borderColor \s ->
  Array.intercalate " "
    [ renderBorderColorFields case_ $ s.top
    , renderBorderColorFields case_ $ s.right
    , renderBorderColorFields case_ $ s.bottom
    , renderBorderColorFields case_ $ s.left
    ]


type BorderStyleFields r =
  ( Dashed
  + Dotted
  + Double
  + GlobalFields
  + Groove
  + Hidden
  + Inset
  + None
  + Outset
  + Ridge
  + Solid
  + r
  )

type BorderStyleValue = Variant (BorderStyleFields ())

type BorderStyleRep =
  { top :: BorderStyleValue
  , right :: BorderStyleValue
  , bottom :: BorderStyleValue
  , left :: BorderStyleValue
  }

type BorderStyle v = (borderStyle :: BorderStyleRep | v)

_borderStyle = SProxy :: SProxy "borderStyle"

borderStyle :: forall v. BorderStyleRep -> Variant (BorderStyle v)
borderStyle = inj _borderStyle

renderBorderStyleFields
  :: forall v
   . (Variant v -> String)
  -> Variant (BorderStyleFields v)
  -> String
renderBorderStyleFields =
  renderDashed
    >>> renderDotted
    >>> renderDouble
    >>> renderGlobalFields
    >>> renderGroove
    >>> renderHidden
    >>> renderInset
    >>> renderNone
    >>> renderOutset
    >>> renderRidge
    >>> renderSolid

renderBorderStyle
  :: forall v
   . (Variant v -> String)
  -> Variant (BorderStyle v)
  -> String
renderBorderStyle = on _borderStyle \s ->
  Array.intercalate " "
    [ renderBorderStyleFields case_ $ s.top
    , renderBorderStyleFields case_ $ s.right
    , renderBorderStyleFields case_ $ s.bottom
    , renderBorderStyleFields case_ $ s.left
    ]


type BorderWidthFields r =
  ( GlobalFields
  + LengthFields
  + BorderWidthKeywordFields
  + Zero
  + r
  )

type BorderWidthValue = Variant (BorderWidthFields ())

type BorderWidthRep =
  { top :: BorderWidthValue
  , right :: BorderWidthValue
  , bottom :: BorderWidthValue
  , left :: BorderWidthValue
  }

type BorderWidth v = (borderWidth :: BorderWidthRep | v)

_borderWidth = SProxy :: SProxy "borderWidth"

borderWidth :: forall v. BorderWidthRep -> Variant (BorderWidth v)
borderWidth = inj _borderWidth

renderBorderWidthFields
  :: forall v
   . (Variant v -> String)
  -> Variant (BorderWidthFields v)
  -> String
renderBorderWidthFields =
  renderBorderWidthKeywordFields
    >>> renderGlobalFields
    >>> renderLengthFields
    >>> renderZero

renderBorderWidth
  :: forall v
   . (Variant v -> String)
  -> Variant (BorderWidth v)
  -> String
renderBorderWidth = on _borderWidth \s ->
  Array.intercalate " "
    [ renderBorderWidthFields case_ $ s.top
    , renderBorderWidthFields case_ $ s.right
    , renderBorderWidthFields case_ $ s.bottom
    , renderBorderWidthFields case_ $ s.left
    ]

type BorderWidthKeywordFields r =
  ( Medium
  + Thick
  + Thin
  + r
  )

renderBorderWidthKeywordFields
  :: forall v
   . (Variant v -> String)
  -> Variant (BorderWidthKeywordFields v)
  -> String
renderBorderWidthKeywordFields =
  renderMedium
    >>> renderThick
    >>> renderThin


type BorderRadiusFields r =
  ( GlobalFields
  + LengthFields
  + Pct
  + Zero
  + r
  )

type BorderRadiusValue = Variant (BorderRadiusFields ())

type BorderRadiusRep =
  { topLeft :: BorderRadiusValue
  , topRight :: BorderRadiusValue
  , bottomRight :: BorderRadiusValue
  , bottomLeft :: BorderRadiusValue
  }

type BorderRadius v = (borderRadius :: BorderRadiusRep | v)

_borderRadius = SProxy :: SProxy "borderRadius"

borderRadius :: forall v. BorderRadiusRep -> Variant (BorderRadius v)
borderRadius = inj _borderRadius

renderBorderRadiusFields
  :: forall v
   . (Variant v -> String)
  -> Variant (BorderRadiusFields v)
  -> String
renderBorderRadiusFields =
  renderGlobalFields
    >>> renderLengthFields
    >>> renderPct
    >>> renderZero

renderBorderRadius
  :: forall v
   . (Variant v -> String)
  -> Variant (BorderRadius v)
  -> String
renderBorderRadius = on _borderRadius \r ->
  Array.intercalate " "
    [ renderBorderRadiusFields case_ $ r.topLeft
    , renderBorderRadiusFields case_ $ r.topRight
    , renderBorderRadiusFields case_ $ r.bottomRight
    , renderBorderRadiusFields case_ $ r.bottomLeft
    ]


type BoxShadowRep =
  { inset :: Boolean
  , offsetX :: Variant (LengthFields_ ())
  , offsetY :: Variant (LengthFields_ ())
  , blurRadius :: Variant (LengthFields_ ())
  , spreadRadius :: Variant (LengthFields_ ())
  , color :: Variant (ColorFields ())
  }

boxShadow_'
  :: Boolean
  -> Variant (LengthFields_ ())
  -> Variant (LengthFields_ ())
  -> Variant (LengthFields_ ())
  -> Variant (LengthFields_ ())
  -> Variant (ColorFields ())
  -> BoxShadowRep
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
  -> Variant (LengthFields_ ())
  -> Variant (LengthFields_ ())
  -> Variant (LengthFields_ ())
  -> Variant (LengthFields_ ())
  -> C.Color
  -> BoxShadowRep
boxShadow_ i x y b s = boxShadow_' i x y b s <<< color_


type BoxShadow v = (boxShadow :: Array BoxShadowRep | v)

_boxShadow = SProxy :: SProxy "boxShadow"

boxShadow :: forall v. Array BoxShadowRep -> Variant (BoxShadow v)
boxShadow = inj _boxShadow

renderBoxShadow
  :: forall v
   . (Variant v -> String)
  -> Variant (BoxShadow v)
  -> String
renderBoxShadow = on _boxShadow renderShadows
  where

  renderShadows :: Array BoxShadowRep -> String
  renderShadows = Array.intercalate ", " <<< map renderShadow

  renderShadow :: BoxShadowRep -> String
  renderShadow shadow =
    shadowInset <> Array.intercalate " "
      [ renderLengthFields_ case_ $ shadow.offsetX
      , renderLengthFields_ case_ $ shadow.offsetY
      , renderLengthFields_ case_ $ shadow.blurRadius
      , renderLengthFields_ case_ $ shadow.spreadRadius
      , renderColorFields case_ $ shadow.color
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


type ColorFields r =
  ( Color_
  + CurrentColor
  + Transparent
  + r
  )

renderColorFields
  :: forall v
   . (Variant v -> String)
  -> Variant (ColorFields v)
  -> String
renderColorFields =
  renderColor_
    >>> renderCurrentColor
    >>> renderTransparent


type Color_ v = (color_ :: C.Color | v)

_color_ = SProxy :: SProxy "color_"

color_ :: forall v. C.Color -> Variant (ColorFields v)
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


type FontRelativeLengthFields r =
  ( Ch
  + Em
  + Ex
  + Rem
  + r
  )

renderFontRelativeLengthFields
  :: forall v
   . (Variant v -> String)
  -> Variant (FontRelativeLengthFields v)
  -> String
renderFontRelativeLengthFields =
  renderCh
    >>> renderEm
    >>> renderEx
    >>> renderRem


type FontWeightKeywordFields r =
  ( Bold
  + Normal
  + r
  )

renderFontWeightKeywordFields
  :: forall v
   . (Variant v -> String)
  -> Variant (FontWeightKeywordFields v)
  -> String
renderFontWeightKeywordFields = renderBold >>> renderNormal


type FontWeightKeywordRelativeFields r =
  ( Bolder
  + Lighter
  + r
  )

renderFontWeightKeywordRelativeFields
  :: forall v
   . (Variant v -> String)
  -> Variant (FontWeightKeywordRelativeFields v)
  -> String
renderFontWeightKeywordRelativeFields = renderBolder >>> renderLighter


type GlobalFields r =
  ( Inherit
  + Initial
  + Unset
  + r
  )

renderGlobalFields
  :: forall v
   . (Variant v -> String)
  -> Variant (GlobalFields v)
  -> String
renderGlobalFields =
  renderInherit
    >>> renderInitial
    >>> renderUnset


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


type LengthFields r =
  ( AbsoluteLengthFields
  + FontRelativeLengthFields
  + ViewportPercentageLengthFields
  + r
  )

renderLengthFields
  :: forall v
   . (Variant v -> String)
  -> Variant (LengthFields v)
  -> String
renderLengthFields =
  renderAbsoluteLengthFields
    >>> renderFontRelativeLengthFields
    >>> renderViewportPercentageLengthFields


type LengthFields_ r =
  ( LengthFields
  + Zero
  + r
  )

renderLengthFields_
  :: forall v
   . (Variant v -> String)
  -> Variant (LengthFields_ v)
  -> String
renderLengthFields_ = renderLengthFields >>> renderZero


type Lighter v = (lighter :: Unit | v)

_lighter = SProxy :: SProxy "lighter"

lighter :: forall v. Variant (Lighter v)
lighter = inj _lighter unit

renderLighter :: forall v. (Variant v -> String) -> Variant (Lighter v) -> String
renderLighter = on _lighter $ const "lighter"


type MarginFields r =
  ( Auto
  + GlobalFields
  + LengthFields
  + Pct
  + Zero
  + r
  )

type MarginValue = Variant (MarginFields ())

type MarginRep =
  { top :: MarginValue
  , right :: MarginValue
  , bottom :: MarginValue
  , left :: MarginValue
  }

type Margin v = (margin :: MarginRep | v)

_margin = SProxy :: SProxy "margin"

margin :: forall v. MarginRep -> Variant (Margin v)
margin = inj _margin

renderMarginFields
  :: forall v
   . (Variant v -> String)
  -> Variant (MarginFields v)
  -> String
renderMarginFields =
  renderAuto
    >>> renderGlobalFields
    >>> renderLengthFields
    >>> renderPct
    >>> renderZero

renderMargin
  :: forall v
   . (Variant v -> String)
  -> Variant (Margin v)
  -> String
renderMargin = on _margin \m ->
  Array.intercalate " "
    [ renderMarginFields case_ $ m.top
    , renderMarginFields case_ $ m.right
    , renderMarginFields case_ $ m.bottom
    , renderMarginFields case_ $ m.left
    ]


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


type Outline_ =
  { width :: OutlineWidthValue
  , style :: OutlineStyleValue
  , color :: OutlineColorValue
  }

type Outline v = (outline :: Outline_ | v)

_outline = SProxy :: SProxy "outline"

outline :: forall v. Outline_ -> Variant (Outline v)
outline = inj _outline

renderOutline
  :: forall v
   . (Variant v -> String)
  -> Variant (Outline v)
  -> String
renderOutline = on _outline \o ->
  Array.intercalate " "
    [ renderOutlineWidthFields case_ $ o.width
    , renderOutlineStyleFields case_ $ o.style
    , renderOutlineColorFields case_ $ o.color
    ]


type OutlineColorFields r =
  ( ColorFields
  + GlobalFields
  + Invert
  + r
  )

type OutlineColorValue = Variant (OutlineColorFields ())

renderOutlineColorFields
  :: forall v
   . (Variant v -> String)
  -> Variant (OutlineColorFields v)
  -> String
renderOutlineColorFields =
  renderColorFields
    >>> renderGlobalFields
    >>> renderInvert


type OutlineStyleFields r =
  ( Auto
  + Dashed
  + Dotted
  + Double
  + GlobalFields
  + Groove
  + Inset
  + None
  + Outset
  + Ridge
  + Solid
  + r
  )

type OutlineStyleValue = Variant (OutlineStyleFields ())

renderOutlineStyleFields
  :: forall v
   . (Variant v -> String)
  -> Variant (OutlineStyleFields v)
  -> String
renderOutlineStyleFields =
  renderAuto
    >>> renderDashed
    >>> renderDotted
    >>> renderDouble
    >>> renderGlobalFields
    >>> renderGroove
    >>> renderInset
    >>> renderNone
    >>> renderOutset
    >>> renderRidge
    >>> renderSolid


type OutlineWidthFields r =
  ( GlobalFields
  + LengthFields
  + OutlineWidthKeywordFields
  + Zero
  + r
  )

type OutlineWidthValue = Variant (OutlineWidthFields ())

renderOutlineWidthFields
  :: forall v
   . (Variant v -> String)
  -> Variant (OutlineWidthFields v)
  -> String
renderOutlineWidthFields =
  renderGlobalFields
    >>> renderLengthFields
    >>> renderOutlineWidthKeyword
    >>> renderZero


type OutlineWidthKeywordFields r =
  ( Medium
  + Thick
  + Thin
  + r
  )

renderOutlineWidthKeyword
  :: forall v
   . (Variant v -> String)
  -> Variant (OutlineWidthKeywordFields v)
  -> String
renderOutlineWidthKeyword =
  renderMedium
    >>> renderThick
    >>> renderThin


type Outset v = (outset :: Unit | v)

_outset = SProxy :: SProxy "outset"

outset :: forall v. Variant (Outset v)
outset = inj _outset unit

renderOutset :: forall v. (Variant v -> String) -> Variant (Outset v) -> String
renderOutset = on _outset $ const "outset"


type PaddingFields r =
  ( GlobalFields
  + LengthFields
  + Pct
  + Zero
  + r
  )

type PaddingValue = Variant (PaddingFields ())

type PaddingRep =
  { top :: PaddingValue
  , right :: PaddingValue
  , bottom :: PaddingValue
  , left :: PaddingValue
  }

type Padding v = (padding :: PaddingRep | v)

_padding = SProxy :: SProxy "padding"

padding :: forall v. PaddingRep -> Variant (Padding v)
padding = inj _padding

renderPaddingFields
  :: forall v
   . (Variant v -> String)
  -> Variant (PaddingFields v)
  -> String
renderPaddingFields =
  renderGlobalFields
    >>> renderLengthFields
    >>> renderPct
    >>> renderZero

renderPadding
  :: forall v
   . (Variant v -> String)
  -> Variant (Padding v)
  -> String
renderPadding = on _padding \p ->
  Array.intercalate " "
    [ renderPaddingFields case_ $ p.top
    , renderPaddingFields case_ $ p.right
    , renderPaddingFields case_ $ p.bottom
    , renderPaddingFields case_ $ p.left
    ]


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


type RelativeSizeFields r =
  ( Smaller
  + Larger
  + r
  )

renderRelativeSizeFields
  :: forall v
   . (Variant v -> String)
  -> Variant (RelativeSizeFields v)
  -> String
renderRelativeSizeFields = renderLarger >>> renderSmaller


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


type ViewportPercentageLengthFields r =
  ( Vh
  + Vmax
  + Vmin
  + Vw
  + r
  )

renderViewportPercentageLengthFields
  :: forall v
   . (Variant v -> String)
  -> Variant (ViewportPercentageLengthFields v)
  -> String
renderViewportPercentageLengthFields =
  renderVh
    >>> renderVmax
    >>> renderVmin
    >>> renderVw


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
