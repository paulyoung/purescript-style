module Style.Selector where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap)

data Selector
  = TypeSelector String
  | ClassSelector String
  | IDSelector String
  | UniversalSelector {-Namespace-}
  | AttributeSelector (Maybe Selector) Attribute
  | CombinedSelector Selector Combinator Selector
  | PseudoClassSelector Selector PseudoClass
  | PseudoElementSelector Selector PseudoElement

render :: Selector -> String
render = case _ of
  TypeSelector el -> el
  ClassSelector c -> "." <> c
  IDSelector i -> "#" <> i
  UniversalSelector -> "*"
  AttributeSelector s a ->
    maybe (renderAttribute a) (\s' -> render s' <> renderAttribute a) s
  CombinedSelector s1 c s2 -> render s1 <> renderCombinator c <> render s2
  PseudoClassSelector s pc -> render s <> renderPseudoClass pc
  PseudoElementSelector s pe -> render s <> renderPseudoElement pe


data Attribute
 = Equal AttributeName AttributeValue
 | TildeEqual AttributeName AttributeValue
 | PipeEqual AttributeName AttributeValue
 | CaretEqual AttributeName AttributeValue
 | DollarEqual AttributeName AttributeValue
 | AsteriskEqual AttributeName AttributeValue
 | CaseInsensitive Attribute

renderAttribute :: Attribute -> String
renderAttribute = case _ of
  Equal n v -> renderAttribute' "=" n v
  TildeEqual n v -> renderAttribute' "~=" n v
  PipeEqual n v -> renderAttribute' "|=" n v
  CaretEqual n v -> renderAttribute' "^=" n v
  DollarEqual n v -> renderAttribute' "$=" n v
  AsteriskEqual n v -> renderAttribute' "*=" n v
  CaseInsensitive a -> renderAttribute a <> " i"

  where

  renderAttribute' :: String -> AttributeName -> AttributeValue -> String
  renderAttribute' op n v =
    "[" <> renderAttributeName n <> op <> renderAttributeValue v <> "]"


newtype AttributeName = AttributeName String

derive instance newtypeAttributeName :: Newtype AttributeName _

renderAttributeName :: AttributeName -> String
renderAttributeName = unwrap


newtype AttributeValue = AttributeValue String

derive instance newtypeAttributeValue :: Newtype AttributeValue _

renderAttributeValue :: AttributeValue -> String
renderAttributeValue = unwrap


data Combinator
  = AdjacentSibling
  | GeneralSibling
  | Child
  | Descendant

renderCombinator :: Combinator -> String
renderCombinator = case _ of
  AdjacentSibling -> " + "
  GeneralSibling -> " ~ "
  Child -> " > "
  Descendant -> " "


data Directionality
  = LeftToRight
  | RightToLeft

renderDirectionality :: Directionality -> String
renderDirectionality = case _ of
  LeftToRight -> "ltr"
  RightToLeft -> "rtl"


newtype LanguageCode = LanguageCode String

derive instance newtypeLanguageCode :: Newtype LanguageCode _

renderLanguageCode :: LanguageCode -> String
renderLanguageCode = unwrap


data Position
  = AnPlusB Int (Maybe Int)
  | Even
  | Odd

renderPosition :: Position -> String
renderPosition = case _ of
  AnPlusB a b ->
    intToString a <> "n" <> maybe "" (\b' -> "+" <> intToString b') b
  Even -> "even"
  Odd -> "odd"

  where

  intToString :: Int -> String
  intToString = Int.toStringAs Int.decimal


data PseudoClass
  = Active
  | Any
  | AnyLink
  | Checked
  | Default
  | Defined
  | Dir Directionality
  | Disabled
  | Empty
  | Enabled
  | First
  | FirstChild
  | FirstOfType
  | Fullscreen
  | Focus
  | FocusVisible
  | Host (Maybe Selector)
  | HostContext Selector
  | Hover
  | Indeterminate
  | InRange
  | Invalid
  | Lang LanguageCode
  | LastChild
  | LastOfType
  | Left
  | Link
  | Not Selector
  | NthChild Position
  | NthLastChild Position
  | NthLastOfType Position
  | NthOfType Position
  | OnlyChild
  | OnlyOfType
  | Optional
  | OutOfRange
  | ReadOnly
  | ReadWrite
  | Required
  | Right
  | Root
  | Scope
  | Target
  | Valid
  | Visited

renderPseudoClass :: PseudoClass -> String
renderPseudoClass pc = ":" <> case pc of
  Active -> "active"
  Any -> "any"
  AnyLink -> "any-link"
  Checked -> "checked"
  Default -> "default"
  Defined -> "defined"
  Dir d -> "dir(" <> renderDirectionality d <> ")"
  Disabled -> "disabled"
  Empty -> "empty"
  Enabled -> "enabled"
  First -> "first"
  FirstChild -> "first-child"
  FirstOfType -> "first-of-type"
  Fullscreen -> "fullscreen"
  Focus -> "focus"
  FocusVisible -> "focus-visible"
  Host h -> maybe "host" (\h' -> "host(" <> render h' <> ")") h
  HostContext s -> "host-context(" <> render s <> ")"
  Hover -> "hover"
  Indeterminate -> "indeterminate"
  InRange -> "in-range"
  Invalid -> "invalid"
  Lang l -> "lang(" <> renderLanguageCode l <> ")"
  LastChild -> "last-child"
  LastOfType -> "last-of-type"
  Left -> "left"
  Link -> "link"
  Not s -> "not(" <> render s <> ")"
  NthChild p -> "nth-child(" <> renderPosition p <> ")"
  NthLastChild p -> "nth-last-child(" <> renderPosition p <> ")"
  NthLastOfType p -> "nth-last-of-type(" <> renderPosition p <> ")"
  NthOfType p -> "nth-of-type(" <> renderPosition p <> ")"
  OnlyChild -> "only-child"
  OnlyOfType -> "only-of-type"
  Optional -> "optional"
  OutOfRange -> "out-of-range"
  ReadOnly -> "read-only"
  ReadWrite -> "read-write"
  Required -> "required"
  Right -> "right"
  Root -> "root"
  Scope -> "scope"
  Target -> "target"
  Valid -> "valid"
  Visited -> "visited"


data PseudoElement
  = After
  | Backdrop
  | Before
  | Cue
  | FirstLetter
  | FirstLine
  | GrammarError
  | Marker
  | Placeholder
  | Selection
  | Slotted
  | SpellingError

renderPseudoElement :: PseudoElement -> String
renderPseudoElement pe = "::" <> case pe of
  After -> "after"
  Backdrop -> "backdrop"
  Before -> "before"
  Cue -> "cue"
  FirstLetter -> "first-letter"
  FirstLine -> "first-line"
  GrammarError -> "grammar-error"
  Marker -> "marker"
  Placeholder -> "placeholder"
  Selection -> "selection"
  Slotted -> "slotted"
  SpellingError -> "spelling-error"
