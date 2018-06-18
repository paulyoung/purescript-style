module Style.Ruleset where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Style.Declaration (Declaration)
import Style.Declaration as Declaration
import Style.Selector (Selector)
import Style.Selector as Selector

data Ruleset = Ruleset (NonEmptyArray Selector) (NonEmptyArray Declaration)

derive instance eqRuleset :: Eq Ruleset
derive instance ordRuleset :: Ord Ruleset

render :: Ruleset -> String
render (Ruleset ss ds) = selectors <> "{" <> declarations <> "}"

  where

  selectors :: String
  selectors = Array.intercalate ",\n" (Selector.render <$> ss) <> " "

  declarations :: String
  declarations = "\n  " <> Array.intercalate "\n  " (Declaration.render <$> ds) <> "\n"
