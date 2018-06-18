module Style.Render where

import Prelude

import Data.Array (intercalate)
import Data.Array.NonEmpty (NonEmptyArray)
import Style.Declaration (Declaration)
import Style.Declaration as Declaration

inline :: NonEmptyArray Declaration -> String
inline = intercalate " " <<< map Declaration.render
