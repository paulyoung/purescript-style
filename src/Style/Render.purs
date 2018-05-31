module Style.Render where

import Prelude

import Data.Array (intercalate)
import Style.Declaration (Declaration(..))
import Style.Declaration.Property as Property
import Style.Declaration.Value as Value

declaration :: Declaration -> String
declaration (Declaration p v) = Property.render p <> ": " <> Value.render v <> ";"

inline :: Array Declaration -> String
inline = intercalate " " <<< map declaration
