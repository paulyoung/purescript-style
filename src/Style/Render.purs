module Style.Render where

import Prelude

import Data.Array (intercalate)
import Style.Declaration (Declaration)
import Style.Declaration as Declaration

inline :: Array Declaration -> String
inline = intercalate " " <<< map Declaration.render
