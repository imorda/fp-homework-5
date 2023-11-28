module HW5.Parser
  ( parse
  ) where

import Data.Void (Void)
import Text.Megaparsec.Error (ParseErrorBundle)

import HW5.Base (HiExpr)

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = undefined
