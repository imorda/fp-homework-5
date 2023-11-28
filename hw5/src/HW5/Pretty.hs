module HW5.Pretty
  ( prettyValue
  ) where

import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)

import HW5.Base (HiValue)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = undefined
