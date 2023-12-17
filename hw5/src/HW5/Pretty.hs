{-# LANGUAGE OverloadedStrings #-}

module HW5.Pretty
  ( 
  -- * Pretty-printing
  -- | This module provides a pretty-printer for the 'HiExpr' datatype.
    prettyValue
  , prettyExpr
  ) where

import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle)

import qualified Data.ByteString as B
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.List (stripPrefix)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ratio
import Data.Scientific (fromRationalRepetendUnlimited)
import Data.Word
import HW5.Base
import Numeric (showHex)
import Text.Casing

-- | Pretty-print 'HiExpr' as a Prettyprinter 'Doc'.
-- Visualises the AST
prettyExpr :: HiExpr -> Doc AnsiStyle
prettyExpr (HiExprValue x) = prettyValue x
prettyExpr (HiExprRun x) = prettyExpr x <> "!"
prettyExpr (HiExprDict x) = vsep
  [ "{"
  , align $ vsep (map (\(key, value) ->
      align (prettyExpr key) <> ": " <> align (prettyExpr value) <> ",") x)
  , "}"]
prettyExpr (HiExprApply func args) = vsep
  ["(" <> align (prettyExpr func) <> ")"
  , "[" <> align (vsep (map prettyExpr args)) <> "]"]

-- | Pretty-print 'HiValue' as a Prettyprinter 'Doc'.
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber x)
  | denominator x == 1 = pretty $ numerator x
  | (num, Nothing) <- fromRationalRepetendUnlimited x = viaShow num
  | (int, remainder) <- quotRem (numerator x) (denominator x) =
    hcat $ (if int > 0 then [pretty int, " + "] else
            if int < 0 then [pretty int, " - "] else
            ["-" | remainder < 0])
    ++ [pretty $ abs remainder, "/", pretty $ denominator x]
prettyValue (HiValueBool x) = pretty $ map toLower $ show x
prettyValue (HiValueFunction HiFunMkDir) = "mkdir" -- kebab formatting inconsistency
prettyValue (HiValueFunction HiFunChDir) = "cd" -- kebab formatting inconsistency
prettyValue (HiValueFunction x) = show x & stripPrefix "HiFun" & fromMaybe "undefined" & kebab & pretty
prettyValue HiValueNull = "null"
prettyValue (HiValueString x) = viaShow x
prettyValue (HiValueList lst) = "["
  <> hsep (punctuate comma $ map prettyValue $ toList lst) <> "]"
prettyValue (HiValueBytes x) = "[#" <+>
  hsep (map (pretty . word8ToHex) $ B.unpack x) <+> "#]"
prettyValue (HiValueAction x) = prettyAction x
prettyValue (HiValueTime x) = "parse-time(\"" <> viaShow x <> "\")"
prettyValue (HiValueDict x) = "{"
  <+> hsep (punctuate comma $ Map.foldMapWithKey (\key value ->
    [prettyValue key <> ":" <+> prettyValue value]) x)
  <+> "}"

-- | Pretty-print 'HiAction' as a Prettyprinter 'Doc'.
prettyAction :: HiAction -> Doc AnsiStyle
prettyAction (HiActionRead x) = "read(" <> viaShow x <> ")"
prettyAction (HiActionWrite x y) = "write(" <> viaShow x <> ", "
  <> prettyValue (HiValueBytes y) <> ")"
prettyAction (HiActionMkDir x) = "mkdir(" <> viaShow x <> ")"
prettyAction (HiActionChDir x) = "cd(" <> viaShow x <> ")"
prettyAction HiActionCwd = "cwd"
prettyAction HiActionNow = "now"
prettyAction (HiActionRand x y) = "rand(" <+> pretty x <> ", " <> pretty y <+> ")"
prettyAction (HiActionEcho x) = "echo(" <> viaShow x <> ")"

-- | Convert a 'Word8' byte to a hexadecimal 'String' with fixed length of 2.
word8ToHex :: Word8 -> String
word8ToHex = prependIfNeeded . flip showHex ""
  where
    prependIfNeeded :: String -> String
    prependIfNeeded str
      | length str == 1 = "0" ++ str
      | otherwise = str
