module HW5.Parser
  (
  -- * Parser
  -- | This module contains the parser for the Hi language.
    parse
  ) where

import Control.Arrow ((>>>))
import Control.Monad (guard, void)
import Control.Monad.Combinators.Expr
import qualified Data.ByteString as B
import Data.Char (isAlpha)
import Data.Ratio
import Data.Text (pack)
import Data.Void (Void)
import Data.Word
import HW5.Base
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Main parser function. Parses a 'String' into a 'HiExpr'.
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (spaceCons *> pExpr <* eof) ""

-- | Parser type.
type Parser = Parsec Void String

-- | Parser for whitespace.
spaceCons :: Parser ()
spaceCons = L.space space1 empty empty

-- | Parser combinator for lexemes. Consumes trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceCons

-- | String parser. Consumes trailing whitespace.
symbol :: String -> Parser String
symbol = L.symbol spaceCons

-- | Parser for a rational signed number.
pNumber :: Parser HiValue
pNumber = HiValueNumber <$> lexeme pRational
  where
    pRational :: Parser Data.Ratio.Rational
    pRational = toRational <$> L.signed spaceCons L.scientific

-- | Parser for a string literal.
stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"') <* spaceCons

-- | Parser for a string HiValue.
pString :: Parser HiValue
pString = HiValueString . pack <$> stringLiteral

-- | Parser for a boolean HiValue.
pBoolean :: Parser HiValue
pBoolean = HiValueBool <$> choice [ symbol "true" >> return True
                                  , symbol "false" >> return False
                                  ]

-- | Parser for a function HiValue. Parses only a function name.
pFunction :: Parser HiValue
pFunction = HiValueFunction <$> pFunc
  where
    pFunc :: Parser HiFun
    pFunc = choice
      [symbol "div" >> return HiFunDiv
      , symbol "mul" >> return HiFunMul
      , symbol "add" >> return HiFunAdd
      , symbol "sub" >> return HiFunSub

      , symbol "not-less-than" >> return HiFunNotLessThan
      , symbol "not-greater-than" >> return HiFunNotGreaterThan
      , symbol "not-equals" >> return HiFunNotEquals
      , symbol "not" >> return HiFunNot
      , symbol "and" >> return HiFunAnd
      , symbol "or" >> return HiFunOr
      , symbol "less-than" >> return HiFunLessThan
      , symbol "greater-than" >> return HiFunGreaterThan
      , symbol "equals" >> return HiFunEquals
      , symbol "if" >> return HiFunIf

      , symbol "length" >> return HiFunLength
      , symbol "to-upper" >> return HiFunToUpper
      , symbol "to-lower" >> return HiFunToLower
      , symbol "reverse" >> return HiFunReverse
      , symbol "trim" >> return HiFunTrim

      , symbol "list" >> return HiFunList
      , symbol "range" >> return HiFunRange
      , symbol "fold" >> return HiFunFold

      , symbol "pack-bytes" >> return HiFunPackBytes
      , symbol "unpack-bytes" >> return HiFunUnpackBytes
      , symbol "zip" >> return HiFunZip
      , symbol "unzip" >> return HiFunUnzip
      , symbol "encode-utf8" >> return HiFunEncodeUtf8
      , symbol "decode-utf8" >> return HiFunDecodeUtf8
      , symbol "serialise" >> return HiFunSerialise
      , symbol "deserialise" >> return HiFunDeserialise

      , symbol "read" >> return HiFunRead
      , symbol "write" >> return HiFunWrite
      , symbol "mkdir" >> return HiFunMkDir
      , symbol "cd" >> return HiFunChDir

      , symbol "parse-time" >> return HiFunParseTime

      , symbol "rand" >> return HiFunRand

      , symbol "echo" >> return HiFunEcho

      , symbol "count" >> return HiFunCount
      , symbol "keys" >> return HiFunKeys
      , symbol "values" >> return HiFunValues
      , symbol "invert" >> return HiFunInvert
      ]

-- | Parser for a HiValue.
pValue :: Parser HiValue
pValue = choice
  [ pNumber
  , pBoolean
  , symbol "null" >> return HiValueNull
  , symbol "cwd" >> return (HiValueAction HiActionCwd)
  , symbol "now" >> return (HiValueAction HiActionNow)
  , pString
  , pFunction
  , HiValueBytes . B.pack <$> pByteArray
  ]

-- | Parser for any expression in parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parser for any expression in square brackets.
parensSquare :: Parser a -> Parser a
parensSquare = between (symbol "[") (symbol "]")

-- | Parser for any expression in curly brackets.
parensCurly :: Parser a -> Parser a
parensCurly = between (symbol "{") (symbol "}")

-- | Term parser. Parses any term in the Hi language.
pTerm :: Parser HiExpr
pTerm = choice
  [ try $ parens pExpr
  , HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> try pList
  , HiExprDict <$> pDict
  , HiExprValue <$> pValue
  ]

-- | Parser for a dictionary literal in curly brackets. 
pDict :: Parser [(HiExpr, HiExpr)]
pDict = parensCurly (sepBy pExprPair $ symbol ",")

-- | Parser for a key-value pair in a dictionary literal.
pExprPair :: Parser (HiExpr, HiExpr)
pExprPair = do
  first <- pExpr
  void $ symbol ":"
  second <- pExpr
  return (first, second)

-- | Parser for a list literal in square brackets.
pList :: Parser [HiExpr]
pList = parensSquare (sepBy pExpr $ symbol ",")

-- | Parser for a byte array literal in "square+hash" brackets.
pByteArray :: Parser [Word8]
pByteArray = between (try $ symbol "[#") (try $ symbol "#]") $ sepEndBy pWord8 space1

-- | Parser for a hexadecimal number that represents one byte.
pWord8 :: Parser Word8
pWord8 = do
  num <- pHex
  guard (num >= 0 && num <= 255) <?> "hex integer between 00 and ff"
  return $ fromInteger num

-- | Parser for a hexadecimal number of length 2.
pHex :: Parser Integer
pHex = read . ("0x" ++) <$> count 2 hexDigitChar

-- | Parser for any expression supported in the Hi language.
pExpr :: Parser HiExpr
pExpr = makeExprParser pTerm operatorTable

-- | Description of all supported operators in the Hi language for the Megaparsec
-- parser.
operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ Postfix $ manyUnary $ choice [pApply, pRun, pDot]
    ]
  , [ InfixL $ try $ pInfix "/" HiFunDiv <* notFollowedBy (symbol "=")
    , InfixL $ pInfix "*" HiFunMul
    ]
  , [ InfixL $ pInfix "+" HiFunAdd
    , InfixL $ pInfix "-" HiFunSub
    ]
  , [ InfixN $ pInfix ">=" HiFunNotLessThan
    , InfixN $ pInfix "<=" HiFunNotGreaterThan
    , InfixN $ pInfix ">" HiFunGreaterThan
    , InfixN $ pInfix "<" HiFunLessThan
    , InfixN $ pInfix "==" HiFunEquals
    , InfixN $ pInfix "/=" HiFunNotEquals
    ]
  , [ InfixR $ pInfix "&&" HiFunAnd
    ]
  , [ InfixR $ pInfix "||" HiFunOr
    ]
  ]

-- | Parser combinator for infix operators. Takes the operator name and the
-- corresponding 'HiFun' constructor.
pInfix :: String -> HiFun -> Parser (HiExpr -> HiExpr -> HiExpr)
pInfix name func = binaryApply func <$ symbol name

-- | Parser combinator for binary function application.
binaryApply :: HiFun -> HiExpr -> HiExpr -> HiExpr
binaryApply func a b = HiExprApply (HiExprValue (HiValueFunction func)) [a, b]

-- | Parser combinator for unary operators. Allows for chaining.
manyUnary :: Parser (a -> a) -> Parser (a -> a)
manyUnary singlePostfix = foldr1 (>>>) <$> some singlePostfix

-- | Parser for function application. Implemented as a postfix operator.
pApply :: Parser (HiExpr -> HiExpr)
pApply = flip HiExprApply <$> parens (sepBy pExpr $ symbol ",")

-- | Dot-access parser. Implemented as a postfix operator.
pDot :: Parser (HiExpr -> HiExpr)
pDot = flip HiExprApply . (:[]) . HiExprValue . HiValueString . Data.Text.pack
  <$> (symbol "." *> lexeme pIdent)

-- | Parser for "run" construction for execution actions. Implemented as a
-- postfix operator.
pRun :: Parser (HiExpr -> HiExpr)
pRun = HiExprRun <$ symbol "!"

-- | Parser for identifiers. Accepts kebab-case with first letter being
-- alphabetic and the rest being alphanumeric.
pIdent :: Parser String
pIdent = foldr1 (\a b -> a ++ "-" ++ b)
  <$> (((:) <$> alphaChar <*> many alphaNumChar) `sepBy1` char '-')

-- | Parser for alphabetic characters.
alphaChar :: Parser Char
alphaChar = satisfy isAlpha <?> "alphabetic character"
