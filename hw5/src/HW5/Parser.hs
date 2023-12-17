module HW5.Parser
  ( parse
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

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (spaceCons *> pExpr <* eof) ""

type Parser = Parsec Void String

spaceCons :: Parser ()
spaceCons = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceCons

symbol :: String -> Parser String
symbol = L.symbol spaceCons

pNumber :: Parser HiValue
pNumber = HiValueNumber <$> lexeme pRational
  where
    pRational :: Parser Data.Ratio.Rational
    pRational = toRational <$> L.signed spaceCons L.scientific

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"') <* spaceCons

pString :: Parser HiValue
pString = HiValueString . pack <$> stringLiteral

pBoolean :: Parser HiValue
pBoolean = HiValueBool <$> choice [ symbol "true" >> return True
                                  , symbol "false" >> return False
                                  ]

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

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parensSquare :: Parser a -> Parser a
parensSquare = between (symbol "[") (symbol "]")

parensCurly :: Parser a -> Parser a
parensCurly = between (symbol "{") (symbol "}")

pTerm :: Parser HiExpr
pTerm = choice
  [ try $ parens pExpr
  , HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> try pList
  , HiExprDict <$> pDict
  , HiExprValue <$> pValue
  ]

pDict :: Parser [(HiExpr, HiExpr)]
pDict = parensCurly (sepBy pExprPair $ symbol ",")

pExprPair :: Parser (HiExpr, HiExpr)
pExprPair = do
  first <- pExpr
  void $ symbol ":"
  second <- pExpr
  return (first, second)

pList :: Parser [HiExpr]
pList = parensSquare (sepBy pExpr $ symbol ",")

pByteArray :: Parser [Word8]
pByteArray = between (try $ symbol "[#") (try $ symbol "#]") $ sepEndBy pWord8 space1

pWord8 :: Parser Word8
pWord8 = do
  num <- pHex
  guard (num >= 0 && num <= 255) <?> "hex integer between 00 and ff"
  return $ fromInteger num

pHex :: Parser Integer
pHex = read . ("0x" ++) <$> count 2 hexDigitChar

pExpr :: Parser HiExpr
pExpr = makeExprParser pTerm operatorTable

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
    , InfixR $ pInfix "||" HiFunOr
    ]
  ]

pInfix :: String -> HiFun -> Parser (HiExpr -> HiExpr -> HiExpr)
pInfix name func = binaryApply func <$ symbol name

binaryApply :: HiFun -> HiExpr -> HiExpr -> HiExpr
binaryApply func a b = HiExprApply (HiExprValue (HiValueFunction func)) [a, b]

manyUnary :: Parser (a -> a) -> Parser (a -> a)
manyUnary singlePostfix = foldr1 (>>>) <$> some singlePostfix

pApply :: Parser (HiExpr -> HiExpr)
pApply = flip HiExprApply <$> parens (sepBy pExpr $ symbol ",")

pDot :: Parser (HiExpr -> HiExpr)
pDot = flip HiExprApply . (:[]) . HiExprValue . HiValueString . Data.Text.pack
  <$> (symbol "." *> lexeme pIdent)

pRun :: Parser (HiExpr -> HiExpr)
pRun = HiExprRun <$ symbol "!"

pIdent :: Parser String
pIdent = foldr1 (\a b -> a ++ "-" ++ b)
  <$> (((:) <$> alphaChar <*> many alphaNumChar) `sepBy1` char '-')

alphaChar :: Parser Char
alphaChar = satisfy isAlpha <?> "alphabetic character"
