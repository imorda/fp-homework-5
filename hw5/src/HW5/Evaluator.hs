{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TupleSections  #-}

module HW5.Evaluator
  (
  -- * Evaluator implementation
    eval
  ) where

import Codec.Compression.Zlib
import Codec.Serialise
import Control.Arrow ((>>>))
import Control.Monad (foldM, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (toList)
import Data.Functor
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ratio
import Data.Semigroup (stimes)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)
import Data.Word
import HW5.Base
import Text.Read (readMaybe)

-- | Evaluates a 'HiExpr' into a 'HiValue' or throws a 'HiError'. Supports 'HiAction's.
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT $ evalExceptT expr

-- | More convenient eval that makes use of ExceptT monad transformer.
evalExceptT :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
-- | The 'HiExprRun' expression runs an action.
evalExceptT (HiExprRun expr) = evalExceptT expr >>= \case
  HiValueAction action -> lift $ runAction action
  _                    -> throwE HiErrorInvalidArgument
-- | The 'HiExprDict' expression creates a dictionary. Evaluates all containing expressions.
evalExceptT (HiExprDict expr) = do
  computed <- mapM (mapMTuple evalExceptT) expr
  return $ HiValueDict $ Map.fromList computed
-- | The 'HiExprValue' expression returns a value.
evalExceptT (HiExprValue val) = return val
-- | The 'HiExprApply' expression applies a function.
-- Valid values are 'HiValueFunction', 'HiValueString', 'HiValueList', 
-- 'HiValueBytes' and 'HiValueDict'.
evalExceptT (HiExprApply func args) = evalExceptT func >>= \case
  HiValueNumber _ -> throwE HiErrorInvalidFunction
  HiValueBool _ -> throwE HiErrorInvalidFunction
  HiValueFunction computedFunc -> invokeRaw computedFunc args
  HiValueNull -> throwE HiErrorInvalidFunction
  HiValueAction _ -> throwE HiErrorInvalidFunction
  HiValueTime _ -> throwE HiErrorInvalidFunction
  HiValueString computedStr -> invokeStr computedStr args
  HiValueList computedLst -> invokeList computedLst args
  HiValueBytes computedBS -> invokeByteString computedBS args
  HiValueDict computedDict -> computeArgs args >>= (unary >=> \key ->
    return $ Map.findWithDefault HiValueNull key computedDict)

-- | Maps a function over a 2-tuple.
mapMTuple :: HiMonad m => (a -> ExceptT HiError m b) -> (a, a) -> ExceptT HiError m (b, b)
mapMTuple f (a, b) = do
  aComputed <- f a
  bComputed <- f b
  return (aComputed, bComputed)

-- | Computes a list of expressions or fails gracefully.
computeArgs :: HiMonad m => [HiExpr] -> ExceptT HiError m [HiValue]
computeArgs = mapM evalExceptT

-- | Subscript implementation for a list.
listSubscript :: HiMonad m => [a] -> (a -> HiValue) -> HiValue -> ExceptT HiError m HiValue
listSubscript lst valueFactory iRaw = do
  i <- integral iRaw
  if i < 0
    then return HiValueNull
    else case drop (fromInteger i) lst of
      []     -> return HiValueNull
      (x: _) -> return $ valueFactory x

-- | Slice implementation for a list.
listSlice :: HiMonad m => [a] -> ([a] -> HiValue) -> HiValue -> HiValue -> ExceptT HiError m HiValue
listSlice lst valueFactory lhsRaw rhsRaw = do
  lhs <- transformIndex lst lhsRaw
  rhs <- transformIndex lst rhsRaw
  let lstDrop = drop (fromMaybe 0 lhs) lst
  return $ valueFactory case rhs of
    Just end -> take (end - fromMaybe 0 lhs) lstDrop
    Nothing  -> lstDrop
  where
    transformIndex :: HiMonad m => [a] -> HiValue -> ExceptT HiError m (Maybe Int)
    transformIndex _ HiValueNull = return Nothing
    transformIndex text iRaw = do
      iNumeric <- integral iRaw
      let i = if iNumeric < 0
          then
            posMod iNumeric $ toInteger $ length text
          else
            iNumeric
      return $ Just $ fromInteger i
    posMod :: Integral a => a -> a -> a
    posMod a b = ((a `mod` b) + b) `mod` b

-- | String invocation implementation. Supports slice and subscript.
invokeStr :: HiMonad m => T.Text -> [HiExpr] -> ExceptT HiError m HiValue
invokeStr str rawArgs = computeArgs rawArgs >>= \case
  [iRaw]       -> listSubscript (T.unpack str) (HiValueString . T.singleton) iRaw
  [aRaw, bRaw] -> listSlice (T.unpack str) (HiValueString . T.pack) aRaw bRaw
  _            -> throwE HiErrorArityMismatch

-- | List invocation implementation. Supports slice and subscript.
invokeList :: HiMonad m => Seq.Seq HiValue -> [HiExpr] -> ExceptT HiError m HiValue
invokeList lst rawArgs = computeArgs rawArgs >>= \case
  [iRaw]       -> listSubscript (toList lst) id iRaw
  [aRaw, bRaw] -> listSlice (toList lst) (HiValueList . Seq.fromList) aRaw bRaw
  _            -> throwE HiErrorArityMismatch

-- | ByteString invocation implementation. Supports slice and subscript.
invokeByteString :: HiMonad m => B.ByteString -> [HiExpr] -> ExceptT HiError m HiValue
invokeByteString lst rawArgs = computeArgs rawArgs >>= \case
  [iRaw]       -> listSubscript (B.unpack lst) (HiValueNumber . toRational) iRaw
  [aRaw, bRaw] -> listSlice (B.unpack lst) (HiValueBytes . B.pack) aRaw bRaw
  _            -> throwE HiErrorArityMismatch

{-|
Function invocation step *before* argument evaluation.

HiFunIf lazy evaluation expects exactly 3 arguments. If the first argument is true,
evaluates the second argument, otherwise evaluates the third argument.

HiFunAnd lazy evaluation expects exactly 2 arguments. If the first argument is false or null,
evaluates to the first argument, otherwise evaluates to the second argument.

HiFunOr lazy evaluation expects exactly 2 arguments. If the first argument is true or non-null,
evaluates to the first argument, otherwise evaluates to the second argument.

Otherwise, proceeds to the next, non-lazy evaluation step. Evaluates all arguments.
-}
invokeRaw :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
invokeRaw HiFunIf args = do
  (condRaw, branch1, branch2) <- ternary args
  condEval <- evalExceptT condRaw
  cond <- boolean condEval
  evalExceptT $ if cond then branch1 else branch2
invokeRaw HiFunAnd args = do
  (lhs, rhs) <- binary args
  lhsEval <- evalExceptT lhs
  case lhsEval of
    HiValueNull       -> return lhsEval
    HiValueBool False -> return lhsEval
    _                 -> evalExceptT rhs
invokeRaw HiFunOr args = do
  (lhs, rhs) <- binary args
  lhsEval <- evalExceptT lhs
  case lhsEval of
    HiValueNull       -> evalExceptT rhs
    HiValueBool False -> evalExceptT rhs
    _                 -> return lhsEval
invokeRaw func rawArgs = computeArgs rawArgs >>= \args -> invoke func args

{-|
Function invocation step *after* argument evaluation.

HiFunDiv evaluates to the division of two numbers
or concatenates two strings with "/" in between.

HiFunMul evaluates to the multiplication of two numbers
or replicates a string, a list or a byte string.

HiFunAdd evaluates to the addition of two numbers,
concatenates two strings, two lists or two byte strings
or adds seconds to a time value.

HiFunSub evaluates to the subtraction of two numbers
or evaluates difference between two time values.

HiFunNot evaluates to the negation of a boolean value. Expects exactly one argument.

HiFunLessThan evaluates to the "<" comparison of two HiValues since they derive Ord.

HiFunGreaterThan evaluates to the ">" comparison of two HiValues.

HiFunEquals evaluates to the "==" comparison of two HiValues.

HiFunNotLessThan evaluates to the ">=" comparison of two HiValues.

HiFunNotGreaterThan evaluates to the "<=" comparison of two HiValues.

HiFunNotEquals evaluates to the "/=" comparison of two HiValues.

HiFunLength evaluates to the length of a string, a list or a byte string.

HiFunToUpper evaluates to the uppercase version of a string. Expects exactly one argument.

HiFunToLower evaluates to the lowercase version of a string. Expects exactly one argument.

HiFunReverse evaluates to the reversed version of a string, a list or a byte string.

HiFunTrim removes whitespace from the beginning and the end of a string.

HiFunList evaluates to a list of HiValues. Expects any number of arguments.

HiFunRange evaluates to a list of numbers from a to b inclusive. Expects exactly two numbers.

HiFunFold evaluates to the result of folding a list with a function.
Expects exactly two arguments where first is a function and second is a list.

HiFunPackBytes evaluates to a byte string from a list of numbers.
Expects one list of numbers in range [0, 255].

HiFunUnpackBytes evaluates to a list of numbers from a byte string.

HiFunZip uses zlib to compress a byte string. Expects exactly one byte string.

HiFunUnzip uses zlib to decompress a byte string. Expects exactly one byte string
that was compressed with HiFunZip.

HiFunEncodeUtf8 evaluates to a byte string from a string. Expects exactly one string.

HiFunDecodeUtf8 evaluates to a string from a byte string.
Expects exactly one byte string representing a valid UTF-8 string.

HiFunSerialise serialises to a byte string from any HiValue.
Expects exactly one HiValue.

HiFunDeserialise deserialises to a HiValue from a valid byte string.

HiFunRead represents the read action. Expects exactly one string.

HiFunWrite represents the write action. Expects exactly two arguments where first is a string
and second is a byte string.

HiFunMkDir represents the mkdir action. Expects exactly one string.

HiFunChDir represents the cd action. Expects exactly one string.

HiFunParseTime parses a string to a time value. Expects exactly one string.

HiFunRand evaluates to a random number in range [a, b] inclusive. Expects exactly two numbers.

HiFunEcho represents the echo action. Expects exactly one string.

HiFunKeys evaluates to a list of keys of a dictionary. Expects exactly one dictionary.

HiFunValues evaluates to a list of values of a dictionary. Expects exactly one dictionary.

HiFunCount evaluates to a dictionary. Maps each element in a list, a string or a byte string
to the number of its occurrences there.

HiFunInvert evaluates to a dictionary. Maps all values of a dictionary to a list of keys
that were previously mapped to that values.
-}
invoke :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
invoke HiFunDiv args = binaryOperation string string HiValueString args (\a b ->
                         a <> T.pack "/" <> b)
                       <||> binaryCheckedOperation numeric numeric HiValueNumber args
                         (assertBinary (\_ -> (/=) 0) HiErrorDivideByZero \a b -> return $ a / b)
invoke HiFunMul args = binaryArithmetic args (*)
                       <||> binaryOperation string integral HiValueString args hiReplicate
                       <||> binaryOperation list integral HiValueList args hiReplicate
                       <||> binaryOperation bytes integral HiValueBytes args hiReplicate
invoke HiFunAdd args = binaryArithmetic args (+)
                       <||> binaryOperation string string HiValueString args (<>)
                       <||> binaryOperation list list HiValueList args (<>)
                       <||> binaryOperation bytes bytes HiValueBytes args (<>)
                       <||> binaryOperation time numeric HiValueTime args (\a b ->
                         addUTCTime (fromRational b) a)
invoke HiFunSub args = binaryArithmetic args (-)
                       <||> binaryOperation time time HiValueNumber args (\a b ->
                         toRational $ diffUTCTime a b)

invoke HiFunNot args = unary args >>= (boolean >=> (return . HiValueBool . not))
invoke HiFunLessThan args = binary args >>= \(a, b) -> return $ HiValueBool $ a < b
invoke HiFunGreaterThan args = invoke HiFunLessThan $ reverse args
invoke HiFunEquals args = binary args >>= \(a, b) -> return $ HiValueBool $ a == b
invoke HiFunNotLessThan args = invoke HiFunLessThan args >>= \val -> invoke HiFunNot [val]
invoke HiFunNotGreaterThan args = invoke HiFunGreaterThan args >>= \val -> invoke HiFunNot [val]
invoke HiFunNotEquals args = invoke HiFunEquals args >>= \val -> invoke HiFunNot [val]

invoke HiFunIf args = invokeRaw HiFunIf $ map HiExprValue args
invoke HiFunAnd args = invokeRaw HiFunAnd $ map HiExprValue args
invoke HiFunOr args = invokeRaw HiFunOr $ map HiExprValue args

invoke HiFunLength args = (unary args >>= (string >=> return . HiValueNumber . toRational . T.length))
                     <||> (unary args >>= (list >=> return . HiValueNumber . toRational . Seq.length))
                     <||> (unary args >>= (bytes >=> return . HiValueNumber . toRational . B.length))
invoke HiFunToUpper args = unary args >>= (string >=> (return . HiValueString . T.toUpper))
invoke HiFunToLower args = unary args >>= (string >=> (return . HiValueString . T.toLower))
invoke HiFunReverse args = (unary args >>= (string >=> return . HiValueString . T.reverse))
                      <||> (unary args >>= (list >=> return . HiValueList . Seq.reverse))
                      <||> (unary args >>= (bytes >=> return . HiValueBytes . B.reverse))
invoke HiFunTrim args = unary args >>= (string >=> (return . HiValueString . T.strip))

invoke HiFunList args = return $ HiValueList $ Seq.fromList args
invoke HiFunRange args = binaryOperation numeric numeric HiValueList args \lhs rhs ->
  if rhs < lhs then Seq.empty else
    Seq.fromFunction (truncate $ rhs - lhs + 1) (HiValueNumber . (lhs +) . toRational)
invoke HiFunFold args = binaryCheckedOperation function list id args $ \func foldable ->
    withExceptT (transformE HiErrorArityMismatch HiErrorInvalidArgument) $
    hiFoldM1 (\a b -> invoke func [a, b]) $ toList foldable

invoke HiFunPackBytes args = unary args >>= (list >=> (mapM
   (integral >=> assert (numBetween 0 255) HiErrorInvalidArgument) >=>
   \intSeq -> return $ HiValueBytes $ B.pack $ map (fromIntegral::Integer -> Word8) $ toList intSeq))
  where
    numBetween :: (Ord a) => a -> a -> a -> Bool
    numBetween minVal maxVal num = num >= minVal && num <= maxVal
invoke HiFunUnpackBytes args = unary args >>= (bytes >=>
  return . HiValueList . fmap (HiValueNumber . toRational) . Seq.fromList . B.unpack)
invoke HiFunZip args = unary args >>= (bytes >=>
  return . HiValueBytes .
  toStrict . compressWith (defaultCompressParams { compressLevel = bestCompression }) . fromStrict)
invoke HiFunUnzip args = unary args >>= (bytes >=>
  return . HiValueBytes .
  toStrict . decompress . fromStrict)
invoke HiFunEncodeUtf8 args = unary args >>= (string >=> return . HiValueBytes . encodeUtf8)
invoke HiFunDecodeUtf8 args = unary args >>= (bytes >=> return . \bs -> case decodeUtf8' bs of
  Right decoded -> HiValueString decoded
  Left _        -> HiValueNull)
invoke HiFunSerialise args = unary args <&> (HiValueBytes . toStrict . serialise)
invoke HiFunDeserialise args = unary args >>= (bytes >=> return . deserialise . fromStrict)

invoke HiFunRead args = unaryStringAction HiActionRead args
invoke HiFunWrite args = binaryOperation string string HiValueAction args \a b ->
  HiActionWrite (T.unpack a) $ encodeUtf8 b
invoke HiFunMkDir args = unaryStringAction HiActionMkDir args
invoke HiFunChDir args = unaryStringAction HiActionChDir args

invoke HiFunParseTime args = unary args >>= (string >=> T.unpack >>> readMaybe >>> (\case
  Just x  -> HiValueTime x
  Nothing -> HiValueNull) >>> return)

invoke HiFunRand args = binaryOperation integral integral HiValueAction args \a b ->
  HiActionRand (fromInteger a) $ fromInteger b

invoke HiFunEcho args = unary args >>= (string >=> return . HiValueAction . HiActionEcho)

invoke HiFunKeys args = unary args >>= (dict >=> return . HiValueList . Seq.fromList . Map.keys)
invoke HiFunValues args = unary args >>= (dict >=> return . HiValueList . Seq.fromList . Map.elems)
invoke HiFunCount args = (unary args >>= (list >=> return . HiValueDict . countList . toList))
                    <||> (unary args >>= (bytes >=> return . HiValueDict . countList .
                          map (HiValueNumber . toRational) . B.unpack))
                    <||> (unary args >>= (string >=> return . HiValueDict . countList .
                          map (HiValueString . T.singleton) . T.unpack))
invoke HiFunInvert args = unary args >>= (dict >=> return . HiValueDict
  . Map.map (HiValueList . Seq.fromList) . Map.fromListWith (++)
  . map (\(aKey, aValue) -> (aValue, [aKey])) . Map.toList)


-- | Counts the number of occurrences of each element in a list.
countList :: [HiValue] -> Map.Map HiValue HiValue
countList source = Map.map HiValueNumber $ Map.fromListWith (+) pairList
  where
    pairList = map (, 1) source

-- | Evaluates to an action with only one string parameter.
unaryStringAction :: HiMonad m => (String -> HiAction) -> [HiValue] -> ExceptT HiError m HiValue
unaryStringAction factory args = unary args >>=
  (string >=> return . HiValueAction . factory . T.unpack)

-- | Transforms one error to another.
transformE :: HiError -> HiError -> HiError -> HiError
transformE from to orig
  | orig == from = to
  | otherwise = orig

-- | Folds a list with a binary function or fails gracefully.
hiFoldM1 :: HiMonad m
         => (HiValue -> HiValue -> ExceptT HiError m HiValue)
         -> [HiValue]
         -> ExceptT HiError m HiValue
hiFoldM1 _ []      = throwE HiErrorInvalidArgument
hiFoldM1 f [x]     = mapExceptT (fmap $ \case
  Left HiErrorArityMismatch -> Left HiErrorArityMismatch
  _                         -> Right x) $ f HiValueNull HiValueNull
hiFoldM1 f (x: xs) = foldM f x xs

-- | Binary operation invocation generalisation. Takes two interpreters for the left and right
-- arguments, a factory for the result, a list of arguments and a binary function.
binaryOperation :: HiMonad m
                => (HiValue -> ExceptT HiError m a)
                -> (HiValue -> ExceptT HiError m b)
                -> (c -> HiValue)
                -> [HiValue]
                -> (a -> b -> c)
                -> ExceptT HiError m HiValue
binaryOperation parserLhs parserRhs resultFactory args f = do
  (aRaw, bRaw) <- binary args
  a <- parserLhs aRaw
  b <- parserRhs bRaw
  return $ resultFactory $ f a b

-- | Binary arithmetic invocation generalisation. Takes a list of arguments and a binary function
-- on Rational numbers.
binaryArithmetic :: HiMonad m
                 => [HiValue]
                 -> (Rational -> Rational -> Rational)
                 -> ExceptT HiError m HiValue
binaryArithmetic = binaryOperation numeric numeric HiValueNumber

-- | Binary checked operation invocation generalisation. Takes two interpreters for the left and
-- right arguments, a factory for the result, a list of arguments and a binary function that
-- may also fail.
binaryCheckedOperation :: HiMonad m
                       => (HiValue -> ExceptT HiError m a)
                       -> (HiValue -> ExceptT HiError m b)
                       -> (c -> HiValue)
                       -> [HiValue]
                       -> (a -> b -> ExceptT HiError m c)
                       -> ExceptT HiError m HiValue
binaryCheckedOperation parserLhs parserRhs resultFactory args f = do
  (aRaw, bRaw) <- binary args
  a <- parserLhs aRaw
  b <- parserRhs bRaw
  c <- f a b
  return $ resultFactory c

-- | Asserts a binary condition or fails gracefully.
assertBinary :: HiMonad m
             => (a -> b -> Bool)
             -> HiError
             -> (a -> b -> ExceptT HiError m c)
             -> a -> b
             -> ExceptT HiError m c
assertBinary cond err f a b = if cond a b then f a b else throwE err

-- | Asserts a condition or fails gracefully.
assert :: HiMonad m => (a -> Bool) -> HiError -> a -> ExceptT HiError m a
assert cond err val = if cond val then return val else throwE err

-- | Asserts the given HiValue is a number or fails gracefully.
numeric :: HiMonad m => HiValue -> ExceptT HiError m Data.Ratio.Rational
numeric (HiValueNumber x) = return x
numeric _                 = throwE HiErrorInvalidArgument

-- | Asserts the given HiValue is a time value or fails gracefully.
time :: HiMonad m => HiValue -> ExceptT HiError m UTCTime
time (HiValueTime x) = return x
time _               = throwE HiErrorInvalidArgument

-- | Asserts the given HiValue is an integral number or fails gracefully.
integral :: HiMonad m => HiValue -> ExceptT HiError m Integer
integral (HiValueNumber num)
  | denominator num == 1 = return $ numerator num
  | otherwise = throwE HiErrorInvalidArgument
integral _ = throwE HiErrorInvalidArgument

-- | Asserts the given HiValue is a boolean or fails gracefully.
boolean :: HiMonad m => HiValue -> ExceptT HiError m Bool
boolean (HiValueBool x) = return x
boolean _               = throwE HiErrorInvalidArgument

-- | Asserts the given HiValue is a string or fails gracefully.
string :: HiMonad m => HiValue -> ExceptT HiError m T.Text
string (HiValueString x) = return x
string _                 = throwE HiErrorInvalidArgument

-- | Asserts the given HiValue is a list or fails gracefully.
list :: HiMonad m => HiValue -> ExceptT HiError m (Seq.Seq HiValue)
list (HiValueList x) = return x
list _               = throwE HiErrorInvalidArgument

-- | Asserts the given HiValue is a byte string or fails gracefully.
bytes :: HiMonad m => HiValue -> ExceptT HiError m B.ByteString
bytes (HiValueBytes x) = return x
bytes _                = throwE HiErrorInvalidArgument

-- | Asserts the given HiValue is a function or fails gracefully.
function :: HiMonad m => HiValue -> ExceptT HiError m HiFun
function (HiValueFunction x) = return x
function _                   = throwE HiErrorInvalidArgument

-- | Asserts the given HiValue is a dictionary or fails gracefully.
dict :: HiMonad m => HiValue -> ExceptT HiError m (Map.Map HiValue HiValue)
dict (HiValueDict x) = return x
dict _               = throwE HiErrorInvalidArgument

-- | Asserts the given HiValue list contains only one element or fails gracefully.
unary :: HiMonad m => [t] -> ExceptT HiError m t
unary [a] = return a
unary _   = throwE HiErrorArityMismatch

-- | Asserts the given HiValue list contains only two elements or fails gracefully.
binary :: HiMonad m => [t] -> ExceptT HiError m (t, t)
binary [a, b] = return (a, b)
binary _      = throwE HiErrorArityMismatch

-- | Asserts the given HiValue list contains only three elements or fails gracefully.
ternary :: HiMonad m => [t] -> ExceptT HiError m (t, t, t)
ternary [a, b, c] = return (a, b, c)
ternary _         = throwE HiErrorArityMismatch

-- | Replicates a value.
hiReplicate :: (Semigroup a, Integral b) => a -> b -> a
hiReplicate lhs rhs = stimes rhs lhs

-- | Infix operator for 'ExceptT' that resembles the Semigroup behaviour of 'Either'.
infixr 6 <||>
(<||>) :: Monad m => ExceptT e1 m a -> ExceptT e2 m a -> ExceptT e2 m a
ExceptT mx <||> ExceptT my = ExceptT $ do
    ex <- mx
    case ex of
        Left _  -> my >>= \ey -> return ey
        Right x -> return (Right x)
