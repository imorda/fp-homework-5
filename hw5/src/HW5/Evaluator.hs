{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TupleSections  #-}

module HW5.Evaluator
  ( eval
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

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT $ evalExceptT expr

evalExceptT :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalExceptT (HiExprRun expr) = evalExceptT expr >>= \case
  HiValueAction action -> lift $ runAction action
  _                    -> throwE HiErrorInvalidArgument
evalExceptT (HiExprDict expr) = do
  computed <- mapM (mapMTuple evalExceptT) expr
  return $ HiValueDict $ Map.fromList computed
evalExceptT (HiExprValue val) = return val
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

mapMTuple :: HiMonad m => (a -> ExceptT HiError m b) -> (a, a) -> ExceptT HiError m (b, b)
mapMTuple f (a, b) = do
  aComputed <- f a
  bComputed <- f b
  return (aComputed, bComputed)

computeArgs :: HiMonad m => [HiExpr] -> ExceptT HiError m [HiValue]
computeArgs = mapM evalExceptT

listSubscript :: HiMonad m => [a] -> (a -> HiValue) -> HiValue -> ExceptT HiError m HiValue
listSubscript lst valueFactory iRaw = do
  i <- integral iRaw
  if i < 0
    then return HiValueNull
    else case drop (fromInteger i) lst of
      []     -> return HiValueNull
      (x: _) -> return $ valueFactory x

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

invokeStr :: HiMonad m => T.Text -> [HiExpr] -> ExceptT HiError m HiValue
invokeStr str rawArgs = computeArgs rawArgs >>= \case
  [iRaw]       -> listSubscript (T.unpack str) (HiValueString . T.singleton) iRaw
  [aRaw, bRaw] -> listSlice (T.unpack str) (HiValueString . T.pack) aRaw bRaw
  _            -> throwE HiErrorArityMismatch

invokeList :: HiMonad m => Seq.Seq HiValue -> [HiExpr] -> ExceptT HiError m HiValue
invokeList lst rawArgs = computeArgs rawArgs >>= \case
  [iRaw]       -> listSubscript (toList lst) id iRaw
  [aRaw, bRaw] -> listSlice (toList lst) (HiValueList . Seq.fromList) aRaw bRaw
  _            -> throwE HiErrorArityMismatch

invokeByteString :: HiMonad m => B.ByteString -> [HiExpr] -> ExceptT HiError m HiValue
invokeByteString lst rawArgs = computeArgs rawArgs >>= \case
  [iRaw]       -> listSubscript (B.unpack lst) (HiValueNumber . toRational) iRaw
  [aRaw, bRaw] -> listSlice (B.unpack lst) (HiValueBytes . B.pack) aRaw bRaw
  _            -> throwE HiErrorArityMismatch

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


countList :: [HiValue] -> Map.Map HiValue HiValue
countList source = Map.map HiValueNumber $ Map.fromListWith (+) pairList
  where
    pairList = map (, 1) source

unaryStringAction :: HiMonad m => (String -> HiAction) -> [HiValue] -> ExceptT HiError m HiValue
unaryStringAction factory args = unary args >>=
  (string >=> return . HiValueAction . factory . T.unpack)

transformE :: HiError -> HiError -> HiError -> HiError
transformE from to orig
  | orig == from = to
  | otherwise = orig

hiFoldM1 :: HiMonad m
         => (HiValue -> HiValue -> ExceptT HiError m HiValue)
         -> [HiValue]
         -> ExceptT HiError m HiValue
hiFoldM1 _ []      = throwE HiErrorInvalidArgument
hiFoldM1 f [x]     = mapExceptT (fmap $ \case
  Left HiErrorArityMismatch -> Left HiErrorArityMismatch
  _                         -> Right x) $ f HiValueNull HiValueNull
hiFoldM1 f (x: xs) = foldM f x xs

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

binaryArithmetic :: HiMonad m
                 => [HiValue]
                 -> (Rational -> Rational -> Rational)
                 -> ExceptT HiError m HiValue
binaryArithmetic = binaryOperation numeric numeric HiValueNumber

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

assertBinary :: HiMonad m
             => (a -> b -> Bool)
             -> HiError
             -> (a -> b -> ExceptT HiError m c)
             -> a -> b
             -> ExceptT HiError m c
assertBinary cond err f a b = if cond a b then f a b else throwE err

assert :: HiMonad m => (a -> Bool) -> HiError -> a -> ExceptT HiError m a
assert cond err val = if cond val then return val else throwE err

numeric :: HiMonad m => HiValue -> ExceptT HiError m Data.Ratio.Rational
numeric (HiValueNumber x) = return x
numeric _                 = throwE HiErrorInvalidArgument

time :: HiMonad m => HiValue -> ExceptT HiError m UTCTime
time (HiValueTime x) = return x
time _               = throwE HiErrorInvalidArgument

integral :: HiMonad m => HiValue -> ExceptT HiError m Integer
integral (HiValueNumber num)
  | denominator num == 1 = return $ numerator num
  | otherwise = throwE HiErrorInvalidArgument
integral _ = throwE HiErrorInvalidArgument

boolean :: HiMonad m => HiValue -> ExceptT HiError m Bool
boolean (HiValueBool x) = return x
boolean _               = throwE HiErrorInvalidArgument

string :: HiMonad m => HiValue -> ExceptT HiError m T.Text
string (HiValueString x) = return x
string _                 = throwE HiErrorInvalidArgument

list :: HiMonad m => HiValue -> ExceptT HiError m (Seq.Seq HiValue)
list (HiValueList x) = return x
list _               = throwE HiErrorInvalidArgument

bytes :: HiMonad m => HiValue -> ExceptT HiError m B.ByteString
bytes (HiValueBytes x) = return x
bytes _                = throwE HiErrorInvalidArgument

function :: HiMonad m => HiValue -> ExceptT HiError m HiFun
function (HiValueFunction x) = return x
function _                   = throwE HiErrorInvalidArgument

dict :: HiMonad m => HiValue -> ExceptT HiError m (Map.Map HiValue HiValue)
dict (HiValueDict x) = return x
dict _               = throwE HiErrorInvalidArgument

unary :: HiMonad m => [t] -> ExceptT HiError m t
unary [a] = return a
unary _   = throwE HiErrorArityMismatch

binary :: HiMonad m => [t] -> ExceptT HiError m (t, t)
binary [a, b] = return (a, b)
binary _      = throwE HiErrorArityMismatch

ternary :: HiMonad m => [t] -> ExceptT HiError m (t, t, t)
ternary [a, b, c] = return (a, b, c)
ternary _         = throwE HiErrorArityMismatch

hiReplicate :: (Semigroup a, Integral b) => a -> b -> a
hiReplicate lhs rhs = stimes rhs lhs

infixr 6 <||>
(<||>) :: Monad m => ExceptT e1 m a -> ExceptT e2 m a -> ExceptT e2 m a
ExceptT mx <||> ExceptT my = ExceptT $ do
    ex <- mx
    case ex of
        Left _  -> my >>= \ey -> return ey
        Right x -> return (Right x)
