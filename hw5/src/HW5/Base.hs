{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module HW5.Base
  ( 
  -- * Hi language types
    HiError(..)
  , HiExpr(..)
  , HiFun(..)
  , HiValue(..)
  , HiMonad(..)
  , HiAction(..)
  ) where

import Codec.Serialise
import qualified Data.ByteString as B
import Data.Map (Map)
import Data.Ratio
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Time.Clock
import GHC.Generics

-- | Function names.
data HiFun =
    HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub

  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf

  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim

  | HiFunList
  | HiFunRange
  | HiFunFold

  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise

  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir

  | HiFunParseTime

  | HiFunRand

  | HiFunEcho

  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert

  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise)

-- | Hi language values.
data HiValue =
    HiValueBool Bool
  | HiValueNumber Data.Ratio.Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString T.Text
  | HiValueList (Seq.Seq HiValue)
  | HiValueBytes B.ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise)

-- | Hi language expressions. Represents the evaluable abstract syntax tree.
data HiExpr =
    HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving Show

-- | Hi language evaluation errors.
data HiError =
    HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq)

-- | Hi language actions. Support permission control. Enables IO.
data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath B.ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho T.Text
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise)

-- | Hi language monad for actions.
class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
