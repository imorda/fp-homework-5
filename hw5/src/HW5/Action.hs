{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE InstanceSigs #-}

module HW5.Action
  ( HiPermission(..)
  , PermissionException(..)
  , HIO(..)
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Control.Monad.Trans.Reader
import qualified Data.ByteString as B
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import qualified Data.Text.IO as T.IO
import Data.Time.Clock (getCurrentTime)
import HW5.Base
import System.Directory
import System.Random


data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Ord, Eq)

newtype PermissionException =
  PermissionRequired HiPermission
  deriving Show

instance Exception PermissionException

newtype HIO a =
  HIO { runHIO :: Set.Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad) via (ReaderT (Set.Set HiPermission) IO)

instance HiMonad HIO where
  runAction :: HiAction -> HIO HiValue
  runAction action = HIO $ \perms -> case action of
    HiActionCwd -> do
      assertPermission AllowRead perms
      HiValueString . T.pack <$> getCurrentDirectory
    (HiActionChDir newDir) -> do
      assertPermission AllowRead perms
      setCurrentDirectory newDir
      return HiValueNull
    (HiActionRead path) -> do
      assertPermission AllowRead perms
      fileExists <- doesFileExist path
      if fileExists
        then do
          rawBytes <- B.readFile path
          return $ case decodeUtf8' rawBytes of
            Right decodedStr -> HiValueString decodedStr
            _                -> HiValueBytes rawBytes
        else do -- It is directory
          rawListing <- listDirectory path
          return $ HiValueList $ Seq.fromList $ map (HiValueString . T.pack) rawListing
    (HiActionWrite path value) -> do
      assertPermission AllowWrite perms
      B.writeFile path value
      return HiValueNull
    (HiActionMkDir path) -> do
      assertPermission AllowWrite perms
      createDirectory path
      return HiValueNull

    HiActionNow -> do
      assertPermission AllowTime perms
      HiValueTime <$> getCurrentTime

    (HiActionRand from to) -> HiValueNumber . toRational <$> getStdRandom (uniformR (from, to))

    (HiActionEcho text) -> do
      assertPermission AllowWrite perms
      T.IO.putStrLn text
      return HiValueNull


assertPermission :: HiPermission -> Set.Set HiPermission -> IO ()
assertPermission expected perms = when (Set.notMember expected perms)
  (throwIO $ PermissionRequired expected)
