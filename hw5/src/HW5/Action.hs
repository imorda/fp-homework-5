{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE InstanceSigs #-}

module HW5.Action
  (
  -- * Action implementation types
    HiPermission(..)
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

-- | Supported permissions.
data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Ord, Eq)

-- | Exception thrown when a permission is required but not granted.
newtype PermissionException =
  PermissionRequired HiPermission
  deriving Show

instance Exception PermissionException

-- | The 'HIO' monad is a wrapper over 'IO' which keeps track of the permissions
newtype HIO a =
  HIO { runHIO :: Set.Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad) via (ReaderT (Set.Set HiPermission) IO)

-- | The 'HiMonad' instance for 'HIO' is implemented by delegating to 'IO' and checking permissions.
instance HiMonad HIO where
  {-|
  The 'HiActionCwd' action returns the current working directory.
  Requires 'AllowRead' permission.
  
  The 'HiActionChDir' action changes the current working directory.
   -- Requires 'AllowRead' permission.
  
  The 'HiActionRead' action reads a file or lists a directory.
  Requires 'AllowRead' permission.
  
  The 'HiActionWrite' action writes a ByteString to a file.
  Requires 'AllowWrite' permission.
  
  The 'HiActionMkDir' action creates a directory.
  Requires 'AllowWrite' permission.
  
  The 'HiActionNow' action returns the current time.
  Requires 'AllowTime' permission.
  
  The 'HiActionRand' action returns a random number in the given range.
  Does not require any permissions.
  
  The 'HiActionEcho' action prints a given string to stdout.
  Requires 'AllowWrite' permission.
  -}
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

-- | Asserts that a permission is granted, otherwise throws a 'PermissionRequired' exception.
assertPermission :: HiPermission -> Set.Set HiPermission -> IO ()
assertPermission expected perms = when (Set.notMember expected perms)
  (throwIO $ PermissionRequired expected)
