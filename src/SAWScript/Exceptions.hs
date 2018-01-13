{-# LANGUAGE ScopedTypeVariables #-}
module SAWScript.Exceptions (TypeErrors(..), failTypecheck,
                             SAWRuntimeError(..), failRuntime, failRuntimeIO) where

import Control.Exception
import Control.Monad.IO.Class

import SAWScript.Utils


newtype TypeErrors = TypeErrors [(Pos, String)]

instance Show TypeErrors where
  show (TypeErrors []) = "Unspecified type error"
  show (TypeErrors [(pos, msg)]) = show pos ++ ": " ++ msg
  show (TypeErrors errs) = "Type errors:\n" ++ showErrs errs
    where showErrs = unlines . map showErr
          showErr (pos, msg) = "  " ++ show pos ++ ": " ++ msg

instance Exception TypeErrors where

failTypecheck :: [(Pos, String)] -> a
failTypecheck = throw . TypeErrors


newtype SAWRuntimeError = SAWRuntimeError String

instance Show SAWRuntimeError where
  show (SAWRuntimeError err) = err

instance Exception SAWRuntimeError where

failRuntime :: String -> a
failRuntime = throw . SAWRuntimeError

failRuntimeIO :: MonadIO m => String -> m a
failRuntimeIO = liftIO . throwIO . SAWRuntimeError



