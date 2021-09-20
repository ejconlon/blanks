{-# LANGUAGE DeriveAnyClass #-}

module Blanks.Util.Sub
  ( SubError (..)
  , ThrowSub (..)
  , rethrowSub
  ) where

import Control.DeepSeq (NFData)
import Control.Exception (Exception, throwIO)
import GHC.Generics (Generic)

-- | Errors that happen in the course of instantiation
data SubError
  = ApplyError !Int !Int
  | NonBinderError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance Exception SubError

-- | Some monadic context that lets you throw a 'SubError'.
-- Exists to let you rethrow to a more convenient context rather than
-- pattern maching.
class ThrowSub m where
  throwSub :: SubError -> m a

rethrowSub :: (Applicative m, ThrowSub m) => Either SubError a -> m a
rethrowSub = either throwSub pure

instance ThrowSub (Either SubError) where
  throwSub = Left

instance ThrowSub IO where
  throwSub = throwIO
