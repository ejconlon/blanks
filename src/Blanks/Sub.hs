module Blanks.Sub
  ( SubError (..)
  , ThrowSub (..)
  , rethrowSub
  ) where

import Control.Exception (Exception, throwIO)

-- | Errors that happen in the course of instantiation, thrown by 'blankApply'
-- and related functions.
data SubError
  = ApplyError !Int !Int
  | UnboundError !Int
  | NonBinderError
  deriving (Eq, Show)

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
