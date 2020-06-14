module Blanks.Sub
  ( SubError (..)
  , ThrowSub (..)
  , rethrowSub
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad.Except (ExceptT, throwError)

data SubError
  = ApplyError !Int !Int
  | UnboundError !Int
  | NonBinderError
  deriving (Eq, Show)

instance Exception SubError

class ThrowSub m where
  throwSub :: SubError -> m a

rethrowSub :: (Applicative m, ThrowSub m) => Either SubError a -> m a
rethrowSub = either throwSub pure

instance ThrowSub (Either SubError) where
  throwSub = Left

instance Monad m => ThrowSub (ExceptT SubError m) where
  throwSub = throwError

instance ThrowSub IO where
  throwSub = throwIO
