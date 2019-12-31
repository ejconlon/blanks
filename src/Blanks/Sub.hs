module Blanks.Sub
  ( Sub (..)
  , SubError (..)
  , ThrowSub (..)
  , rethrowSub
  , runSub
  ) where

import Control.Exception (Exception)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Data.Typeable (Typeable)

data SubError
  = ApplyError !Int !Int
  | UnboundError !Int
  | NonBinderError
  deriving (Eq, Show, Typeable)

instance Exception SubError

class ThrowSub m where
  throwSub :: SubError -> m a

rethrowSub :: (Applicative m, ThrowSub m) => Either SubError a -> m a
rethrowSub = either throwSub pure

newtype Sub a =
  Sub
    { unSub :: Except SubError a
    }
  deriving (Functor, Applicative, Monad)

instance ThrowSub Sub where
  throwSub = Sub . throwError

runSub :: Sub a -> Either SubError a
runSub = runExcept . unSub
