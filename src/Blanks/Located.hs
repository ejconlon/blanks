{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Blanks.Located
  ( Colocated (..)
  , Located (..)
  , askColocated
  , colocated
  , runColocated
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (ap)
import Control.Monad.Reader (MonadReader, Reader, ReaderT (..), ask, reader, runReader)
import Control.Monad.Writer (MonadWriter (..))
import Data.Distributive (Distributive (..))
import Data.Functor.Adjunction (Adjunction (..))
import Data.Functor.Rep (Representable)
import GHC.Generics (Generic)

-- | This is basically the 'Env' comonad, but with the env strict.
-- It's also basically the 'Writer' monad in certain contexts.
-- We define a new, non-transforming datatype so we can pattern-match.
data Located l a = Located
  { locatedLoc :: !l
  , locatedVal :: a
  } deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (NFData)

-- | Because we defined a unique left adjoint, we have to define the unique right.
newtype Colocated l a = Colocated
  { unColocated :: Reader l a
  } deriving newtype (Functor, Applicative, Monad, MonadReader l, Representable)

colocated :: (l -> a) -> Colocated l a
colocated f = Colocated (reader f)

askColocated :: Colocated l l
askColocated = Colocated ask

runColocated :: Colocated l a -> l -> a
runColocated = runReader . unColocated

instance Distributive (Colocated l) where
  distribute = Colocated . distribute . fmap unColocated

instance Adjunction (Located l) (Colocated l) where
  leftAdjunct v a = colocated (v . flip Located a)
  rightAdjunct h (Located l a) = runColocated (h a) l

instance Monoid l => Applicative (Located l) where
  pure = Located mempty
  (<*>) = ap

instance Monoid l => Monad (Located l) where
  return = pure
  Located l a >>= f = let Located p b = f a in Located (l <> p) b

instance Monoid l => MonadWriter l (Located l) where
  writer (a, l) = Located l a
  tell l = Located l ()
  listen (Located l a) = Located l (a, l)
  pass (Located l (a, f)) = Located (f l) a
