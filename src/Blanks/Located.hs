{-# LANGUAGE UndecidableInstances #-}

module Blanks.Located
  ( Colocated (..)
  , Located (..)
  , askColocated
  , colocated
  , runColocated
  ) where

import Blanks.RightAdjunct (RightAdjunct)
import Control.Monad (ap)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (MonadReader, Reader, ReaderT (..), ask, runReader)
import Data.Distributive (Distributive (..))
import Data.Functor.Adjunction (Adjunction (..))
import Data.Functor.Rep (Representable)

data Located l a = Located
  { _locatedLoc :: !l
  , _locatedVal :: !a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Colocated l a = Colocated
  { unColocated :: Reader l a
  } deriving (Functor, Applicative, Monad, MonadReader l, Representable)

type instance RightAdjunct (Located l) = Colocated l

colocated :: (l -> a) -> Colocated l a
colocated f = Colocated (ReaderT (Identity . f))

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
