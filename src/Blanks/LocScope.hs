{-# LANGUAGE UndecidableInstances #-}

module Blanks.LocScope
  ( Colocated (..)
  , Located (..)
  , LocScope (..)
  , LocScopeFold
  , colocated
  , locScopeBind
  , locScopeEmbed
  , locScopeFold
  , locScopeFree
  , runColocated
  ) where

import Blanks.Class
import Blanks.RightAdjunct (RightAdjunct)
import Blanks.ScopeT (ScopeT (..), scopeTBind, scopeTFold, scopeTFree)
import Blanks.UnderScope (EmbedScope (..), UnderScope (..), UnderScopeFold (..), underScopeFoldContraMap)
import Control.Monad (ap)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (MonadReader, Reader, ReaderT (..), runReader)
import Data.Distributive (Distributive (..))
import Data.Functor.Adjunction (Adjunction (..))
import Data.Functor.Rep (Representable)

data Located l a = Located
  { _locatedLoc :: l
  , _locatedVal :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Colocated l a = Colocated
  { unColocated :: Reader l a
  } deriving (Functor, Applicative, Monad, MonadReader l, Representable)

type instance RightAdjunct (Located l) = Colocated l

colocated :: (l -> a) -> Colocated l a
colocated f = Colocated (ReaderT (Identity . f))

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

newtype LocScope l n f a = LocScope
  { unLocScope :: ScopeT (Located l) n f a
  } deriving (Functor, Foldable, Traversable, BlankAbstract)

type instance BlankInfo (LocScope l n f) = n
type instance BlankFunctor (LocScope l n f) = f
type instance BlankCodomain (LocScope l n f) = Colocated l

instance Functor f => BlankEmbed (LocScope l n f) where
  blankEmbed = locScopeEmbed

instance (Eq (f (ScopeT (Located l) n f a)), Eq l, Eq n, Eq a) => Eq (LocScope l n f a) where
  LocScope su == LocScope sv = su == sv

instance (Show (f (ScopeT (Located l) n f a)), Show l, Show n, Show a) => Show (LocScope l n f a) where
  showsPrec d (LocScope (ScopeT tu)) = showString "LocScope " . showsPrec (d+1) tu

locScopeEmbed :: Functor f => f (LocScope l n f a) -> Colocated l (LocScope l n f a)
locScopeEmbed fe = colocated (\l -> LocScope (ScopeT (Located l (UnderEmbedScope (EmbedScope (fmap unLocScope fe))))))

locScopeBind :: Functor f => (a -> Colocated l (LocScope l n f b)) -> LocScope l n f a -> LocScope l n f b
locScopeBind f = LocScope . scopeTBind (fmap unLocScope . f) . unLocScope

locScopeFree :: a -> Colocated l (LocScope l n f a)
locScopeFree = fmap LocScope . scopeTFree

type LocScopeFold l n f a r = UnderScopeFold n f (LocScope l n f a) a (Colocated l r)

locScopeFold :: Functor f => LocScopeFold l n f a r -> LocScope l n f a -> r
locScopeFold usf = scopeTFold (underScopeFoldContraMap LocScope usf) . unLocScope
