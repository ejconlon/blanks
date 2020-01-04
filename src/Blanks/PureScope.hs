{-# LANGUAGE UndecidableInstances #-}

module Blanks.PureScope
  ( PureScope (..)
  , PureScopeFold
  , pureScopeBind
  , pureScopeEmbed
  , pureScopeFold
  , pureScopeFree
  ) where

import Blanks.Class
import Blanks.ScopeT (ScopeT (..), scopeTBind, scopeTFree, scopeTRawFold)
import Blanks.UnderScope (EmbedScope (..), UnderScope (..), UnderScopeFold (..), underScopeFoldContraMap)
import Control.Monad (ap)
import Control.Monad.Identity (Identity (..))

newtype PureScope n f a = PureScope
  { unPureScope :: ScopeT Identity n f a
  } deriving (Functor, Foldable, Traversable, BlankAbstract)

type instance BlankInfo (PureScope n f) = n
type instance BlankFunctor (PureScope n f) = f
type instance BlankCodomain (PureScope n f) = Identity

instance Functor f => Applicative (PureScope n f) where
  pure = pureScopeFree
  (<*>) = ap

instance Functor f => Monad (PureScope n f) where
  return = pureScopeFree
  s >>= f = pureScopeBind f s

instance Functor f => BlankEmbed (PureScope n f) where
  blankEmbed = Identity . pureScopeEmbed

instance (Eq (f (ScopeT Identity n f a)), Eq n, Eq a) => Eq (PureScope n f a) where
  PureScope su == PureScope sv = su == sv

instance (Show (f (ScopeT Identity n f a)), Show n, Show a) => Show (PureScope n f a) where
  showsPrec d (PureScope (ScopeT tu)) = showString "PureScope " . showsPrec (d+1) tu

pureScopeFree :: a -> PureScope n f a
pureScopeFree = PureScope . runIdentity . scopeTFree

pureScopeBind :: Functor f => (a -> PureScope n f b) -> PureScope n f a -> PureScope n f b
pureScopeBind f = PureScope . scopeTBind (Identity . unPureScope . f) . unPureScope

pureScopeEmbed :: Functor f => f (PureScope n f a) -> PureScope n f a
pureScopeEmbed fe = PureScope (ScopeT (Identity (UnderEmbedScope (EmbedScope (fmap unPureScope fe)))))

type PureScopeFold n f a r = UnderScopeFold n f (PureScope n f a) a r

pureScopeFold :: Traversable f => PureScopeFold n f a r -> PureScope n f a -> r
pureScopeFold usf = runIdentity . scopeTRawFold (underScopeFoldContraMap PureScope usf) . unPureScope
