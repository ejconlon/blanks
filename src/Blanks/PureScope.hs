{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Blanks.PureScope
  ( PureScope
  , pattern PureScopeBound
  , pattern PureScopeFree
  , pattern PureScopeBinder
  , pattern PureScopeEmbed
  ) where

import Blanks.Interface (Blank, BlankLeft, BlankRight, BlankInfo, BlankFunctor, blankFree, blankBind)
import Blanks.NatNewtype (NatNewtype)
import Blanks.ScopeW (ScopeW (..))
import Blanks.UnderScope (pattern UnderScopeBound, pattern UnderScopeFree, pattern UnderScopeBinder, pattern UnderScopeEmbed)
import Control.Monad (ap)
import Control.Monad.Identity (Identity (..))

newtype PureScope n f a = PureScope
  { unPureScope :: ScopeW Identity n f (PureScope n f) a
  } deriving (Functor, Foldable, Traversable)

type instance BlankLeft (PureScope n f) = Identity
type instance BlankRight (PureScope n f) = Identity
type instance BlankInfo (PureScope n f) = n
type instance BlankFunctor (PureScope n f) = f

instance Functor f => Blank (PureScope n f)
instance NatNewtype (ScopeW Identity n f (PureScope n f)) (PureScope n f)

pattern PureScopeBound :: Int -> PureScope n f a
pattern PureScopeBound b = PureScope (ScopeW (Identity (UnderScopeBound b)))

pattern PureScopeFree :: a -> PureScope n f a
pattern PureScopeFree a = PureScope (ScopeW (Identity (UnderScopeFree a)))

pattern PureScopeBinder :: Int -> n -> PureScope n f a -> PureScope n f a
pattern PureScopeBinder i n e = PureScope (ScopeW (Identity (UnderScopeBinder i n e)))

pattern PureScopeEmbed :: f (PureScope n f a) -> PureScope n f a
pattern PureScopeEmbed fe = PureScope (ScopeW (Identity (UnderScopeEmbed fe)))

{-# COMPLETE PureScopeBound, PureScopeFree, PureScopeBinder, PureScopeEmbed #-}

instance Functor f => Applicative (PureScope n f) where
  pure = runIdentity . blankFree
  (<*>) = ap

instance Functor f => Monad (PureScope n f) where
  return = pure
  s >>= f = blankBind (Identity . f) s

instance (Eq (f (PureScope n f a)), Eq n, Eq a) => Eq (PureScope n f a) where
  PureScope su == PureScope sv = su == sv

instance (Show (f (PureScope n f a)), Show n, Show a) => Show (PureScope n f a) where
  showsPrec d (PureScope (ScopeW tu)) = showString "PureScope " . showsPrec (d+1) tu
