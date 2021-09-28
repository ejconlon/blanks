{-# LANGUAGE DeriveAnyClass #-}

-- | Internals.
module Blanks.Internal.Under
  ( UnderScope (..)
  , underScopeShift
  ) where

import Blanks.Internal.Abstract (Abstract, IsAbstractInfo (..), ShouldShift (..))
import Control.DeepSeq (NFData)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Kind (Type)
import GHC.Generics (Generic)

data UnderScope (n :: Type -> Type) (f :: Type -> Type) (e :: Type) (a :: Type)
  = UnderScopeBound !Int
  | UnderScopeFree !a
  | UnderScopeAbstract !(Abstract n e)
  | UnderScopeEmbed !(f e)
  deriving stock (Eq, Show, Functor, Generic)
  deriving anyclass (NFData)

instance (Functor n, Functor f) => Bifunctor (UnderScope n f) where
  bimap _ _ (UnderScopeBound b) = UnderScopeBound b
  bimap _ g (UnderScopeFree a) = UnderScopeFree (g a)
  bimap f _ (UnderScopeAbstract ab) = UnderScopeAbstract (fmap f ab)
  bimap f _ (UnderScopeEmbed fe) = UnderScopeEmbed (fmap f fe)

instance (Foldable n, Foldable f) => Bifoldable (UnderScope n f) where
  bifoldr _ _ z (UnderScopeBound _) = z
  bifoldr _ g z (UnderScopeFree a) = g a z
  bifoldr f _ z (UnderScopeAbstract ab) = foldr f z ab
  bifoldr f _ z (UnderScopeEmbed fe) = foldr f z fe

instance (Traversable n, Traversable f) => Bitraversable (UnderScope n f) where
  bitraverse _ _ (UnderScopeBound b) = pure (UnderScopeBound b)
  bitraverse _ g (UnderScopeFree a) = fmap UnderScopeFree (g a)
  bitraverse f _ (UnderScopeAbstract ab) = fmap UnderScopeAbstract (traverse f ab)
  bitraverse f _ (UnderScopeEmbed fe) = fmap UnderScopeEmbed (traverse f fe)

underScopeShift :: (IsAbstractInfo n, Functor f) => (Int -> Int -> e -> e) -> Int -> Int -> UnderScope n f e a -> UnderScope n f e a
underScopeShift recShift c d us =
  case us of
    UnderScopeBound b ->
      if b < c
        then us
        else UnderScopeBound (b + d)
    UnderScopeFree _ -> us
    UnderScopeAbstract ab ->
      let i = abstractInfoArity ab
      in UnderScopeAbstract $ flip abstractInfoMapShouldShift ab $ \_ ss ->
        let c' = case ss of { ShouldShiftYes -> c + i ; ShouldShiftNo -> c }
        in recShift c' d
    UnderScopeEmbed fe -> UnderScopeEmbed (fmap (recShift c d) fe)
