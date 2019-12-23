{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Blanks.Scope
  ( BoundScope (..)
  , FreeScope (..)
  , BinderScope (..)
  , EmbedScope (..)
  , UnderScope (..)
  , Scope (..)
  , Binder
  , _UnderScope
  , _UnderBoundScope
  , _UnderFreeScope
  , _UnderBinderScope
  , _UnderEmbedScope
  , BottomUp
  , scopeFreeVars
  , transformScope
  , abstract
  , abstract1
  , unAbstract
  , unAbstract1
  , instantiate
  , instantiate1
  , apply
  , apply1
  , simpleEmbed
  , simpleAbstract1
  , simpleAbstract
  ) where

import Blanks.Sub (SubError (..), ThrowSub (..))
import Control.Lens (Iso', iso)
import Control.Lens.TH (makePrisms)
import Control.Monad (ap)
import Control.Monad.Trans (MonadTrans (..))
import Control.Newtype.Generics (Newtype)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)

newtype BoundScope =
  BoundScope
    { unBoundScope :: Int
    }
  deriving (Eq, Show)

newtype FreeScope a =
  FreeScope
    { unFreeScope :: a
    }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data BinderScope n e =
  BinderScope
    { binderScopeArity :: !Int
    , binderScopeInfo :: !n
    , binderScopeBody :: !e
    }
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype EmbedScope f e =
  EmbedScope
    { unEmbedScope :: f e
    }
  deriving (Eq, Show, Functor)

data UnderScope n f e a
  = UnderBoundScope !BoundScope
  | UnderFreeScope !(FreeScope a)
  | UnderBinderScope !(BinderScope n e)
  | UnderEmbedScope !(EmbedScope f e)
  deriving (Eq, Show, Functor, Foldable, Traversable)

$(makePrisms ''UnderScope)

instance Functor f => Bifunctor (UnderScope n f) where
  bimap _ _ (UnderBoundScope (BoundScope b)) = UnderBoundScope (BoundScope b)
  bimap _ g (UnderFreeScope (FreeScope a)) = UnderFreeScope (FreeScope (g a))
  bimap f _ (UnderBinderScope (BinderScope i x e)) = UnderBinderScope (BinderScope i x (f e))
  bimap f _ (UnderEmbedScope (EmbedScope fe)) = UnderEmbedScope (EmbedScope (f <$> fe))

instance Foldable f => Bifoldable (UnderScope n f) where
  bifoldr _ _ z (UnderBoundScope _) = z
  bifoldr _ g z (UnderFreeScope (FreeScope a)) = g a z
  bifoldr f _ z (UnderBinderScope (BinderScope _ _ e)) = f e z
  bifoldr f _ z (UnderEmbedScope (EmbedScope fe)) = foldr f z fe

instance Traversable f => Bitraversable (UnderScope n f) where
  bitraverse _ _ (UnderBoundScope (BoundScope b)) = pure (UnderBoundScope (BoundScope b))
  bitraverse _ g (UnderFreeScope (FreeScope a)) = UnderFreeScope . FreeScope <$> g a
  bitraverse f _ (UnderBinderScope (BinderScope i x e)) = UnderBinderScope . BinderScope i x <$> f e
  bitraverse f _ (UnderEmbedScope (EmbedScope fe)) = UnderEmbedScope . EmbedScope <$> traverse f fe

newtype Scope n f a =
  Scope
    { unScope :: UnderScope n f (Scope n f a) a
    } deriving (Generic)

type Binder n f a = BinderScope n (Scope n f a)

_UnderScope :: Iso' (Scope n f a) (UnderScope n f (Scope n f a) a)
_UnderScope = iso unScope Scope

instance Newtype (Scope n f a)

type BottomUp n f g a = f (Scope n g a) -> g (Scope n g a)

instance (Eq (f (Scope n f a)), Eq n, Eq a) => Eq (Scope n f a) where
  Scope u == Scope v = u == v

instance (Show (f (Scope n f a)), Show n, Show a) => Show (Scope n f a) where
  showsPrec d (Scope u) = showsPrec d u

instance Functor f => Functor (Scope n f) where
  fmap f (Scope us) = Scope (bimap (fmap f) f us)

instance Foldable f => Foldable (Scope n f) where
  foldr f z (Scope us) = bifoldr (flip (foldr f)) f z us

instance Traversable f => Traversable (Scope n f) where
  traverse f (Scope us) = Scope <$> bitraverse (traverse f) f us

instance Functor f => Applicative (Scope n f) where
  pure = Scope . UnderFreeScope . FreeScope
  (<*>) = ap

subScopeShift :: Functor f => Int -> Int -> Scope n f a -> Scope n f a
subScopeShift c d s@(Scope us) =
  case us of
    UnderBoundScope (BoundScope b) ->
      if b < c
        then s
        else Scope (UnderBoundScope (BoundScope (b + d)))
    UnderFreeScope _ -> s
    UnderBinderScope (BinderScope i x e) -> Scope (UnderBinderScope (BinderScope i x (subScopeShift (c + i) d e)))
    UnderEmbedScope fe -> Scope (UnderEmbedScope (subScopeShift c d <$> fe))

scopeShift :: Functor f => Int -> Scope n f a -> Scope n f a
scopeShift = subScopeShift 0

scopeBind :: Functor f => Int -> Scope n f a -> (a -> Scope n f b) -> Scope n f b
scopeBind n (Scope us) f =
  case us of
    UnderBoundScope (BoundScope b) -> Scope (UnderBoundScope (BoundScope b))
    UnderFreeScope (FreeScope a) -> scopeShift n (f a)
    UnderBinderScope (BinderScope i x e) -> Scope (UnderBinderScope (BinderScope i x (scopeBind (n + i) e f)))
    UnderEmbedScope (EmbedScope fe) -> Scope (UnderEmbedScope (EmbedScope ((\e -> scopeBind n e f) <$> fe)))

scopeBindOpt :: Functor f => Int -> Scope n f a -> (a -> Maybe (Scope n f a)) -> Scope n f a
scopeBindOpt n s@(Scope us) f =
  case us of
    UnderBoundScope _ -> s
    UnderFreeScope (FreeScope a) ->
      case f a of
        Nothing -> s
        Just s' -> scopeShift n s'
    UnderBinderScope (BinderScope i x e) -> Scope (UnderBinderScope (BinderScope i x (scopeBindOpt (n + i) e f)))
    UnderEmbedScope (EmbedScope fe) -> Scope (UnderEmbedScope (EmbedScope ((\e -> scopeBindOpt n e f) <$> fe)))

instance Functor f => Monad (Scope n f) where
  return = pure
  (>>=) = scopeBind 0

instance MonadTrans (Scope n) where
  lift = Scope . UnderEmbedScope . EmbedScope . fmap pure

scopeFreeVars :: (Foldable f, Ord a) => Scope n f a -> Set a
scopeFreeVars = Set.fromList . toList

transformScope :: Functor f => BottomUp n f g a -> Scope n f a -> Scope n g a
transformScope t (Scope us) =
  case us of
    UnderBoundScope (BoundScope b) -> Scope (UnderBoundScope (BoundScope b))
    UnderFreeScope (FreeScope a) -> Scope (UnderFreeScope (FreeScope a))
    UnderBinderScope (BinderScope i x e) -> Scope (UnderBinderScope (BinderScope i x (transformScope t e)))
    UnderEmbedScope (EmbedScope fe) -> Scope (UnderEmbedScope (EmbedScope (t (transformScope t <$> fe))))

subAbstract :: (Functor f, Eq a) => Int -> n -> Seq a -> Scope n f a -> BinderScope n (Scope n f a)
subAbstract n x ks s =
  BinderScope n x (scopeBindOpt 0 s ((Scope . UnderBoundScope . BoundScope <$>) . flip Seq.elemIndexL ks))

subInstantiate :: Functor f => Int -> Seq (Scope n f a) -> Scope n f a -> Scope n f a
subInstantiate n vs s@(Scope us) =
  case us of
    UnderBoundScope (BoundScope b) -> fromMaybe s (vs Seq.!? (b - n))
    UnderFreeScope _ -> s
    UnderBinderScope (BinderScope i x e) ->
      Scope (UnderBinderScope (BinderScope i x (subInstantiate (n + i) (scopeShift i <$> vs) e)))
    UnderEmbedScope (EmbedScope fe) -> Scope (UnderEmbedScope (EmbedScope (subInstantiate n vs <$> fe)))

abstract :: (Functor f, Eq a) => n -> Seq a -> Scope n f a -> Binder n f a
abstract x ks =
  let n = Seq.length ks
   in subAbstract n x ks . scopeShift n

unAbstract :: Functor f => Seq a -> Scope n f a -> Scope n f a
unAbstract as = instantiate (Scope . UnderFreeScope . FreeScope <$> as)

instantiate :: Functor f => Seq (Scope n f a) -> Scope n f a -> Scope n f a
instantiate = subInstantiate 0

rawApply :: (ThrowSub m, Applicative m, Functor f) => Seq (Scope n f a) -> Int -> Scope n f a -> m (Scope n f a)
rawApply vs i e =
  let len = Seq.length vs
   in if len == i
        then pure (scopeShift (-1) (instantiate vs e))
        else throwSub (ApplyError len i)

apply :: (ThrowSub m, Applicative m, Functor f) => Seq (Scope n f a) -> Binder n f a -> m (Scope n f a)
apply vs (BinderScope i _ e) = rawApply vs i e

abstract1 :: (Functor f, Eq a) => n -> a -> Scope n f a -> Binder n f a
abstract1 n k = abstract n (Seq.singleton k)

unAbstract1 :: Functor f => a -> Scope n f a -> Scope n f a
unAbstract1 a = unAbstract (Seq.singleton a)

instantiate1 :: Functor f => Scope n f a -> Scope n f a -> Scope n f a
instantiate1 v = instantiate (Seq.singleton v)

apply1 :: (ThrowSub m, Applicative m, Functor f) => Scope n f a -> Binder n f a -> m (Scope n f a)
apply1 v = apply (Seq.singleton v)

simpleEmbed :: f (Scope n f a) -> Scope n f a
simpleEmbed = Scope . UnderEmbedScope . EmbedScope

simpleAbstract1 :: (Functor f, Eq a) => n -> a -> Scope n f a -> Scope n f a
simpleAbstract1 n k = Scope . UnderBinderScope . abstract1 n k

simpleAbstract :: (Functor f, Eq a) => n -> Seq a -> Scope n f a -> Scope n f a
simpleAbstract n ks = Scope. UnderBinderScope . abstract n ks
