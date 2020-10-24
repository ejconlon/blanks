{-# LANGUAGE UndecidableInstances #-}

module Blanks.LocScope
  ( LocScope (..)
  , pattern LocScopeBound
  , pattern LocScopeFree
  , pattern LocScopeBinder
  , pattern LocScopeEmbed
  , locScopeLocation
  , locScopeFree
  , locScopeEmbed
  , locScopeFromInnerBinder
  , locScopeBind
  , locScopeBindOpt
  , locScopeLift
  , locScopeInnerBinder
  , locScopeInnerBinder1
  , locScopeAbstract
  , locScopeAbstract1
  , locScopeUnAbstract
  , locScopeUnAbstract1
  , locScopeInstantiate
  , locScopeInstantiate1
  , locScopeApply
  , locScopeApply1
  , locScopeLiftAnno
  , locScopeHoistAnno
  , locScopeMapAnno
  ) where

import Blanks.Core (BinderScope)
import Blanks.Located (Colocated, Located (..), askColocated)
import Blanks.NatNewtype (NatNewtype)
import Blanks.ScopeW (ScopeW (..), scopeWAbstract, scopeWAbstract1, scopeWApply, scopeWApply1, scopeWBind,
                      scopeWBindOpt, scopeWEmbed, scopeWFree, scopeWFromInnerBinder, scopeWHoistAnno, scopeWInnerBinder,
                      scopeWInnerBinder1, scopeWInstantiate, scopeWInstantiate1, scopeWLift, scopeWLiftAnno,
                      scopeWMapAnno, scopeWUnAbstract, scopeWUnAbstract1)
import Blanks.Sub (SubError)
import Blanks.Under (pattern UnderScopeBinder, pattern UnderScopeBound, pattern UnderScopeEmbed, pattern UnderScopeFree)
import Control.DeepSeq (NFData (..))
import Control.Monad (ap)
import Control.Monad.Writer (MonadWriter (..))
import Data.Sequence (Seq)

-- | A 'Scope' annotated with some information between constructors.
newtype LocScope l n f a = LocScope
  { unLocScope :: ScopeW (Located l) n f (LocScope l n f) a
  } deriving stock (Functor, Foldable, Traversable)

instance NatNewtype (ScopeW (Located l) n f (LocScope l n f)) (LocScope l n f)

instance (NFData l, NFData n, NFData a, NFData (f (LocScope l n f a))) => NFData (LocScope l n f a) where
  rnf (LocScope s) = seq (rnf s) ()

pattern LocScopeBound :: l -> Int -> LocScope l n f a
pattern LocScopeBound l b = LocScope (ScopeW (Located l (UnderScopeBound b)))

pattern LocScopeFree :: l -> a -> LocScope l n f a
pattern LocScopeFree l a = LocScope (ScopeW (Located l (UnderScopeFree a)))

pattern LocScopeBinder :: l -> Int -> n -> LocScope l n f a -> LocScope l n f a
pattern LocScopeBinder l i n e = LocScope (ScopeW (Located l (UnderScopeBinder i n e)))

pattern LocScopeEmbed :: l -> f (LocScope l n f a) -> LocScope l n f a
pattern LocScopeEmbed l fe = LocScope (ScopeW (Located l (UnderScopeEmbed fe)))

{-# COMPLETE LocScopeBound, LocScopeFree, LocScopeBinder, LocScopeEmbed #-}

-- | Extract the location (annotation) from this scope.
locScopeLocation :: LocScope l n f a -> l
locScopeLocation s =
  case s of
    LocScopeBound l _ -> l
    LocScopeFree l _ -> l
    LocScopeBinder l _ _ _ -> l
    LocScopeEmbed l _ -> l

instance (Monoid l, Functor f) => Applicative (LocScope l n f) where
  pure = LocScopeFree mempty
  (<*>) = ap

instance (Monoid l, Functor f) => Monad (LocScope l n f) where
  return = pure
  s >>= f = locScopeBind go s where
    go a = fmap (\l1 -> let LocScope (ScopeW (Located l2 b)) = f a in LocScope (ScopeW (Located (l1 <> l2) b))) askColocated

instance (Monoid l, Functor f) => MonadWriter l (LocScope l n f) where
  writer (a, l) = LocScopeFree l a
  tell l = LocScopeFree l ()
  listen = locScopeMapAnno (\(Located l a) -> Located l (a, l))
  pass = locScopeMapAnno (\(Located l (a, f)) -> Located (f l) a)

instance (Eq (f (LocScope l n f a)), Eq l, Eq n, Eq a) => Eq (LocScope l n f a) where
  LocScope su == LocScope sv = su == sv

instance (Show (f (LocScope l n f a)), Show l, Show n, Show a) => Show (LocScope l n f a) where
  showsPrec d (LocScope (ScopeW tu)) = showString "LocScope " . showsPrec (d+1) tu

-- * Interface

locScopeFree :: Functor f => a -> Colocated l (LocScope l n f a)
locScopeFree = scopeWFree
{-# INLINE locScopeFree #-}

locScopeEmbed :: Functor f => f (LocScope l n f a) -> Colocated l (LocScope l n f a)
locScopeEmbed = scopeWEmbed
{-# INLINE locScopeEmbed #-}

locScopeFromInnerBinder :: Functor f => BinderScope n (LocScope l n f a) -> Colocated l (LocScope l n f a)
locScopeFromInnerBinder = scopeWFromInnerBinder
{-# INLINE locScopeFromInnerBinder #-}

locScopeBind :: Functor f => (a -> Colocated l (LocScope l n f b)) -> LocScope l n f a -> LocScope l n f b
locScopeBind = scopeWBind
{-# INLINE locScopeBind #-}

locScopeBindOpt :: Functor f => (a -> Maybe (Colocated l (LocScope l n f a))) -> LocScope l n f a -> LocScope l n f a
locScopeBindOpt = scopeWBindOpt
{-# INLINE locScopeBindOpt #-}

locScopeLift :: Traversable f => f a -> Colocated l (LocScope l n f a)
locScopeLift = scopeWLift
{-# INLINE locScopeLift #-}

locScopeInnerBinder :: (Functor f, Eq a) => n -> Seq a -> LocScope l n f a -> BinderScope n (LocScope l n f a)
locScopeInnerBinder = scopeWInnerBinder
{-# INLINE locScopeInnerBinder #-}

locScopeInnerBinder1 :: (Functor f, Eq a) => n -> a -> LocScope l n f a -> BinderScope n (LocScope l n f a)
locScopeInnerBinder1 = scopeWInnerBinder1
{-# INLINE locScopeInnerBinder1 #-}

locScopeAbstract :: (Functor f, Eq a) => n -> Seq a -> LocScope l n f a -> Colocated l (LocScope l n f a)
locScopeAbstract = scopeWAbstract
{-# INLINE locScopeAbstract #-}

locScopeAbstract1 :: (Functor f, Eq a) => n -> a -> LocScope l n f a -> Colocated l (LocScope l n f a)
locScopeAbstract1 = scopeWAbstract1
{-# INLINE locScopeAbstract1 #-}

locScopeUnAbstract :: Functor f => Seq a -> LocScope l n f a -> LocScope l n f a
locScopeUnAbstract = scopeWUnAbstract
{-# INLINE locScopeUnAbstract #-}

locScopeUnAbstract1 :: Functor f => a -> LocScope l n f a -> LocScope l n f a
locScopeUnAbstract1 = scopeWUnAbstract1
{-# INLINE locScopeUnAbstract1 #-}

locScopeInstantiate :: Functor f => Seq (Colocated l (LocScope l n f a)) -> LocScope l n f a -> LocScope l n f a
locScopeInstantiate = scopeWInstantiate
{-# INLINE locScopeInstantiate #-}

locScopeInstantiate1 :: Functor f => Colocated l (LocScope l n f a) -> LocScope l n f a -> LocScope l n f a
locScopeInstantiate1 = scopeWInstantiate1
{-# INLINE locScopeInstantiate1 #-}

locScopeApply :: Functor f => Seq (Colocated l (LocScope l n f a)) -> LocScope l n f a -> Either SubError (LocScope l n f a)
locScopeApply = scopeWApply
{-# INLINE locScopeApply #-}

locScopeApply1 :: Functor f => Colocated l (LocScope l n f a) -> LocScope l n f a -> Either SubError (LocScope l n f a)
locScopeApply1 = scopeWApply1
{-# INLINE locScopeApply1 #-}

locScopeLiftAnno :: Located l a -> LocScope l n f a
locScopeLiftAnno = scopeWLiftAnno
{-# INLINE locScopeLiftAnno #-}

-- Need an explicit type sig and forall to use this in the hoist below
mapLocatedForall :: (l -> x) -> (forall z. Located l z -> Located x z)
mapLocatedForall f (Located l z) = Located (f l) z

locScopeHoistAnno :: Functor f => (l -> x) -> LocScope l n f a -> LocScope x n f a
locScopeHoistAnno f = scopeWHoistAnno (mapLocatedForall f)
{-# INLINE locScopeHoistAnno #-}

locScopeMapAnno :: Functor f => (Located l a -> Located l b) -> LocScope l n f a -> LocScope l n f b
locScopeMapAnno = scopeWMapAnno
{-# INLINE locScopeMapAnno #-}
