{-# LANGUAGE UndecidableInstances #-}

-- | Read the docs for 'Scope' first. 'LocScope' adds location annotations to the scope tree.
module Blanks.LocScope
  ( LocScope (..)
  , pattern LocScopeBound
  , pattern LocScopeFree
  , pattern LocScopeAbstract
  , pattern LocScopeEmbed
  , locScopeLocation
  , locScopeFree
  , locScopeAbstract
  , locScopeEmbed
  , locScopeBind
  , locScopeBindOpt
  , locScopeLift
  , locScopeBindFree
  , locScopeBindFree1
  , locScopeFillBound
  , locScopeFillBound1
  , locScopeUnBindFree
  , locScopeUnBindFree1
  , locScopeApply
  , locScopeApply1
  , locScopeLiftAnno
  , locScopeHoistAnno
  , locScopeMapAnno
  )
where

import Blanks.Internal.Abstract (Abstract, IsAbstractInfo)
import Blanks.Internal.ScopeW
import Blanks.Internal.Under (UnderScope (..))
import Blanks.Util.Located (Colocated, Located (..), askColocated)
import Blanks.Util.NatNewtype (NatNewtype)
import Blanks.Util.Sub (SubError)
import Control.DeepSeq (NFData (..))
import Control.Monad (ap)
import Control.Monad.Writer (MonadWriter (..))
import Data.Sequence (Seq)

-- | A 'Scope' annotated with some information between constructors.
newtype LocScope l n f a = LocScope
  { unLocScope :: ScopeW (Located l) n f (LocScope l n f) a
  }
  deriving stock (Functor, Foldable, Traversable)

instance NatNewtype (ScopeW (Located l) n f (LocScope l n f)) (LocScope l n f)

instance (NFData l, NFData (n (LocScope l n f a)), NFData a, NFData (f (LocScope l n f a))) => NFData (LocScope l n f a) where
  rnf (LocScope s) = seq (rnf s) ()

pattern LocScopeBound :: l -> Int -> LocScope l n f a
pattern LocScopeBound l b = LocScope (ScopeW (Located l (UnderScopeBound b)))

pattern LocScopeFree :: l -> a -> LocScope l n f a
pattern LocScopeFree l a = LocScope (ScopeW (Located l (UnderScopeFree a)))

pattern LocScopeAbstract :: l -> Abstract n (LocScope l n f a) -> LocScope l n f a
pattern LocScopeAbstract l ab = LocScope (ScopeW (Located l (UnderScopeAbstract ab)))

pattern LocScopeEmbed :: l -> f (LocScope l n f a) -> LocScope l n f a
pattern LocScopeEmbed l fe = LocScope (ScopeW (Located l (UnderScopeEmbed fe)))

{-# COMPLETE LocScopeBound, LocScopeFree, LocScopeAbstract, LocScopeEmbed #-}

-- | Extract the location (annotation) from this scope.
locScopeLocation :: LocScope l n f a -> l
locScopeLocation s =
  case s of
    LocScopeBound l _ -> l
    LocScopeFree l _ -> l
    LocScopeAbstract l _ -> l
    LocScopeEmbed l _ -> l

instance (Monoid l, IsAbstractInfo n, Functor f) => Applicative (LocScope l n f) where
  pure = LocScopeFree mempty
  (<*>) = ap

instance (Monoid l, IsAbstractInfo n, Functor f) => Monad (LocScope l n f) where
  return = pure
  s >>= f = locScopeBind go s
   where
    go a = fmap (\l1 -> let LocScope (ScopeW (Located l2 b)) = f a in LocScope (ScopeW (Located (l1 <> l2) b))) askColocated

instance (Monoid l, IsAbstractInfo n, Functor f) => MonadWriter l (LocScope l n f) where
  writer (a, l) = LocScopeFree l a
  tell l = LocScopeFree l ()
  listen = locScopeMapAnno (\(Located l a) -> Located l (a, l))
  pass = locScopeMapAnno (\(Located l (a, f)) -> Located (f l) a)

instance (Eq (f (LocScope l n f a)), Eq l, Eq (n (LocScope l n f a)), Eq a) => Eq (LocScope l n f a) where
  LocScope su == LocScope sv = su == sv

instance (Show (f (LocScope l n f a)), Show l, Show (n (LocScope l n f a)), Show a) => Show (LocScope l n f a) where
  showsPrec d (LocScope (ScopeW tu)) = showString "LocScope " . showParen True (showsPrec (d + 1) tu)

-- * Interface

locScopeFree :: (IsAbstractInfo n, Functor f) => a -> Colocated l (LocScope l n f a)
locScopeFree = scopeWFree
{-# INLINE locScopeFree #-}

locScopeAbstract :: (IsAbstractInfo n, Functor f) => Abstract n (LocScope l n f a) -> Colocated l (LocScope l n f a)
locScopeAbstract = scopeWAbstract
{-# INLINE locScopeAbstract #-}

locScopeEmbed :: (IsAbstractInfo n, Functor f) => f (LocScope l n f a) -> Colocated l (LocScope l n f a)
locScopeEmbed = scopeWEmbed
{-# INLINE locScopeEmbed #-}

locScopeBind :: (IsAbstractInfo n, Functor f) => (a -> Colocated l (LocScope l n f b)) -> LocScope l n f a -> LocScope l n f b
locScopeBind = scopeWBind
{-# INLINE locScopeBind #-}

locScopeBindOpt :: (IsAbstractInfo n, Functor f) => (a -> Maybe (Colocated l (LocScope l n f a))) -> LocScope l n f a -> LocScope l n f a
locScopeBindOpt = scopeWBindOpt
{-# INLINE locScopeBindOpt #-}

locScopeLift :: (IsAbstractInfo n, Traversable f) => f a -> Colocated l (LocScope l n f a)
locScopeLift = scopeWLift
{-# INLINE locScopeLift #-}

locScopeBindFree :: (IsAbstractInfo n, Functor f, Eq a) => Seq a -> LocScope l n f a -> LocScope l n f a
locScopeBindFree = scopeWBindFree
{-# INLINE locScopeBindFree #-}

locScopeBindFree1 :: (IsAbstractInfo n, Functor f, Eq a) => a -> LocScope l n f a -> LocScope l n f a
locScopeBindFree1 = scopeWBindFree1
{-# INLINE locScopeBindFree1 #-}

locScopeFillBound :: (IsAbstractInfo n, Functor f) => Seq (Colocated l (LocScope l n f a)) -> LocScope l n f a -> LocScope l n f a
locScopeFillBound = scopeWFillBound
{-# INLINE locScopeFillBound #-}

locScopeFillBound1 :: (IsAbstractInfo n, Functor f) => Colocated l (LocScope l n f a) -> LocScope l n f a -> LocScope l n f a
locScopeFillBound1 = scopeWFillBound1
{-# INLINE locScopeFillBound1 #-}

locScopeUnBindFree :: (IsAbstractInfo n, Functor f) => Seq a -> LocScope l n f a -> LocScope l n f a
locScopeUnBindFree = scopeWUnBindFree
{-# INLINE locScopeUnBindFree #-}

locScopeUnBindFree1 :: (IsAbstractInfo n, Functor f) => a -> LocScope l n f a -> LocScope l n f a
locScopeUnBindFree1 = scopeWUnBindFree1
{-# INLINE locScopeUnBindFree1 #-}

locScopeApply :: (IsAbstractInfo n, Functor f) => Seq (Colocated l (LocScope l n f a)) -> LocScope l n f a -> Either SubError (LocScope l n f a)
locScopeApply = scopeWApply
{-# INLINE locScopeApply #-}

locScopeApply1 :: (IsAbstractInfo n, Functor f) => Colocated l (LocScope l n f a) -> LocScope l n f a -> Either SubError (LocScope l n f a)
locScopeApply1 = scopeWApply1
{-# INLINE locScopeApply1 #-}

locScopeLiftAnno :: Located l a -> LocScope l n f a
locScopeLiftAnno = scopeWLiftAnno
{-# INLINE locScopeLiftAnno #-}

-- Need an explicit type sig and forall to use this in the hoist below
mapLocatedForall :: (l -> x) -> (forall z. Located l z -> Located x z)
mapLocatedForall f (Located l z) = Located (f l) z
{-# INLINE mapLocatedForall #-}

locScopeHoistAnno :: (Functor n, Functor f) => (l -> x) -> LocScope l n f a -> LocScope x n f a
locScopeHoistAnno f = scopeWHoistAnno (mapLocatedForall f)
{-# INLINE locScopeHoistAnno #-}

locScopeMapAnno :: (IsAbstractInfo n, Functor f) => (Located l a -> Located l b) -> LocScope l n f a -> LocScope l n f b
locScopeMapAnno = scopeWMapAnno
{-# INLINE locScopeMapAnno #-}
