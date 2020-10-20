{-# LANGUAGE UndecidableInstances #-}

module Blanks.LocScope
  ( LocScope (..)
  , pattern LocScopeBound
  , pattern LocScopeFree
  , pattern LocScopeBinder
  , pattern LocScopeEmbed
  , locScopeLocation
  , locScopeHoistAnno
  ) where

import Blanks.Interface (Blank, BlankFunctor, BlankInfo, BlankLeft, BlankRight, blankBind, blankHoistAnno, blankMapAnno)
import Blanks.Located (Colocated, Located (..), askColocated)
import Blanks.NatNewtype (NatNewtype)
import Blanks.ScopeW (ScopeW (..))
import Blanks.UnderScope (pattern UnderScopeBinder, pattern UnderScopeBound, pattern UnderScopeEmbed,
                          pattern UnderScopeFree)
import Control.DeepSeq (NFData (..))
import Control.Monad (ap)
import Control.Monad.Writer (MonadWriter (..))

-- | A 'Scope' annotated with some information between constructors.
-- See 'Blank' for usage, and see the patterns in this module for easy manipulation
-- and inspection.
newtype LocScope l n f a = LocScope
  { unLocScope :: ScopeW (Located l) n f (LocScope l n f) a
  } deriving stock (Functor, Foldable, Traversable)

type instance BlankLeft (LocScope l n f) = Located l
type instance BlankRight (LocScope l n f) = Colocated l
type instance BlankInfo (LocScope l n f) = n
type instance BlankFunctor (LocScope l n f) = f

instance Functor f => Blank (LocScope l n f)
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
  s >>= f = blankBind go s where
    go a = fmap (\l1 -> let LocScope (ScopeW (Located l2 b)) = f a in LocScope (ScopeW (Located (l1 <> l2) b))) askColocated

instance (Monoid l, Functor f) => MonadWriter l (LocScope l n f) where
  writer (a, l) = LocScopeFree l a
  tell l = LocScopeFree l ()
  listen = blankMapAnno (\(Located l a) -> Located l (a, l))
  pass = blankMapAnno (\(Located l (a, f)) -> Located (f l) a)

instance (Eq (f (LocScope l n f a)), Eq l, Eq n, Eq a) => Eq (LocScope l n f a) where
  LocScope su == LocScope sv = su == sv

instance (Show (f (LocScope l n f a)), Show l, Show n, Show a) => Show (LocScope l n f a) where
  showsPrec d (LocScope (ScopeW tu)) = showString "LocScope " . showsPrec (d+1) tu

-- Need an explicit type sig and forall to use this in the hoist below
mapLocatedForall :: (l -> x) -> (forall z. Located l z -> Located x z)
mapLocatedForall f (Located l z) = Located (f l) z

-- | Map over locations
locScopeHoistAnno :: Functor f => (l -> x) -> LocScope l n f a -> LocScope x n f a
locScopeHoistAnno f = blankHoistAnno (mapLocatedForall f)
