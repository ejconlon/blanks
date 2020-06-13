{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Blanks.LocScope
  ( LocScope (..)
  , LocScopeRawFold
  , LocScopeFold
  , pattern LocScopeBound
  , pattern LocScopeFree
  , pattern LocScopeBinder
  , pattern LocScopeEmbed
  ) where

import Blanks.Class (Blank (..), BlankDomain, BlankEmbedded, BlankFold, BlankFunctor, BlankInfo, BlankRawFold)
-- import Blanks.Internal (BlankInternal)
import Blanks.Located (Located (..))
import Blanks.NatTrans (RealNatIso)
import Blanks.ScopeW (ScopeW (..))
import Blanks.UnderScope (pattern UnderScopeBinder, pattern UnderScopeBound, pattern UnderScopeEmbed,
                          pattern UnderScopeFree)
import Data.Coerce (coerce)

newtype LocScope l n f a = LocScope
  { unLocScope :: ScopeW (Located l) n f (LocScope l n f) a
  } deriving (Functor, Foldable, Traversable)

instance RealNatIso (ScopeW (Located l) n f (LocScope l n f)) (LocScope l n f)

-- instance Functor f => Blank (LocScope l n f) where
--   blankFree = fmap LocScope . blankFree
--   blankAbstract = undefined
--   blankUnAbstract = undefined
--   blankInstantiate = undefined
--   blankApply = undefined
--   blankBind = undefined
--   blankBindOpt = undefined
--   blankEmbed = undefined
--   blankRawFold = undefined
--   blankFold = undefined
--   blankLiftAnno = undefined

pattern LocScopeBound :: l -> Int -> LocScope l n f a
pattern LocScopeBound l b = LocScope (ScopeW (Located l (UnderScopeBound b)))

pattern LocScopeFree :: l -> a -> LocScope l n f a
pattern LocScopeFree l a = LocScope (ScopeW (Located l (UnderScopeFree a)))

pattern LocScopeBinder :: l -> Int -> n -> LocScope l n f a -> LocScope l n f a
pattern LocScopeBinder l i n e = LocScope (ScopeW (Located l (UnderScopeBinder i n e)))

pattern LocScopeEmbed :: l -> f (LocScope l n f a) -> LocScope l n f a
pattern LocScopeEmbed l fe = LocScope (ScopeW (Located l (UnderScopeEmbed fe)))

{-# COMPLETE LocScopeBound, LocScopeFree, LocScopeBinder, LocScopeEmbed #-}

type instance BlankDomain (LocScope l n f) = Located l
-- type instance BlankCodomain (LocScope l n f) = Colocated l
type instance BlankInfo (LocScope l n f) = n
type instance BlankFunctor (LocScope l n f) = f
type instance BlankEmbedded (LocScope l n f) = LocScope l n f

instance (Eq (f (LocScope l n f a)), Eq l, Eq n, Eq a) => Eq (LocScope l n f a) where
  LocScope su == LocScope sv = su == sv

instance (Show (f (LocScope l n f a)), Show l, Show n, Show a) => Show (LocScope l n f a) where
  showsPrec d (LocScope (ScopeW tu)) = showString "LocScope " . showsPrec (d+1) tu

type LocScopeRawFold l n f a r = BlankRawFold (LocScope l n f) a r
type LocScopeFold l n f a r = BlankFold (LocScope l n f) a r
