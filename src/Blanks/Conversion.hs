module Blanks.Conversion
  ( locScopeForget
  , scopeAnno
  ) where

import Blanks.LocScope (LocScope, pattern LocScopeBinder, pattern LocScopeBound, pattern LocScopeEmbed,
                        pattern LocScopeFree)
import Blanks.Scope (Scope, pattern ScopeBinder, pattern ScopeBound, pattern ScopeEmbed, pattern ScopeFree)

-- | Forget all the annotations and yield a plain 'Scope'.
locScopeForget :: Functor f => LocScope l n f a -> Scope n f a
locScopeForget ls =
  case ls of
    LocScopeBound _ b -> ScopeBound b
    LocScopeFree _ a -> ScopeFree a
    LocScopeBinder _ r x e -> ScopeBinder r x (locScopeForget e)
    LocScopeEmbed _ fe -> ScopeEmbed (fmap locScopeForget fe)

-- | Annotate every location in the 'Scope' with a given value as a 'LocScope'.
scopeAnno :: Functor f => l -> Scope n f a -> LocScope l n f a
scopeAnno l = go where
  go s =
    case s of
      ScopeBound b -> LocScopeBound l b
      ScopeFree a -> LocScopeFree l a
      ScopeBinder r x e -> LocScopeBinder l r x (go e)
      ScopeEmbed fe -> LocScopeEmbed l (fmap go fe)
