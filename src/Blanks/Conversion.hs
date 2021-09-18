-- | Functions to convert between annotated and un-annotated scopes.
module Blanks.Conversion
  ( locScopeForget
  , scopeAnno
  , asLocScope
  ) where

import Blanks.LocScope (LocScope, pattern LocScopeAbstract, pattern LocScopeBound, pattern LocScopeEmbed,
                        pattern LocScopeFree)
import Blanks.Scope (Scope, pattern ScopeAbstract, pattern ScopeBound, pattern ScopeEmbed, pattern ScopeFree)

-- | Forget all the annotations and yield a plain 'Scope'.
locScopeForget :: (Functor n, Functor f) => LocScope l n f a -> Scope n f a
locScopeForget ls =
  case ls of
    LocScopeBound _ b -> ScopeBound b
    LocScopeFree _ a -> ScopeFree a
    LocScopeAbstract _ ab -> ScopeAbstract (fmap locScopeForget ab)
    LocScopeEmbed _ fe -> ScopeEmbed (fmap locScopeForget fe)

-- | Annotate every location in the 'Scope' with a given value as a 'LocScope'.
scopeAnno :: (Functor n, Functor f) => l -> Scope n f a -> LocScope l n f a
scopeAnno l = go where
  go s =
    case s of
      ScopeBound b -> LocScopeBound l b
      ScopeFree a -> LocScopeFree l a
      ScopeAbstract ab -> LocScopeAbstract l (fmap go ab)
      ScopeEmbed fe -> LocScopeEmbed l (fmap go fe)

-- | Apply the function as if the 'Scope' had trivial annotations. Not very efficient, but easy.
asLocScope :: (Functor n1, Functor n2, Functor f1, Functor f2) => (LocScope () n1 f1 a1 -> LocScope () n2 f2 a2) -> (Scope n1 f1 a1 -> Scope n2 f2 a2)
asLocScope f = locScopeForget . f . scopeAnno ()
