module Test.Blanks.SimpleScope where

import Blanks (NameOnly, pattern NameOnly, Scope, pattern ScopeBound, pattern ScopeEmbed, Tracked, locScopeLocation,
               scopeAbstract1, trackScopeSimple)
import Data.Set (Set)
import qualified Data.Set as Set

data SimpleFunctor a = SimpleFunctor !a !a
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

type SimpleScope = Scope (NameOnly Char) SimpleFunctor Char

abst :: Char -> SimpleScope -> SimpleScope
abst a = scopeAbstract1 (NameOnly a) a

bound :: Int -> SimpleScope
bound = ScopeBound

var :: Char -> SimpleScope
var = pure

freeVars :: SimpleScope -> Set Char
freeVars = foldMap Set.singleton

tracked :: SimpleScope -> Tracked Char
tracked = locScopeLocation . trackScopeSimple

embed :: SimpleScope -> SimpleScope -> SimpleScope
embed x y = ScopeEmbed (SimpleFunctor x y)

svar, sbound, sfree, sfree2, sid, swonky, sconst, sflip, svar2, swonky2, spair, swonky3 :: SimpleScope
svar = var 'x'
sbound = bound 0
sfree = abst 'y' (var 'x')
sfree2 = abst 'z' (abst 'y' (var 'x'))
sid = abst 'x' (var 'x')
swonky = abst 'x' (bound 0)
sconst = abst 'x' (abst 'y' (var 'x'))
sflip = abst 'x' (abst 'y' (var 'y'))
svar2 = var 'e'
swonky2 = abst 'x' svar2
spair = embed svar sbound
swonky3 = abst 'x' (bound 3)
