module Test.Blanks.SimpleScope where

import Blanks (Scope, Tracked, locScopeLocation, pattern ScopeBound, pattern ScopeEmbed, scopeAbstract1,
               trackScopeSimple)
import Data.Set (Set)
import qualified Data.Set as Set

data SimpleFunctor a =
    SimpleFunctorApp !a !a
  | SimpleFunctorLet !a !a
  | SimpleFunctorBase !Char
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data SimpleInfo =
    SimpleInfoLam !Char
  | SimpleInfoLet !Char
  deriving stock (Show)

instance Eq SimpleInfo where
  SimpleInfoLam _ == SimpleInfoLam _ = True
  SimpleInfoLet _ == SimpleInfoLet _ = True
  _ == _ = False

type SimpleScope = Scope SimpleInfo SimpleFunctor Char

lam :: Char -> SimpleScope -> SimpleScope
lam a = scopeAbstract1 (SimpleInfoLam a) a

base :: Char -> SimpleScope
base = ScopeEmbed . SimpleFunctorBase

bound :: Int -> SimpleScope
bound = ScopeBound

var :: Char -> SimpleScope
var = pure

freeVars :: SimpleScope -> Set Char
freeVars = foldMap Set.singleton

tracked :: SimpleScope -> Tracked Char
tracked = locScopeLocation . trackScopeSimple

app :: SimpleScope -> SimpleScope -> SimpleScope
app x y = ScopeEmbed (SimpleFunctorApp x y)

lets :: Char -> SimpleScope -> SimpleScope -> SimpleScope
lets a x y = ScopeEmbed (SimpleFunctorLet x (scopeAbstract1 (SimpleInfoLet a) a y))

svar, sbound, sfree, sfree2, sid, swonky, sconst, sflip, svar2, swonky2, sapp,
  swonky3, sbase, sbase2, slet, sletFree, sletFree2, sletWonky, sletWonky2,
  sletLamBodyId, sletLamBodyConst, sletLamBodyFree, slamLetBodyId, slamLetBodyConst, slamLetBodyFree,
  sletLamArgInner, sletLamArgFree, slamLetArgOuter, slamLetArgFree :: SimpleScope
svar = var 'x'
sbound = bound 0
sfree = lam 'y' (var 'x')
sfree2 = lam 'z' (lam 'y' (var 'x'))
sid = lam 'x' (var 'x')
swonky = lam 'x' (bound 0)
sconst = lam 'x' (lam 'y' (var 'x'))
sflip = lam 'x' (lam 'y' (var 'y'))
svar2 = var 'e'
swonky2 = lam 'x' svar2
sapp = app svar sbound
swonky3 = lam 'x' (bound 3)
sbase = base 'm'
sbase2 = base 'n'
slet = lets 'y' sbase (var 'y')
sletFree = lets 'y' sbase svar
sletFree2 = lets 'y' svar sbase
sletWonky = lets 'y' sbase (bound 0)
sletWonky2 = lets 'y' (bound 0) sbase
sletLamBodyId = lets 'y' sbase (lam 'z' (var 'z'))
sletLamBodyConst = lets 'y' sbase (lam 'z' (var 'y'))
sletLamBodyFree = lets 'y' sbase (lam 'z' (var 'x'))
slamLetBodyId = lam 'y' (lets 'z' sbase (var 'z'))
slamLetBodyConst = lam 'y' (lets 'z' sbase (var 'y'))
slamLetBodyFree = lam 'y' (lets 'z' sbase (var 'x'))
sletLamArgInner = lets 'y' (lam 'z' (var 'z')) sbase
sletLamArgFree = lets 'y' (lam 'z' (var 'x')) sbase
slamLetArgOuter = lam 'y' (lets 'z' (var 'y') sbase)
slamLetArgFree = lam 'y' (lets 'z' (var 'x') sbase)
