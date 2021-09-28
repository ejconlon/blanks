{-# LANGUAGE DeriveAnyClass #-}

module Test.Blanks.Lib.SimpleScope where

import Blanks (Abstract (..), IsAbstractInfo (..), IsPlacedAbstractInfo (..), Located (..), Placed (..), Scope, ShouldShift (..), Tracked, locScopeLocation,
               pattern ScopeAbstract, pattern ScopeBound, pattern ScopeEmbed, scopeBindFree1, trackScopeSimple)
import Control.DeepSeq (NFData)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)

data SimpleFunctor a =
    SimpleFunctorApp !a !a
  | SimpleFunctorLet !a !a
  | SimpleFunctorBase !Char
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

data SimpleInfo e =
    SimpleInfoLam !(Seq Char)
  | SimpleInfoLet !Char
  deriving stock (Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

instance Eq (SimpleInfo e) where
  SimpleInfoLam s1 == SimpleInfoLam s2 = Seq.length s1 == Seq.length s2
  SimpleInfoLet _ == SimpleInfoLet _ = True
  _ == _ = False

instance IsAbstractInfo SimpleInfo where
  abstractInfoArity s =
    case s of
      SimpleInfoLam cs -> Seq.length cs
      SimpleInfoLet _ -> 1

data SimpleInfoPlace =
    SimpleInfoPlaceLam
  | SimpleInfoPlaceLet
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance Placed SimpleInfo where
  type Place SimpleInfo = SimpleInfoPlace
  traversePlaced _ si =
    case si of
      SimpleInfoLam x -> pure (SimpleInfoLam x)
      SimpleInfoLet x -> pure (SimpleInfoLet x)

instance IsPlacedAbstractInfo SimpleInfo where
  abstractInfoShouldShift _ _ = ShouldShiftNo

type SimpleScope = Scope SimpleInfo SimpleFunctor Char

lam :: Char -> SimpleScope -> SimpleScope
lam a e = ScopeAbstract (Abstract (SimpleInfoLam (Seq.singleton a)) (scopeBindFree1 a e))

base :: Char -> SimpleScope
base = ScopeEmbed . SimpleFunctorBase

bound :: Int -> SimpleScope
bound = ScopeBound

var :: Char -> SimpleScope
var = pure

freeVars :: SimpleScope -> Set Char
freeVars = foldMap Set.singleton

tracked :: SimpleScope -> Tracked Char
tracked = locatedLoc . locScopeLocation . trackScopeSimple

app :: SimpleScope -> SimpleScope -> SimpleScope
app x y = ScopeEmbed (SimpleFunctorApp x y)

lets :: Char -> SimpleScope -> SimpleScope -> SimpleScope
lets a x y = ScopeEmbed (SimpleFunctorLet x (ScopeAbstract (Abstract (SimpleInfoLet a) (scopeBindFree1 a y))))

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
