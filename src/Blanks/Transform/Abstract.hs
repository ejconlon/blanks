{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Blanks.Transform.Abstract where

import Blanks.Internal.Abstract (Abstract (..), IsAbstractInfo (..))
import Blanks.LocScope (LocScope, locScopeBindFree, pattern LocScopeAbstract, pattern LocScopeBound,
                        pattern LocScopeEmbed, pattern LocScopeFree)
import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Data.Sequence (Seq)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

newtype Fix j = Fix { unFix :: j (Fix j) } deriving stock (Generic)

deriving stock instance Eq (j (Fix j)) => Eq (Fix j)
deriving stock instance Show (j (Fix j)) => Show (Fix j)
deriving anyclass instance NFData (j (Fix j)) => NFData (Fix j)

data LocFix l j = LocFix
  { locFixLoc :: !l
  , locFixRec :: !(j (LocFix l j))
  } deriving stock (Generic)

deriving stock instance (Eq l, Eq (f (LocFix l f))) => Eq (LocFix l f)
deriving stock instance (Show l, Show (f (LocFix l f))) => Show (LocFix l f)
deriving anyclass instance (NFData l, NFData (f (LocFix l f))) => NFData (LocFix l f)

data AbstractSelection n f a x =
    AbstractSelectionFree !a
  | AbstractSelectionAbstract !(n x) !(Seq a) !x
  | AbstractSelectionEmbed !(f x)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

type AbstractSelector j n f a = forall x. j x -> AbstractSelection n f a x

type UnAbstractSelector j n f a = forall x. AbstractSelection n f a x -> j x

abstractLocFix :: (IsAbstractInfo n, Functor f, Eq a) => AbstractSelector j n f a -> LocFix l j -> LocScope l n f a
abstractLocFix sel = go where
  go (LocFix l j) =
    case sel j of
      AbstractSelectionFree a -> LocScopeFree l a
      AbstractSelectionAbstract n as x ->
        let n' = fmap go n
            s = go x
            s' = locScopeBindFree as s
        in LocScopeAbstract l (Abstract n' s')
      AbstractSelectionEmbed f -> LocScopeEmbed l (fmap go f)

data UnboundError l = UnboundError
  { unboundErrorLoc :: !l
  , unboundErrorIndex :: !Int
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

instance (Typeable l, Show l) => Exception (UnboundError l)

-- | Invariant: abstractInfoNames yields Seq of same length as arity
class IsAbstractInfo n => IsNamedAbstractInfo a n | n -> a where
  abstractInfoNames :: n x -> Seq a

unAbstractLocFix ::
  (IsNamedAbstractInfo a n, Traversable f)
  => UnAbstractSelector j n f a -> LocScope l n f a -> Either (UnboundError l) (LocFix l j)
unAbstractLocFix unSel = go where
  go ls =
    case ls of
      LocScopeBound l b -> Left (UnboundError l b)
      LocScopeFree l a -> Right (LocFix l (unSel (AbstractSelectionFree a)))
      LocScopeAbstract l (Abstract n s) -> do
        let names = abstractInfoNames n
        n' <- traverse go n
        s' <- go s
        Right (LocFix l (unSel (AbstractSelectionAbstract n' names s')))
      LocScopeEmbed l fa -> traverse go fa >>= Right . LocFix l . unSel . AbstractSelectionEmbed
