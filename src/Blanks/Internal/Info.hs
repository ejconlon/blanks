{-# LANGUAGE DeriveAnyClass #-}

module Blanks.Internal.Info
  ( SimpleLamInfo (..)
  , TyLamPlace (..)
  , TyLamInfo (..)
  , SimpleLetOneInfo (..)
  , TyLetOnePlace (..)
  , TyLetOneInfo (..)
  , SimpleLetRecInfo (..)
  , TyLetRecPlace (..)
  , TyLetRecArg (..)
  , TyLetRecInfo (..)
  )
where

import Blanks.Internal.Abstract (IsAbstractInfo (..), ShouldShift (..))
import Blanks.Internal.Placed (Placed (..))
import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Void (Void)
import GHC.Generics (Generic)

-- * SimpleLam

-- | Info for lambdas without type annotations
newtype SimpleLamInfo e = SimpleLamInfo
  { simpleLamInfoArity :: Int
  -- ^ The arity of the lambda
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

instance Placed SimpleLamInfo where
  type Place SimpleLamInfo = Void
  traversePlaced _ (SimpleLamInfo arity) = pure (SimpleLamInfo arity)

instance IsAbstractInfo SimpleLamInfo where
  abstractInfoArity = simpleLamInfoArity
  abstractInfoShouldShift _ _ = ShouldShiftNo

-- * TyLam

data TyLamPlace
  = TyLamPlaceArg !Int
  | TyLamPlaceRet
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Info for lambdas with type annotations on arguments and a return type
data TyLamInfo e = TyLamInfo
  { tyLamInfoArgs :: !(Seq e)
  , tyLamInfoRet :: e
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

instance Placed TyLamInfo where
  type Place TyLamInfo = TyLamPlace
  traversePlaced f (TyLamInfo args ret) = liftA2 TyLamInfo argsM retM
   where
    argsM = traversePlaced (f . TyLamPlaceArg) args
    retM = f TyLamPlaceRet ret

instance IsAbstractInfo TyLamInfo where
  abstractInfoArity = Seq.length . tyLamInfoArgs
  abstractInfoShouldShift _ _ = ShouldShiftNo

-- * SimpleLetOne

-- | Info for a single (non-recursive) let without type annotation
newtype SimpleLetOneInfo e = SimpleLetOneInfo
  { simpleLetOneInfoArg :: e
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

instance Placed SimpleLetOneInfo where
  type Place SimpleLetOneInfo = ()
  traversePlaced f (SimpleLetOneInfo arg) = fmap SimpleLetOneInfo (f () arg)

instance IsAbstractInfo SimpleLetOneInfo where
  abstractInfoArity _ = 1
  abstractInfoShouldShift _ _ = ShouldShiftYes

-- * TyLetOne

data TyLetOnePlace
  = TyLetOnePlaceArg
  | TyLetOnePlaceTy
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Info for a single (non-recursive) let with argument type and body type
data TyLetOneInfo e = TyLetOneInfo
  { tyLetOneInfoArg :: e
  , tyLetOneInfoTy :: e
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

instance Placed TyLetOneInfo where
  type Place TyLetOneInfo = TyLetOnePlace
  traversePlaced f (TyLetOneInfo arg ty) = liftA2 TyLetOneInfo argM tyM
   where
    argM = f TyLetOnePlaceArg arg
    tyM = f TyLetOnePlaceTy ty

instance IsAbstractInfo TyLetOneInfo where
  abstractInfoArity _ = 1
  abstractInfoShouldShift _ pl =
    case pl of
      TyLetOnePlaceArg -> ShouldShiftYes
      TyLetOnePlaceTy -> ShouldShiftYes

-- * SimpleLetRec

-- | Info for a recursive let with no type annotations
newtype SimpleLetRecInfo e = SimpleLetRecInfo
  { simpleLetRecInfoArgs :: Seq e
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

instance Placed SimpleLetRecInfo where
  type Place SimpleLetRecInfo = Int
  traversePlaced f = fmap SimpleLetRecInfo . traversePlaced f . simpleLetRecInfoArgs

instance IsAbstractInfo SimpleLetRecInfo where
  abstractInfoArity = Seq.length . simpleLetRecInfoArgs
  abstractInfoShouldShift _ _ = ShouldShiftYes

-- * TyLetRec

data TyLetRecPlace
  = TyLetRecPlaceArgExp !Int
  | TyLetRecPlaceArgTy !Int
  | TyLetRecPlaceRet
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data TyLetRecArg e = TyLetRecArg
  { tyLetRecArgExp :: e
  , tyLetRecArgTy :: e
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

-- | Info for a recursive let with argument types and body type
data TyLetRecInfo e = TyLetRecInfo
  { tyLetRecInfoArgs :: !(Seq (TyLetRecArg e))
  , tyLetRecInfoRet :: e
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

instance Placed TyLetRecInfo where
  type Place TyLetRecInfo = TyLetRecPlace
  traversePlaced f (TyLetRecInfo args ret) = liftA2 TyLetRecInfo argsM retM
   where
    argsM = traversePlaced (\i (TyLetRecArg ex ty) -> liftA2 TyLetRecArg (f (TyLetRecPlaceArgExp i) ex) (f (TyLetRecPlaceArgTy i) ty)) args
    retM = f TyLetRecPlaceRet ret

instance IsAbstractInfo TyLetRecInfo where
  abstractInfoArity = Seq.length . tyLetRecInfoArgs
  abstractInfoShouldShift _ pl =
    case pl of
      TyLetRecPlaceArgExp _ -> ShouldShiftYes
      _ -> ShouldShiftNo
