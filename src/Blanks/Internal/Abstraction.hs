{-# LANGUAGE UndecidableInstances #-}

module Blanks.Internal.Abstraction where

import Control.Applicative (liftA2)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Void (Void)
import Blanks.Internal.Placed (Placed (..))

-- * HasArity

-- | Ad-hoc class that helps us count the arity of an 'Abstraction'.
class HasArity n where
  whatArity :: n -> Int

-- * Abstraction

data AbstractionPlace g =
    AbstractionPlaceInfo !(Place g)
  | AbstractionPlaceBody

deriving stock instance Eq (Place g) => Eq (AbstractionPlace g)
deriving stock instance Ord (Place g) => Ord (AbstractionPlace g)
deriving stock instance Show (Place g) => Show (AbstractionPlace g)

-- | An abstraction is a pair (info, body) where info describes the parts of body we can substitute into.
data Abstraction g e = Abstraction
  { abstractionInfo :: !(g e)
    -- ^ "Info" about the abstraction - might be just arity as in 'SimpleLam', or typed recursive let info as in 'TyLetRec'.
    -- Should have an arity.
  , abstractionBody :: e
    -- ^ Body expression to substitute into
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Placed g => Placed (Abstraction g) where
  type Place (Abstraction g) = AbstractionPlace g
  traversePlaced f (Abstraction info body) = liftA2 Abstraction infoM bodyM where
    infoM = traversePlaced (f . AbstractionPlaceInfo) info
    bodyM = f AbstractionPlaceBody body

instance HasArity (g e) => HasArity (Abstraction g e) where
  whatArity = whatArity . abstractionInfo

-- * Anno

data AnnoPlace g x = AnnoPlace !(Place g) !x

deriving stock instance (Eq (Place g), Eq x) => Eq (AnnoPlace g x)
deriving stock instance (Ord (Place g), Ord x) => Ord (AnnoPlace g x)
deriving stock instance (Show (Place g), Show x) => Show (AnnoPlace g x)

data AnnoInfo g x e = AnnoInfo
  { annoInfoInfo :: !(g e)
  , annoInfoAnno :: !x
  } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Placed g => Placed (AnnoInfo g x) where
  type Place (AnnoInfo g x) = AnnoPlace g x
  traversePlaced f (AnnoInfo info anno) = fmap (`AnnoInfo` anno) (traversePlaced (f . (`AnnoPlace` anno)) info)

instance HasArity (g e) => HasArity (AnnoInfo g x e) where
  whatArity = whatArity . annoInfoInfo

-- * SimpleLam

-- | Info for lambdas without type annotations
newtype SimpleLamInfo e = SimpleLamInfo
  { simpleLamInfoArity :: Int
    -- ^ The arity of the lambda
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Placed SimpleLamInfo where
  type Place SimpleLamInfo = Void
  traversePlaced _ (SimpleLamInfo arity) = pure (SimpleLamInfo arity)

instance HasArity (SimpleLamInfo e) where
  whatArity = simpleLamInfoArity

-- * TyLam

data TyLamPlace =
    TyLamPlaceArg !Int
  | TyLamPlaceRet
  deriving stock (Eq, Ord, Show)

-- | Info for lambdas with type annotations on arguments and a return type
data TyLamInfo e = TyLamInfo
  { tyLamInfoArgs :: !(Seq e)
  , tyLamInfoRet :: e
   } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Placed TyLamInfo where
  type Place TyLamInfo = TyLamPlace
  traversePlaced f (TyLamInfo args ret) = liftA2 TyLamInfo argsM retM where
    argsM = traversePlaced (f . TyLamPlaceArg) args
    retM = f TyLamPlaceRet ret

instance HasArity (TyLamInfo e) where
  whatArity = Seq.length . tyLamInfoArgs

-- * SimpleLetOne

-- | Info for a single (non-recursive) let without type annotation
newtype SimpleLetOneInfo e = SimpleLetOneInfo
  { simpleLetOneInfoArg :: e
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Placed SimpleLetOneInfo where
  type Place SimpleLetOneInfo = ()
  traversePlaced f (SimpleLetOneInfo arg) = fmap SimpleLetOneInfo (f () arg)

instance HasArity (SimpleLetOneInfo e) where
  whatArity = const 1

-- * TyLetOne

data TyLetOnePlace =
    TyLetOnePlaceArg
  | TyLetOnePlaceTy
  deriving stock (Eq, Ord, Show)

-- | Info for a single (non-recursive) let with argument type and body type
data TyLetOneInfo e = TyLetOneInfo
  { tyLetOneInfoArg :: e
  , tyLetOneInfoTy :: e
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Placed TyLetOneInfo where
  type Place TyLetOneInfo = TyLetOnePlace
  traversePlaced f (TyLetOneInfo arg ty) = liftA2 TyLetOneInfo argM tyM where
    argM = f TyLetOnePlaceArg arg
    tyM = f TyLetOnePlaceTy ty

instance HasArity (TyLetOneInfo e) where
  whatArity = const 1

-- * SimpleLetRec

-- | Info for a recursive let with no type annotations
newtype SimpleLetRecInfo e = SimpleLetRecInfo
  { simpleLetRecInfoArgs :: Seq e
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Placed SimpleLetRecInfo where
  type Place SimpleLetRecInfo = Int
  traversePlaced f = fmap SimpleLetRecInfo . traversePlaced f . simpleLetRecInfoArgs

instance HasArity (SimpleLetRecInfo e) where
  whatArity = Seq.length . simpleLetRecInfoArgs

-- * TyLetRec

data TyLetRecPlace =
    TyLetRecPlaceArgExp !Int
  | TyLetRecPlaceArgTy !Int
  | TyLetRecPlaceRet
  deriving stock (Eq, Ord, Show)

data TyLetRecArg e = TyLetRecArg
  { tyLetRecArgExp :: e
  , tyLetRecArgTy :: e
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

-- | Info for a recursive let with argument types and body type
data TyLetRecInfo e = TyLetRecInfo
  { tyLetRecInfoArgs :: !(Seq (TyLetRecArg e))
  , tyLetRecInfoRet :: e
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Placed TyLetRecInfo where
  type Place TyLetRecInfo = TyLetRecPlace
  traversePlaced f (TyLetRecInfo args ret) = liftA2 TyLetRecInfo argsM retM where
    argsM = traversePlaced (\i (TyLetRecArg ex ty) -> liftA2 TyLetRecArg (f (TyLetRecPlaceArgExp i) ex) (f (TyLetRecPlaceArgTy i) ty)) args
    retM = f TyLetRecPlaceRet ret

instance HasArity (TyLetRecInfo e) where
  whatArity = Seq.length . tyLetRecInfoArgs
