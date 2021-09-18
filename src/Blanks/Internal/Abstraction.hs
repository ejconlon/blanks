{-# LANGUAGE UndecidableInstances #-}

module Blanks.Internal.Abstraction where

import Control.Applicative (liftA2)
-- import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Void (Void)
import Blanks.Internal.Placed (Placed (..))

-- * HasArity

-- | Ad-hoc class that helps us count the arity of an 'Abstraction'.
class HasArity n where
  whatArity :: n -> Int

-- class Placed g => HasFnArgs g where
--   arityFnArgs :: g a -> Int
--   filterFnArgs :: Proxy g -> Place g -> Bool
--   itraverseFnArgs :: Applicative m => Proxy g -> (Place g -> a -> m a) -> g a -> m (g a)
--   itraverseFnArgs p f = traversePlaced (\x a -> if filterArg p x then f x a else pure a)
--   gatherFnArgs :: g a -> [Place g]
--   gatherFnArgs = filter filterFnArgs Proxy . gatherPlaced

-- * Abstraction

data AbstractionPlace g =
    AbstractionPlaceInfo (Place g)
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

-- * SimpleLam

-- | Info for lambdas without type annotations
newtype SimpleLam e = SimpleLam
  { simpleLamArity :: Int
    -- ^ The arity of the lambda
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Placed SimpleLam where
  type Place SimpleLam = Void
  traversePlaced _ (SimpleLam arity) = pure (SimpleLam arity)

instance HasArity (SimpleLam e) where
  whatArity = simpleLamArity

-- * TyLam

data TyLamPlace =
    TyLamPlaceArg !Int
  | TyLamPlaceRet
  deriving stock (Eq, Ord, Show)

-- | Info for lambdas with type annotations on arguments and a return type
data TyLam e = TyLam
  { tyLamArgs :: !(Seq e)
  , tyLamRet :: e
   } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Placed TyLam where
  type Place TyLam = TyLamPlace
  traversePlaced f (TyLam args ret) = liftA2 TyLam argsM retM where
    argsM = traversePlaced (f . TyLamPlaceArg) args
    retM = f TyLamPlaceRet ret

instance HasArity (TyLam e) where
  whatArity = Seq.length . tyLamArgs

-- * SimpleLetOne

-- | Info for a single (non-recursive) let without type annotation
newtype SimpleLetOne e = SimpleLetOne
  { simpleLetOneArg :: e
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Placed SimpleLetOne where
  type Place SimpleLetOne = ()
  traversePlaced f (SimpleLetOne arg) = fmap SimpleLetOne (f () arg)

instance HasArity (SimpleLetOne e) where
  whatArity = const 1

-- * TyLetOne

data TyLetOnePlace =
    TyLetOnePlaceArg
  | TyLetOnePlaceTy
  deriving stock (Eq, Ord, Show)

-- | Info for a single (non-recursive) let with argument type and body type
data TyLetOne e = TyLetOne
  { tyLetOneArg :: e
  , tyLetOneTy :: e
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Placed TyLetOne where
  type Place TyLetOne = TyLetOnePlace
  traversePlaced f (TyLetOne arg ty) = liftA2 TyLetOne argM tyM where
    argM = f TyLetOnePlaceArg arg
    tyM = f TyLetOnePlaceTy ty

instance HasArity (TyLetOne e) where
  whatArity = const 1

-- * SimpleLetRec

-- | Info for a recursive let with no type annotations
newtype SimpleLetRec e = SimpleLetRec
  { simpleLetRecArgs :: Seq e
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Placed SimpleLetRec where
  type Place SimpleLetRec = Int
  traversePlaced f = fmap SimpleLetRec . traversePlaced f . simpleLetRecArgs

instance HasArity (SimpleLetRec e) where
  whatArity = Seq.length . simpleLetRecArgs

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
data TyLetRec e = TyLetRec
  { tyLetRecArgs :: !(Seq (TyLetRecArg e))
  , tyLetRecRet :: e
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Placed TyLetRec where
  type Place TyLetRec = TyLetRecPlace
  traversePlaced = undefined

instance HasArity (TyLetRec e) where
  whatArity = Seq.length . tyLetRecArgs
