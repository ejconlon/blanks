{-# LANGUAGE UndecidableInstances #-}

module Blanks.Internal.Info where

import Control.Applicative (liftA2)
-- import Data.Kind (Type)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Void (Void)
import Blanks.Internal.Placed (Placed (..))

-- * HasArity

class HasArity n where
  whatArity :: n -> Int

-- instance (HasArity f, HasArity g) => HasArity (Sum f g) where
--   whatArity s =
--     case s of
--       InL sl -> whatArity sl
--       InR sr -> whatArity sr

-- * Binder

data BinderPlace g =
    BinderPlaceInfo (Place g)
  | BinderPlaceBody

deriving stock instance Eq (Place g) => Eq (BinderPlace g)
deriving stock instance Show (Place g) => Show (BinderPlace g)

data Binder g e = Binder
  { binderInfo :: !(g e)
  , binderBody :: e
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Placed g => Placed (Binder g) where
  type Place (Binder g) = BinderPlace g
  traversePlaced = undefined

-- * SimpleLam

newtype SimpleLam e = SimpleLam
  { simpleLamArity :: Int
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance HasArity (SimpleLam e) where
  whatArity = simpleLamArity

instance Placed SimpleLam where
  type Place SimpleLam = Void
  traversePlaced _ (SimpleLam arity) = pure (SimpleLam arity)

-- * TyLam

data TyLamArg x e = TyLamArg
  { tyLamAnno :: !x
  , tyLamTy :: e
   } deriving stock (Eq, Show, Functor, Foldable, Traversable)

data TyLamPlace x =
    TyLamPlaceArg !x !Int
  | TyLamPlaceRet
  deriving stock (Eq, Show, Ord)

data TyLam x e = TyLam
  { tyLamArgs :: !(Seq (TyLamArg x e))
  , tyLamRet :: e
   } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance HasArity (TyLam x e) where
  whatArity = Seq.length . tyLamArgs

instance Placed (TyLam x) where
  type Place (TyLam x) = TyLamPlace x
  traversePlaced f (TyLam args ret) = liftA2 TyLam argsM retM where
    argsM = traversePlaced (\i (TyLamArg x v) -> fmap (TyLamArg x) (f (TyLamPlaceArg x i) v)) args
    retM = f TyLamPlaceRet ret

-- * SimpleLetOne

newtype SimpleLetOne e = SimpleLetOne
  { simpleLetOneArg :: e
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Placed SimpleLetOne where
  type Place SimpleLetOne = ()
  traversePlaced f (SimpleLetOne arg) = fmap SimpleLetOne (f () arg)

-- * TyLetOne

data TyLetOne x e = TyLetOne
  { tyLetOneAnno :: !x
  , tyLetOneArg :: e
  , tyLetOneTy :: e
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Placed (TyLetOne x) where
  type Place (TyLetOne x) = ()
  traversePlaced f (TyLetOne anno arg ty) = undefined

-- * SimpleLetRec

newtype SimpleLetRec e = SimpleLetRec
  { simpleLetRecArgs :: Seq e
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Placed SimpleLetRec where
  type Place SimpleLetRec = ()
  traversePlaced = undefined

-- * TyLetRec

data TyLetRecArg x e = TyLetRecArg
  { tyLetRecArgAnno :: !x
  , tyLetRecArgExp :: e
  , tyLetRecArgTy :: e
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

data TyLetRec x e = TyLetRec
  { tyLetRecArgs :: !(Seq (TyLetRecArg x e))
  , tyLetRecRet :: e
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Placed (TyLetRec x) where
  type Place (TyLetRec x) = ()
  traversePlaced = undefined

-- * Junk

-- import Data.Functor.Const (Const (..))
-- import Data.Maybe (isJust, catMaybes)
-- import Data.Foldable (for_, toList)
-- import Data.Functor (($>))
-- import Data.Distributive (Distributive (..))
-- import Data.Functor.Rep (Representable (..))
-- import Data.Proxy (Proxy (..))
-- import Data.Vector (Vector)
-- import qualified Data.Vector as V

-- class Ord i => HasArgPos i where
--   toArgPos :: i -> Maybe Int
--   fromArgPos :: Int -> i
--   uptoArgCount :: Int -> [i]
--   uptoArgCount i = [fromArgPos x | x <- [0 .. i - 1]]

-- instance HasArgPos Int where
--   toArgPos = Just
--   fromArgPos = id

-- class (Functor f, HasArgPos (Key f)) => Informed f where
--   type Key f :: Type
--   type Value f :: Type -> Type

--   informedBodyKey :: Proxy f -> Key f

--   informedArgKeys :: f a -> [Key f]

--   informedTraverse :: Applicative m => (Key f -> Value f a -> m (Value f b)) -> f a -> m (f b)

--   informedArgArity :: f a -> Key f -> Int

--   informedBodyArity :: f a -> Int
--   informedBodyArity fa = informedKeyArity (informedBodyKey fa Proxy)

-- class (Functor f, Traversable (Places f)) => Informed f where
--   type Key f :: Type
--   type Places f :: Type -> Type

--   informedBodyKey :: Proxy f -> Key f

--   informedArgCount :: f a -> Int

--   informedArgKey :: f a -> Int -> Maybe (Key f)

--   informedTraverse :: Applicative m => (Key f -> Places f a -> m (Places f b)) -> f a -> m (f b)

--   informedKeyArity :: f a -> Key f -> Int

--   informedArgKeys :: f a -> [Key f]
--   informedArgKeys fa =
--     let c = informedArgCount fa
--     in catMaybes [informedArgKey fa i | i <- [0 .. c - 1]]

--   informedArgArity :: f a -> Int -> Maybe Int
--   informedArgArity fa = fmap (informedKeyArity fa)  . informedArgKey fa

--   informedBodyArity :: f a -> Int
--   informedBodyArity fa = informedKeyArity fa (informedBodyKey (Proxy :: Proxy f))

-- class IsInfo n where
--   type Key n
--   type Value n
--   infoArity :: n -> Int
--   infoKeyArity :: Key n -> Int

-- class (Representable f, IsPosition (Key f)) => Informed f where
--   type Key f
--   informedArity :: f a -> Int
--   informedKeyArity :: Proxy f -> Key f -> Int
--   informedFromKey :: Proxy f -> Key f -> Rep f
--   informedToKey :: Proxy f -> Rep f -> Maybe (Key f)

-- data BarePlace e = BarePlace
--   { barePlaceArity :: !Int
--   , barePlaceBody :: e
--   } deriving stock (Eq, Show, Functor, Foldable, Traversable)

-- instance Representable BarePlace where
--   type Rep BarePlace = Int
--   -- tabulate :: (Rep f -> a) -> f a
--   tabulate f =

-- data BinderScope n e =
--   BinderScope
--     { binderScopeInfo :: n
--     , binderScopeBody :: e
--     }
--   deriving stock (Eq, Show, Functor)

--   infoKeys :: n -> [Key n]
--   infoKeys n = toList (getConst (infoTraverse go n)) where
--     go k _ = Const (Seq.singleton k)

-- class IsInfo n => IsInfoAdvanced n where
--   infoValid :: n -> Key n -> Bool
--   infoValid n k = isJust (infoLookup n k)
--   infoPositionValues :: n -> [Value n]
--   infoLookup :: n -> Key n -> Maybe (Value n)
--   infoReplace :: Key n -> Value n -> n -> Maybe n
--   infoAdjust :: (Value n -> Value n) -> Key n -> n -> Maybe n
--   infoAdjust f k n = infoLookup n k >>= \v -> infoReplace k (f v) n
--   infoTabulate :: (Key n -> Value n) -> n -> n

-- infoPositions :: IsInfo n => n -> [Key n]
-- infoPositions = uptoPosition . infoArity

-- infoForPositions :: (Applicative m, IsInfo n) => (Key n -> m ()) -> n -> m n
-- infoForPositions f n = for_ (infoPositions n) f $> n

-- newtype LambdaBareInfo = LambdaBareInfo { unLambdaBareInfo :: Int } deriving newtype (Eq, Show)

-- instance IsInfo LambdaBareInfo where
--   type Key LambdaBareInfo = Int
--   type Value LambdaBareInfo = ()

--   infoArity = unLambdaBareInfo
--   infoKeys = infoPositions
--   infoTraverse f = infoForPositions (`f` ())

--   -- infoValid (ArityInfo i) j = j >= 0 && j < i
--   -- infoPositionArity (ArityInfo i) = i
--   -- infoPositionValues (ArityInfo i) = replicate i ()
--   -- infoLookup n k = if infoValid n k then Just () else Nothing
--   -- infoReplace k _ n = if infoValid n k then Just n else Nothing
--   -- infoAdjust _ k n = if infoValid n k then Just n else Nothing
--   -- infoTabulate _ = id

-- data LambdaAnnoInfo n = LambdaAnnoInfo
--   { lambdaAnnoInfoArity :: !Int
--   , lambdaAnnoInfoAnno :: !n
--    } deriving stock (Eq, Show)

-- instance IsInfo (LambdaAnnoInfo n) where
--   type Key (LambdaAnnoInfo n) = Int
--   type Value (LambdaAnnoInfo n) = ()

--   infoArity = lambdaAnnoInfoArity
--   infoKeys = infoPositions
--   infoTraverse f = infoForPositions (`f` ())

--   -- infoValid (LambdaInfo i _) j = j >= 0 && j < i
--   -- infoPositionArity (ArityInfo i) = i
--   -- infoPositionValues (ArityInfo i) = replicate i ()
--   -- infoLookup n k = if infoValid n k then Just () else Nothing
--   -- infoReplace k _ n = if infoValid n k then Just n else Nothing
--   -- infoAdjust _ k n = if infoValid n k then Just n else Nothing
--   -- infoTabulate _ = id

-- data LambdaTyInfo n e = LambdaTyInfo
--   { lambdaTyInfoArgTys :: !(Seq e)
--   , lambdaTyInfoRetTy :: !e
--   }

-- data LambdaTyKey =
--     LambdaTyKeyArgPos !Int
--   | LambdaTyKeyRet
--   deriving stock (Eq, Show, Ord)

-- instance IsPosition LambdaTyKey where
--   fromPosition k =
--     case k of
--       LambdaTyKeyArgPos i -> Just i
--       _ -> Nothing
--   toPosition = LambdaTyKeyArgPos

-- seqApp :: Applicative m => (Int -> k) -> (k -> v -> m w) -> Seq v -> m (Seq w)
-- seqApp onPos onPair = sequenceA . go 0 Empty where
--   go !i !acc ss =
--     case ss of
--       Empty -> acc
--       v :<| vs ->
--         let j = i + 1
--             w = onPair (onPos i) v
--         in go j (acc :|> w) vs

-- seqAppPos :: (IsPosition k, Applicative m) => (k -> v -> m w) -> Seq v -> m (Seq w)
-- seqAppPos = seqApp toPosition

-- instance IsInfo (LambdaTyInfo n e) where
--   type Key (LambdaTyInfo n e) = LambdaTyKey
--   type Value (LambdaTyInfo n e) = e

--   infoArity = Seq.length . lambdaTyInfoArgTys
--   infoKeys n = LambdaTyKeyRet : infoPositions n
--   infoTraverse f (LambdaTyInfo atys rty) = LambdaTyInfo <$> atysM <*> rtyM where
--     atysM = seqAppPos f atys
--     rtyM = f LambdaTyKeyRet rty
