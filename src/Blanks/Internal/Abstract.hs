{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Blanks.Internal.Abstract
  ( IsAbstractInfo (..)
  , AbstractPlace (..)
  , Abstract (..)
  , AnnoPlace (..)
  , AnnoInfo (..)
  , ShouldShift (..)
  ) where

import Blanks.Internal.Placed (Placed (..))
import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)

-- * IsAbstractInfo

nProxy :: n a -> Proxy n
nProxy = const Proxy
{-# INLINE nProxy #-}

data ShouldShift =
    ShouldShiftYes
  | ShouldShiftNo
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Ad-hoc class that helps us count the arity of an 'Abstract'.
-- arity is _upper bound_ of gathered args - there may not be expressions attached
class Placed n => IsAbstractInfo n where
  abstractInfoArity :: n a -> Int

  abstractInfoShouldShift :: Proxy n -> Place n -> ShouldShift

  abstractInfoTraverseShouldShift :: Applicative m => (Place n -> ShouldShift -> a -> m b) -> n a -> m (n b)
  abstractInfoTraverseShouldShift f na =
    let proxy = nProxy na
    in traversePlaced (\p a -> f p (abstractInfoShouldShift proxy p) a) na

  abstractInfoMapShouldShift :: (Place n -> ShouldShift -> a -> b) -> n a -> n b
  abstractInfoMapShouldShift f na =
    let proxy = nProxy na
    in mapPlaced (\p a -> f p (abstractInfoShouldShift proxy p) a) na

-- * Abstract

data AbstractPlace n =
    AbstractPlaceInfo !(Place n)
  | AbstractPlaceBody

deriving stock instance Eq (Place n) => Eq (AbstractPlace n)
deriving stock instance Ord (Place n) => Ord (AbstractPlace n)
deriving stock instance Show (Place n) => Show (AbstractPlace n)
deriving stock instance Generic (AbstractPlace n)
deriving anyclass instance NFData (Place n) => NFData (AbstractPlace n)

-- | An abstraction is a pair (info, body) where info describes the parts of body we can substitute into.
data Abstract n e = Abstract
  { abstractInfo :: !(n e)
    -- ^ "Info" about the abstraction - might be just arity as in 'SimpleLam', or typed recursive let info as in 'TyLetRec'.
    -- Should have an arity.
  , abstractBody :: e
    -- ^ Body expression to substitute into
  } deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (NFData)

instance Placed n => Placed (Abstract n) where
  type Place (Abstract n) = AbstractPlace n
  traversePlaced f (Abstract info body) = liftA2 Abstract infoM bodyM where
    infoM = traversePlaced (f . AbstractPlaceInfo) info
    bodyM = f AbstractPlaceBody body

abstractProxy :: Proxy (Abstract n) -> Proxy n
abstractProxy Proxy = Proxy
{-# INLINE abstractProxy #-}

instance IsAbstractInfo n => IsAbstractInfo (Abstract n) where
  abstractInfoArity = abstractInfoArity . abstractInfo

  abstractInfoShouldShift proxy place =
    case place of
      AbstractPlaceInfo nplace -> abstractInfoShouldShift (abstractProxy proxy) nplace
      AbstractPlaceBody -> ShouldShiftYes

-- * Anno

data AnnoPlace n x = AnnoPlace
  { annoPlacePlace :: !(Place n)
  , annoPlaceAnno :: !x
  }

deriving stock instance (Eq (Place n), Eq x) => Eq (AnnoPlace n x)
deriving stock instance (Ord (Place n), Ord x) => Ord (AnnoPlace n x)
deriving stock instance (Show (Place n), Show x) => Show (AnnoPlace n x)
deriving stock instance Generic (AnnoPlace n x)
deriving anyclass instance (NFData (Place n), NFData x) => NFData (AnnoPlace n x)

data AnnoInfo n x e = AnnoInfo
  { annoInfoInfo :: !(n e)
  , annoInfoAnno :: !x
  } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (NFData)

instance Placed n => Placed (AnnoInfo n x) where
  type Place (AnnoInfo n x) = AnnoPlace n x
  traversePlaced f (AnnoInfo info anno) = fmap (`AnnoInfo` anno) (traversePlaced (f . (`AnnoPlace` anno)) info)

annoProxy :: Proxy (AnnoInfo n x) -> Proxy n
annoProxy Proxy = Proxy
{-# INLINE annoProxy #-}

instance IsAbstractInfo n => IsAbstractInfo (AnnoInfo n x) where
  abstractInfoArity = abstractInfoArity . annoInfoInfo

  abstractInfoShouldShift p = abstractInfoShouldShift (annoProxy p) . annoPlacePlace
