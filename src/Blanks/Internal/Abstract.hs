{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Blanks.Internal.Abstract
  ( IsAbstractInfo (..)
  , defaultAbstractInfoGatherArgs
  , AbstractPlace (..)
  , Abstract (..)
  , abstractArity
  , AnnoPlace (..)
  , AnnoInfo (..)
  ) where

import Blanks.Internal.Placed (Placed (..))
import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)

-- * IsAbstractInfo

-- | Ad-hoc class that helps us count the arity of an 'Abstract'.
-- arity is _upper bound_ of gathered args - there may not be expressions attached
class Placed n => IsAbstractInfo n where
  abstractInfoArity :: n a -> Int
  abstractInfoFilterArg :: Proxy n -> Place n -> Bool
  abstractInfoGatherArgs :: n a -> [Place n]
  abstractInfoGatherArgs = defaultAbstractInfoGatherArgs
  {-# INLINE abstractInfoGatherArgs #-}

nProxy :: n a -> Proxy n
nProxy _ = Proxy
{-# INLINE nProxy #-}

defaultAbstractInfoGatherArgs :: IsAbstractInfo n => n a -> [Place n]
defaultAbstractInfoGatherArgs na = filter (abstractInfoFilterArg (nProxy na)) (gatherPlaced na)
{-# INLINE defaultAbstractInfoGatherArgs #-}

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

abstractArity :: IsAbstractInfo n => Abstract n e -> Int
abstractArity = abstractInfoArity . abstractInfo

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
  abstractInfoFilterArg p = abstractInfoFilterArg (annoProxy p) . annoPlacePlace
