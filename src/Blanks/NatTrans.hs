{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Blanks.NatTrans
  ( IsNatIso (..)
  , IsNatTrans (..)
  , NatTrans (..)
  , idNatTrans
  , composeNatTrans
  , RealNatIso
  , realNatIsoFrom
  , realNatIsoTo
  ) where

import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)

newtype NatTrans (f :: Type -> Type) (g :: Type -> Type) = NatTrans { unNatTrans :: forall a. f a -> g a }

idNatTrans :: NatTrans f f
idNatTrans = NatTrans id

composeNatTrans :: NatTrans g f -> NatTrans h g -> NatTrans h f
composeNatTrans (NatTrans gf) (NatTrans hg) = NatTrans (gf . hg)

class IsNatTrans (f :: Type -> Type) (g :: Type -> Type) where
  natTrans :: NatTrans f g

instance IsNatTrans f f where
  natTrans = idNatTrans

class IsNatIso (f :: Type -> Type) (g :: Type -> Type) where
  natIsoTo :: NatTrans f g
  natIsoFrom :: NatTrans g f

instance IsNatIso f f where
  natIsoTo = idNatTrans
  natIsoFrom = idNatTrans

class (forall a. Coercible (f a) (g a), forall a. Coercible (g a) (f a)) => RealNatIso (f :: Type -> Type) (g :: Type -> Type)

realNatIsoTo :: RealNatIso f g => f a -> g a
realNatIsoTo = coerce
{-# INLINE realNatIsoTo #-}

realNatIsoFrom :: RealNatIso f g => g a -> f a
realNatIsoFrom = coerce
{-# INLINE realNatIsoFrom #-}
