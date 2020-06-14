{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Blanks.NatNewtype
  ( NatNewtype
  , natNewtypeFrom
  , natNewtypeTo
  ) where

import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)

class (forall a. Coercible (m a) (g a), forall a. Coercible (g a) (m a)) => NatNewtype (m :: Type -> Type) (g :: Type -> Type) | g -> m

natNewtypeTo :: NatNewtype m g => m a -> g a
natNewtypeTo = coerce
{-# INLINE natNewtypeTo #-}

natNewtypeFrom :: NatNewtype m g => g a -> m a
natNewtypeFrom = coerce
{-# INLINE natNewtypeFrom #-}
