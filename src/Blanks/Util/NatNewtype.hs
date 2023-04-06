{-# LANGUAGE QuantifiedConstraints #-}

module Blanks.Util.NatNewtype
  ( NatNewtype
  , natNewtypeFrom
  , natNewtypeTo
  )
where

import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)

-- | A "natural isomorphism" between two functors, like exists
-- derivably between newtyped functors and their wrapped types.
-- The functional dependency requires that 'g' be the newtype
-- and 'm' the wrapped type.
-- This is used in library internals to coerce recursive parts of datatypes back and forth across newtypes.
class
  ( forall a. Coercible (m a) (g a)
  , forall a. Coercible (g a) (m a)
  ) =>
  NatNewtype (m :: Type -> Type) (g :: Type -> Type)
    | g -> m

-- | Coerce from the wrapped type to the newtype.
natNewtypeTo :: NatNewtype m g => m a -> g a
natNewtypeTo = coerce
{-# INLINE natNewtypeTo #-}

-- | Coerce from the newtype to the wrapped type.
natNewtypeFrom :: NatNewtype m g => g a -> m a
natNewtypeFrom = coerce
{-# INLINE natNewtypeFrom #-}
