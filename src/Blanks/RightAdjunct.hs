{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Blanks.RightAdjunct
  ( RightAdjunct
  , RightAdjunction
  , RightAdjunctionApplicative
  ) where

import Control.Monad.Identity (Identity)
import Data.Functor.Adjunction (Adjunction (..))
import Data.Kind (Type)

type family RightAdjunct (t :: Type -> Type) :: Type -> Type

type instance RightAdjunct Identity = Identity

type RightAdjunction (t :: Type -> Type) = Adjunction t (RightAdjunct t)

type RightAdjunctionApplicative t = (RightAdjunction t, Applicative (RightAdjunct t))
