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

type family RightAdjunct (t :: * -> *) :: * -> *

type instance RightAdjunct Identity = Identity

type RightAdjunction (t :: * -> *) = Adjunction t (RightAdjunct t)

type RightAdjunctionApplicative t = (RightAdjunction t, Applicative (RightAdjunct t))
