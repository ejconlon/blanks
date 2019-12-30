{-# LANGUAGE UndecidableInstances #-}

module Blanks.PureScope where

import Blanks.Class (Blanks (..))
import Blanks.ScopeT
import Control.Monad.Identity (Identity (..))
import Data.Proxy (Proxy (..))

newtype PureScope n f a = PureScope
  { unPureScope :: ScopeT Identity n f a
  } deriving (Functor, Foldable, Traversable, Applicative, Monad, Blanks n f)

instance (Eq (f (ScopeT Identity n f a)), Eq n, Eq a) => Eq (PureScope n f a) where
  PureScope su == PureScope sv = su == sv

-- TODO show instance

type PureScopeFold n f a r = BlankFold (PureScope n f) a r

pureScopeFold :: Traversable f => PureScopeFold n f a r -> PureScope n f a -> r
pureScopeFold usf = runIdentity . runFold Proxy usf
