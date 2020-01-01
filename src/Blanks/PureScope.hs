{-# LANGUAGE UndecidableInstances #-}

module Blanks.PureScope
  ( PureScope (..)
  , PureScopeFold
  , pureScopeFold
  ) where

import Blanks.Class (Blanks (..))
import Blanks.ScopeT (ScopeT (..), scopeTFold)
import Blanks.UnderScope (UnderScopeFold (..), underScopeFoldContraMap)
import Control.Monad.Identity (Identity (..))

newtype PureScope n f a = PureScope
  { unPureScope :: ScopeT Identity n f a
  } deriving (Functor, Foldable, Traversable, Applicative, Monad, Blanks n f)

instance (Eq (f (ScopeT Identity n f a)), Eq n, Eq a) => Eq (PureScope n f a) where
  PureScope su == PureScope sv = su == sv

instance (Show (f (ScopeT Identity n f a)), Show n, Show a) => Show (PureScope n f a) where
  showsPrec d (PureScope (ScopeT tu)) = showString "PureScope " . showsPrec (d+1) tu

type PureScopeFold n f a r = UnderScopeFold n f (PureScope n f a) a r

pureScopeFold :: Traversable f => PureScopeFold n f a r -> PureScope n f a -> r
pureScopeFold usf = runIdentity . scopeTFold (underScopeFoldContraMap PureScope usf) . unPureScope
