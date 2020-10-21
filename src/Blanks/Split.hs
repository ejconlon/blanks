{-# LANGUAGE DeriveAnyClass #-}

module Blanks.Split where

import Blanks.Core (BinderScope (..))
import Blanks.LocScope (LocScope, pattern LocScopeBinder, pattern LocScopeBound, pattern LocScopeEmbed,
                        pattern LocScopeFree)
import Control.DeepSeq (NFData)
import Control.Monad.State (State, gets, modify')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

data Stream x = Stream
  { streamHead :: !x
  , streamTail :: Stream x
  }

enumStreamFrom :: Enum e => e -> Stream e
enumStreamFrom e = Stream e (enumStreamFrom (succ e))

enumStream :: Enum e => Stream e
enumStream = enumStreamFrom (toEnum 0)

newtype BinderId = BinderId { unBinderId :: Int }
  deriving stock (Eq, Show, Ord)
  deriving newtype (Enum, NFData)

data SplitVar a =
    SplitVarFree !a
  | SplitVarBinder !BinderId
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData)

type LocBinder l n f a = BinderScope n (LocScope l n f (SplitVar a))

data SplitState l n f a = SplitState
  { splitStateStream :: !(Stream BinderId)
  , splitStateBinders :: !(Map BinderId (LocBinder l n f a))
  }

initSplitState :: SplitState l n f a
initSplitState = SplitState enumStream Map.empty

getNextBinderId :: State (SplitState l n f a) BinderId
getNextBinderId = do
  st <- gets splitStateStream
  let Stream hd tl = st
  modify' (\ss -> ss { splitStateStream = tl })
  pure hd

insertBinder :: BinderId -> LocBinder l n f a -> State (SplitState l n f a) ()
insertBinder bid lb = modify' (\ss -> ss { splitStateBinders = Map.insert bid lb (splitStateBinders ss) })

splitLocScope :: Traversable f => LocScope l n f a -> State (SplitState l n f a) (LocScope l n f (SplitVar a))
splitLocScope ls =
  case ls of
    LocScopeBound l b -> pure (LocScopeBound l b)
    LocScopeFree l a -> pure (LocScopeFree l (SplitVarFree a))
    LocScopeBinder l r n e -> do
      bid <- getNextBinderId
      e' <- splitLocScope e
      let lb = BinderScope r n e'
      insertBinder bid lb
      pure (LocScopeFree l (SplitVarBinder bid))
    LocScopeEmbed l fe -> fmap (LocScopeEmbed l) (traverse splitLocScope fe)
