module Blanks.Internal
  ( BlankInternal (..)
  , blankShift
  , defaultBlankBind
  , defaultBlankBindOpt
  , defaultBlankInstantiate
  ) where

import Blanks.Class (Blank, BlankCodomain)
import Data.Kind (Type)
import Data.Sequence (Seq)

defaultBlankInstantiate :: BlankInternal m => Seq (BlankCodomain m (m a)) -> m a -> m a
defaultBlankInstantiate = blankInstantiateN 0

defaultBlankBind :: BlankInternal m => (a -> BlankCodomain m (m b)) -> m a -> m b
defaultBlankBind f = blankBindN f 0

defaultBlankBindOpt :: BlankInternal m => (a -> Maybe (BlankCodomain m (m a))) -> m a -> m a
defaultBlankBindOpt f = blankBindOptN f 0

class Blank m => BlankInternal (m :: Type -> Type) where
  blankShiftN :: Int -> Int -> m a -> m a

  blankBindN :: (a -> BlankCodomain m (m b)) -> Int -> m a -> m b

  blankBindOptN :: (a -> Maybe (BlankCodomain m (m a))) -> Int -> m a -> m a

  blankInstantiateN :: Int -> Seq (BlankCodomain m (m a)) -> m a -> m a

blankShift :: BlankInternal m => Int -> m a -> m a
blankShift = blankShiftN 0
