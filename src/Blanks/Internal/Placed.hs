module Blanks.Internal.Placed
  ( Placed (..)
  ) where

import Control.Applicative (liftA2)
import Control.Monad.Writer.Strict (execWriter, tell)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product (Product (..))
import Data.Functor.Sum (Sum (..))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import Data.Tree (Tree (..))
import Data.Void (Void)

-- | 'Traversable' but allows you to observe your 'Place' as you go.
-- Similar to https://hackage.haskell.org/package/keys-3.12.3/docs/Data-Key.html#t:TraversableWithKey
class Traversable g => Placed g where
  type Place g :: Type
  traversePlaced :: Applicative m => (Place g -> a -> m b) -> g a -> m (g b)
  gatherPlaced :: g a -> [Place g]
  gatherPlaced = execWriter . traversePlaced (\x _ -> tell [x])

instance Placed [] where
  type Place [] = Int
  traversePlaced f = go 0 where
    go !i ss =
      case ss of
        [] -> pure []
        v : vs -> liftA2 (:) (f i v) (go (i + 1) vs)

instance Placed Seq where
  type Place Seq = Int
  traversePlaced f = go 0 where
    go !i ss =
      case ss of
        Empty -> pure Empty
        v :<| vs -> liftA2 (:<|) (f i v) (go (i + 1) vs)

instance Placed (Map k) where
  type Place (Map k) = k
  traversePlaced = Map.traverseWithKey

instance Placed Tree where
  type Place Tree = Seq Int
  traversePlaced f = go1 Empty where
    go1 !acc (Node a fs) = liftA2 Node (f acc a) (go2 acc 0 fs)
    go2 !acc !i fs =
      case fs of
        [] -> pure []
        v : vs -> liftA2 (:) (go1 (acc :|> i) v) (go2 acc (i + 1) vs)

instance Placed Identity where
  type Place Identity = ()
  traversePlaced f (Identity a) = fmap Identity (f () a)

instance Placed (Const a) where
  type Place (Const a) = Void
  traversePlaced _ (Const a) = pure (Const a)

instance (Placed f, Placed g) => Placed (Compose f g) where
  type Place (Compose f g) = (Place f, Place g)
  traversePlaced f (Compose fga) = fmap Compose (traversePlaced (\pf ga -> traversePlaced (\pg -> f (pf, pg)) ga) fga)

instance (Placed f, Placed g) => Placed (Product f g) where
  type Place (Product f g) = Either (Place f) (Place g)
  traversePlaced f (Pair fa ga) = liftA2 Pair (traversePlaced (f . Left) fa) (traversePlaced (f . Right) ga)

instance (Placed f, Placed g) => Placed (Sum f g) where
  type Place (Sum f g) = Either (Place f) (Place g)
  traversePlaced f s =
    case s of
      InL sl -> fmap InL (traversePlaced (f . Left) sl)
      InR sr -> fmap InR (traversePlaced (f . Right) sr)
