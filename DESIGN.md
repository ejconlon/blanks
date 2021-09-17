-- New design:
-- Binder arity info body
-- info must be indexable

-- info -> rec/non-rec
-- Lens from info to (Seq e)

class WithArity i where
  asArity :: Int -> i

class WithArity (Ix n) => WithIndex n where
  type Ix n
  type Res n
  lookupIx :: n -> Ix n -> Maybe (Res n)
  rebuildIx :: (Ix n -> Res n) -> n -> n

