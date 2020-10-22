-- | You'll get most of what you want by just importing this module unqualified.
-- See 'Scope' for the basic wrapper and 'LocScope' for a wrapper with annotations you can use
-- for source locations and the like. See the test suite for examples.
module Blanks
  ( module Blanks
  , BinderScope (..)
  ) where

import Blanks.Conversion as Blanks
import Blanks.Core (BinderScope (..))
import Blanks.Located as Blanks
import Blanks.LocScope as Blanks
import Blanks.Name as Blanks
import Blanks.Scope as Blanks
import Blanks.Split as Blanks
import Blanks.Sub as Blanks
