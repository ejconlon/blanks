-- | You'll get most of what you want by just importing this module unqualified.
-- See the 'Blanks' class definition and related methods to manipulate variables and abstractions.
-- See 'Scope' for the basic wrapper and 'LocScope' for a wrapper with annotations you can use
-- for source locations and the like. See the test suite for examples.
module Blanks
  ( module Blanks
  ) where

import Blanks.Conversion as Blanks
import Blanks.Located as Blanks
import Blanks.LocScope as Blanks
import Blanks.Name as Blanks
import Blanks.Scope as Blanks
import Blanks.Sub as Blanks
import Blanks.Tracked as Blanks
