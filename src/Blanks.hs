-- | You'll get most of what you want by just importing this module unqualified.
-- See 'Scope' for the basic wrapper and 'LocScope' for a wrapper with annotations you can use
-- for source locations and the like. See the test suite for examples.
module Blanks
  ( module Blanks
  )
where

import Blanks.Conversion as Blanks
import Blanks.Internal.Abstract as Blanks
import Blanks.Internal.Info as Blanks
import Blanks.Internal.Placed as Blanks
import Blanks.LocScope as Blanks
import Blanks.ReExports as Blanks
import Blanks.Scope as Blanks
import Blanks.Transform.Global as Blanks
import Blanks.Transform.Lift as Blanks
import Blanks.Transform.Track as Blanks
import Blanks.Util.Located as Blanks
import Blanks.Util.Name as Blanks
import Blanks.Util.Sub as Blanks
