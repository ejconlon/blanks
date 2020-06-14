module Blanks.Sub
  ( SubError (..)
  ) where

import Control.Exception (Exception)

data SubError
  = ApplyError !Int !Int
  | UnboundError !Int
  | NonBinderError
  deriving (Eq, Show)

instance Exception SubError
