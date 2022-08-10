-- | Slimmest useful interface
module Uniter
  ( Alignable (..)
  , UnalignableErr (..)
  , BoundId (..)
  , ProcessErr (..)
  , MonadUniter (..)
  , Unitable (..)
  , uniteTerm
  , MonadReuniter (..)
  , Reunitable (..)
  , reuniteTerm
  , module Uniter.Unitable.Driver
  ) where

import Uniter.Align (Alignable (..), UnalignableErr (..))
import Uniter.Core (BoundId (..))
import Uniter.Process (ProcessErr (..))
import Uniter.Reunitable.Class (MonadReuniter (..), Reunitable (..), reuniteTerm)
import Uniter.Unitable.Class (MonadUniter (..), Unitable (..), uniteTerm)
import Uniter.Unitable.Driver
