-- | Slimmest useful interface
module Uniter
  ( Alignable (..)
  , UnalignableErr (..)
  , ProcessErr (..)
  , MonadUniter (..)
  , Unitable (..)
  , uniteTerm
  , MonadReuniter (..)
  , Reunitable (..)
  , reuniteTerm
  , module Uniter.Core
  , module Uniter.Unitable.Driver
  ) where

import Uniter.Align (Alignable (..), UnalignableErr (..))
import Uniter.Core
import Uniter.Process (ProcessErr (..))
import Uniter.Reunitable.Class (MonadReuniter (..), Reunitable (..), reuniteTerm)
import Uniter.Unitable.Class (MonadUniter (..), Unitable (..), uniteTerm)
import Uniter.Unitable.Driver
