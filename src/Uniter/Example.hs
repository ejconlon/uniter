module Uniter.Example
  ( main
  )
where

import qualified Uniter.Example.Complex
import qualified Uniter.Example.Simple

main :: IO ()
main = do
  -- Uniter.Example.Simple.main
  Uniter.Example.Complex.main
