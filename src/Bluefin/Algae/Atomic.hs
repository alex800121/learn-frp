module Bluefin.Algae.Atomic where

import Bluefin.Algae
import Bluefin.Eff
import Bluefin.IO
import Control.Concurrent.STM

data Atomic :: AEffect where
  Atomically :: STM a -> Atomic a

atomic :: (z :> zz) => Handler Atomic z -> STM a -> Eff zz a
atomic h stm = call h (Atomically stm)

atomicToIO :: forall a e zz. (e :> zz) => IOE e -> (forall z. Handler Atomic z -> Eff (z :& zz) a) -> Eff zz a
atomicToIO ioe = handle f
  where
    f (Atomically stm) g = effIO ioe (atomically stm) >>= g
