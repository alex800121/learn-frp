module Bluefin.Algae.MSF where

import Bluefin.Algae.Reader
import Bluefin.Algae.State
import Bluefin.Eff
import Bluefin.Algae
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.MSF (runReaderS, readerS)

type Ball = Int

ballToRight :: (Num c, s :> ss) => Handler (Reader c) s -> MSF (Eff ss) a c
ballToRight lPos = count >>> arrM (\n -> (+ n) <$> ask lPos)

ballToLeft rPos = count >>> arrM (\n -> subtract n <$> ask rPos)

hitRight rPos = arrM (\n -> (n >=) <$> ask rPos)

hitLeft lPos = arrM (\n -> (n <=) <$> ask lPos)

testMSF lPos rPos = ballToRight lPos >>> (arr id &&& hitRight rPos)

-- >>> test
-- [(1,False),(2,False),(3,True),(4,True),(5,True)]

test = runPureEff $ runReader 0 $ \lPos -> runReader 3 $ \rPos -> embed (ballToRight lPos >>> (arr id &&& hitRight rPos)) (replicate 5 ())

runF :: (forall c. (Handler alg e -> Eff (e :& eff) c) -> Eff eff c) -> (Handler alg e -> MSF (Eff (e :& eff)) a b) -> MSF (Eff eff) a b
runF h msf = do
  undefined
