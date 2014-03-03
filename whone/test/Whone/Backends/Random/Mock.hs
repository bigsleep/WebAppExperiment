module Whone.Backends.Random.Mock
( run
) where

import Whone.Random (IRandom(..))
import System.Random (mkStdGen)
import qualified Control.Monad.Random as R (MonadRandom, getRandom, getRandomR)

run :: (MonadRandom m) => Int -> IRandom (m a) -> m a

run (GetRandom f) = return . f $ R.getRandom

run (GetRandomR r f) = return . f $ R.getRandomR r
