
module MonadRandom where

import Control.Applicative
import Control.Monad.State
import qualified System.Random as R
import Test.QuickCheck
import Test.QuickCheck.Gen

class (Functor m, Applicative m, Monad m) => MonadRandom m where
  randomR :: R.Random a => (a, a) -> m a
  random  :: R.Random a => m a

instance MonadRandom IO where
  randomR = R.randomRIO
  random  = R.randomIO

instance (Functor m, Applicative m, Monad m, R.RandomGen g) => MonadRandom (StateT g m) where
  randomR r = do
    (x, g) <- gets $ R.randomR r
    liftM (const x) $ put g
  random = do
    (x, g) <- gets R.random
    liftM (const x) $ put g

instance MonadRandom Gen where
  randomR = choose
  random  = MkGen $ \g _ -> fst $ R.random g

