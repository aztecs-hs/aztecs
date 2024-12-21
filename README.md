# Aztecs

An ECS for Haskell

```hs
import Control.Monad.IO.Class
import Data.Aztecs
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.Task as T

-- Components

data X = X Int deriving (Show)

instance Component X

data Y = Y Int deriving (Show)

instance Component Y

data A = A (Query (Write X))

-- Systems

instance System IO A where
  access = A <$> query Q.write
  run (A q) = do
    liftIO $ print "A"

    T.command $ do
      e <- T.spawn (X 0)
      T.insert e (Y 1)

    xs <- T.all q
    liftIO $ print xs

    T.alter xs (\(X x) -> X $ x + 1)

data XY = XY X Y deriving (Show)

data B = B (Query XY)

instance System IO B where
  access = B <$> query (XY <$> Q.read <*> Q.read)
  run (B q) = do
    liftIO $ print "B"

    xys <- T.all q
    liftIO $ print xys

app :: Scheduler IO
app = schedule @Startup @_ @A [] <> schedule @Update @_ @B []

main :: IO ()
main = runScheduler app
```
