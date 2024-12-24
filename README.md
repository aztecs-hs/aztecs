# Aztecs

A type-safe and friendly [ECS](https://en.wikipedia.org/wiki/Entity_component_system) for Haskell.

## Features

 - Automatic parallelism: Systems run in parallel if possible, and in multiple stages
 - Type-safe DSL: Components and systems are accessed by marker types that determine their storage
 - Modular design: Aztecs can be extended for a variety of use cases

```hs
import Control.Monad.IO.Class
import Data.Aztecs
import qualified Data.Aztecs.Command as C
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.System as S

-- Components

data X = X Int deriving (Show)

instance Component X

data Y = Y Int deriving (Show)

instance Component Y

-- Systems

data A

instance System IO A where
  access = S.command $ do
    e <- C.spawn (X 0)
    C.insert e (Y 1)

data XY = XY X Y deriving (Show)

data B

instance System IO B where
  access = do
    -- Increment every X component
    S.alter (Q.read @X) (\_ (X x) -> X $ x + 1)

    -- Query for all entities with an X and Y component
    xys <- (S.all (XY <$> Q.read <*> Q.read))
    liftIO $ print xys

app :: Scheduler IO
app = schedule @Startup @_ @A [] <> schedule @Update @_ @B []

main :: IO ()
main = runScheduler app
```

## Inspiration
Aztecs' approach to type-safety is inspired by [Bevy](https://github.com/bevyengine/bevy/)
