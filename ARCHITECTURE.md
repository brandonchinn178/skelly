# Architecture

This document provides an overview of how the code is organized.

## Entrypoint

`Main.hs` is the entrypoint and parses CLI options for all of the Skelly commands we support. At a high level, `Main.hs` only imports from `Skelly.CLI.*`.

`Skelly.CLI.Command` defines the base `Command` type that contains all the information to render, parse, and execute a Skelly command. Then each `Skelly.CLI.CommandFoo` module exports a `commandFoo` value, which has type `Command`.

## Services pattern

A lot of modules follow the Services pattern, which consists of the following:

* A `Service` data type
* An `initService` function
* Functions that take a `Service` and other args, and returns an `IO` action

`Service`, `initService`, and optionally the other functions are intended to be imported qualified:

```hs
import Skelly.Core.Foo qualified as Foo
import Skelly.Core.Logging (LogLevel (..), logDebug)
import Skelly.Core.Logging qualified as Logging

data Service = Service
  { loggingService :: Logging.Service
  , fooService :: Foo.Service
  }

initService :: LogLevel -> Service
initService logLevel = Service{..}
  where
    loggingService = Logging.initService logLevel
    fooService = Foo.initService loggingService

doThing :: Service -> IO ()
doThing Service{..} = do
  logDebug loggingService "Hello world"
  Foo.run fooService
```

This pattern is helpful for unit testing, where `doThing` can mock out all functionality defined in `Service`.
