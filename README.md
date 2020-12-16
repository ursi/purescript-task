This is a library for performing asynchronous effects, heavily inspired by Elm's [Task](https://package.elm-lang.org/packages/elm/core/latest/Task). In terms of functionality, it is a superset of the Elm implementation, with the following additions:

- Tasks can be evaluated in parallel
- Users can create their own tasks (this is technically *possible* in Elm via a [hack](https://github.com/ursi/jim))
- An [Apply](https://pursuit.purescript.org/packages/purescript-prelude/docs/Control.Apply#t:Apply) instance*
- An [Alt](https://pursuit.purescript.org/packages/purescript-control/docs/Control.Alt#t:Alt) instance (both non-parallel* and parallel)
- A [Monoid](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Monoid#t:Monoid) instance*

## Example

Here is a contrived example where we use [js-timers](https://pursuit.purescript.org/packages/purescript-js-timers) and [node-fs](https://pursuit.purescript.org/packages/purescript-node-fs) to make `wait` and `readFile` tasks, then use them together to make a program that waits 1 second then logs itself to the console.

```purescript
module Main where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Effect.Timer (setTimeout, clearTimeout)
import Node.Buffer (toString)
import Node.Encoding (Encoding(..))
import Node.FS.Async as Fs
import Task (Task, makeTask)
import Task as Task

main :: Effect Unit
main =
  Task.run do
    wait 1000
    log =<< readFile "src/Main.purs"

wait :: ∀ x. Int -> Task x Unit
wait ms =
  makeTask \aC _ -> do
    id <- setTimeout ms $ aC unit
    pure $ clearTimeout id

readFile :: String -> Task Error String
readFile path =
  makeTask \aC xC -> do
    Fs.readTextFile UTF8 path case _ of
      Right str -> aC str
      Left error -> xC error
    -- There is no canceler for this task
    pure $ pure unit

```

<hr>

\* something similar can be done in Elm, but it would be more clunky and it isn't provided out of the box
