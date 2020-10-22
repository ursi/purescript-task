module Test.Main where

import MasonPrelude
import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import Control.Parallel (parallel, sequential)
import Data.Bifunctor (lmap, rmap)
import Effect.Timer (clearTimeout, setTimeout)
import Task (Promise, Task)
import Task as Task
import Test.Assert (assert)

main :: Effect Unit
main = do
  assertRight "pure" identity $ pure true
  assertLeft "fail" identity $ Task.fail true
  assertRight "map" (eq 3) $ add 1 <$> pure 2
  assertRight "apply" (eq 3) $ lift2 add (pure 1) (pure 2)
  assertRight "bind" (eq 3) do
    one <- pure 1
    two <- pure 2
    pure $ one + two
  assertRight "append" (eq "ab") $ pure "a" <> pure "b"
  assertRight "mempty" (eq "ab") $ pure "a" <> mempty <> pure "b"
  assertLeft "lmap" (eq 3) $ lmap (add 1) $ Task.fail 2
  assertRight "rmap" (eq 3) $ rmap (add 1) $ pure 2
  assertRight "liftEffect" identity $ liftEffect $ pure true
  assertRight "bindError" (eq 3) $ Task.bindError (Task.fail 1) $ pure <. add 2
  assertRight "promise resolve" identity $ Task.fromPromise resolve
  assertLeft "promise reject" identity $ Task.fromPromise reject
  assertRight "promise setTimeout" (eq unit) $ wait 500
  assertLeft "promise setTimeout reject" (eq unit) $ waitReject 500
  assertRight "parallel instance" identity $ sequential $ parallel $ pure true
  assertRight "ParTask map" (eq 3) $ sequential $ add 1 <$> pure 2
  assertRight "ParTask apply" (eq 3) $ sequential $ lift2 add (pure 1) (pure 2)
  testDelayRight "delay" 500 $ wait 500
  testDelayLeft "delay with error" 500
    $ wait 500
    *> Task.fail unit
  testDelayRight "apply delay" 750
    $ sequence_
    $ wait
    <$> [ 250, 250, 250 ]
  testDelayRight "parallel delay" 500 $ parSequence
    $ wait
    <$> [ 500, 500, 500 ]
  testDelayLeft "parallel delay with failure" 500
    $ parSequence
    $ [ wait 750
      , wait 500 *> Task.fail unit
      ]
  assertLeft
    "error callback is called only once (non-parallel, sync)"
    (eq 1)
    $ sequence
    $ Task.fail
    <$> [ 1, 2, 3 ]
  assertLeft
    "error callback is called only once (non-parallel, async)"
    (eq 1)
    $ sequence
    $ ((*>) (wait 500) <. Task.fail)
    <$> [ 1, 2, 3 ]
  assertLeft
    "error callback is called only once (parallel, sync)"
    (eq 1)
    $ parSequence
    $ Task.fail
    <$> [ 1, 2, 3 ]
  assertLeft
    "error callback is called only once (parallel, async)"
    (eq 2)
    $ parSequence
    $ [ wait 750 *> Task.fail 1
      , wait 500 *> Task.fail 2
      , wait 750 *> Task.fail 3
      ]
  assertLeft
    "error callback is called only once (parallel, mixed)"
    (eq 2)
    $ parSequence
    $ [ wait 500 *> Task.fail 1
      , Task.fail 2
      , Task.fail 3
      ]
  ( let
      desc = "cancel with sync fail"
    in
      assertLeft desc identity
        $ parSequence
            [ cancelableWait 500 *> throw desc
            , cancelableWait 500 *> throw desc
            , Task.fail true
            ]
  )
  ( let
      desc = "cancel with async fail"
    in
      assertLeft desc identity
        $ parSequence
            [ cancelableWait 750 *> throw desc
            , cancelableWait 750 *> throw desc
            , wait 500 *> Task.fail true
            ]
  )

foreign import resolve :: ∀ x. Effect (Promise x Boolean)

foreign import reject :: ∀ a. Effect (Promise Boolean a)

foreign import waitImpl :: ∀ x. Int -> Effect (Promise x Unit)

wait :: ∀ x. Int -> Task x Unit
wait = Task.fromPromise <. waitImpl

foreign import waitRejectImpl :: ∀ a. Int -> Effect (Promise Unit a)

waitReject :: ∀ a. Int -> Task Unit a
waitReject = Task.fromPromise <. waitRejectImpl

foreign import now :: Effect Int

foreign import throwImpl :: String -> Effect Unit

throw :: ∀ x. String -> Task x Unit
throw = liftEffect <. throwImpl

status :: Boolean -> String -> Effect Unit
status bool str =
  if bool then
    log $ green "✓ " <> str
  else
    log $ red "✗ " <> str

green :: String -> String
green = withGraphics $ foreground Green

red :: String -> String
red = withGraphics $ foreground Red

assertRight :: ∀ x a. Show a => String -> (a -> Boolean) -> Task x a -> Effect Unit
assertRight desc tester =
  Task.capture
    ( case _ of
        Right a -> do
          -- logShow $ Debug.taggedLog "debug" a
          let
            b = tester a
          status b desc
          assert b
        Left _ -> do
          status false desc
          assert false
    )

assertLeft :: ∀ x a. String -> (x -> Boolean) -> Task x a -> Effect Unit
assertLeft desc tester =
  Task.capture
    ( case _ of
        Right _ -> do
          status false desc
          assert false
        Left x -> do
          let
            b = tester x
          status b desc
          assert b
    )

testDelayRight :: ∀ x a. String -> Int -> Task x a -> Effect Unit
testDelayRight desc ms task = do
  t <- now
  Task.capture
    ( case _ of
        Right a -> do
          t' <- now
          let
            b = between ms (ms + 10) $ t' - t
          status b desc
          assert $ b
        Left _ -> do
          status false desc
          assert false
    )
    task

testDelayLeft :: ∀ x a. String -> Int -> Task x a -> Effect Unit
testDelayLeft desc ms task = do
  t <- now
  Task.capture
    ( case _ of
        Right _ -> do
          status false desc
          assert false
        Left x -> do
          t' <- now
          let
            b = between ms (ms + 10) $ t' - t
          status b desc
          assert $ b
    )
    task

cancelableWait :: ∀ x. Int -> Task x Unit
cancelableWait ms =
  Task.makeTask \aC xC -> do
    id <- setTimeout ms $ aC unit
    pure $ clearTimeout id
