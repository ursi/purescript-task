module Test.Main where

import MasonPrelude hiding (throw)
import MasonPrelude as MP
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
  assertRight "alt (succeed)" (eq 1) $ pure 1 <|> pure 2 <|> pure 3
  assertRight "alt (mixed)" identity $ Task.fail false <|> Task.fail false <|> pure true
  assertLeft "alt (fail)" (eq 3) $ Task.fail 1 <|> Task.fail 2 <|> Task.fail 3
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
  assertRight "ParTask alt (succeed, sync)" (eq 1) $ sequential $ pure 1 <|> pure 2 <|> pure 3
  assertRight "ParTask alt (mixed, sync)" identity $ sequential
    $ parallel (Task.fail false)
    <|> parallel (Task.fail false)
    <|> pure true
  assertLeft "ParTask alt (fail, sync)" (eq 3) $ sequential
    $ parallel (Task.fail 1)
    <|> parallel (Task.fail 2)
    <|> parallel (Task.fail 3)
  assertRight "ParTask alt (succeed, mixed)" (eq 3) $ sequential
    $ parallel (wait 500 *> pure 1)
    <|> parallel (wait 500 *> pure 2)
    <|> pure 3
  assertRight "ParTask alt (mixed, mixed)" identity $ sequential
    $ parallel (wait 500 *> Task.fail false)
    <|> parallel (wait 500 *> Task.fail false)
    <|> pure true
  assertLeft "ParTask alt (fail, mixed)" (eq 1) $ sequential
    $ parallel (wait 500 *> Task.fail 1)
    <|> parallel (wait 300 *> Task.fail 2)
    <|> parallel (Task.fail 3)
  assertRight "ParTask alt (succeed, async)" (eq 3) $ sequential
    $ parallel (wait 500 *> pure 1)
    <|> parallel (wait 400 *> pure 2)
    <|> parallel (wait 300 *> pure 3)
  assertRight "ParTask alt (mixed, async)" identity $ sequential
    $ parallel (wait 500 *> pure true)
    <|> parallel (wait 400 *> Task.fail false)
    <|> parallel (wait 300 *> Task.fail false)
  assertLeft "ParTask alt (fail, async)" (eq 1) $ sequential
    $ parallel (wait 500 *> Task.fail 1)
    <|> parallel (wait 400 *> Task.fail 2)
    <|> parallel (wait 300 *> Task.fail 3)
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
    $ [ wait 700
      , wait 600
      , wait 500 *> Task.fail unit
      ]
  testDelayRight "ParTask alt delay (succeed)" 300 $ sequential
    $ parallel (wait 500 *> pure unit)
    <|> parallel (wait 400 *> pure unit)
    <|> parallel (wait 300 *> pure unit)
  testDelayLeft "ParTask alt delay (fail)" 500 $ sequential
    $ parallel (wait 500 *> Task.fail unit)
    <|> parallel (wait 400 *> Task.fail unit)
    <|> parallel (wait 300 *> Task.fail unit)
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
    "ParTask error callback is called only once (sync)"
    (eq 1)
    $ parSequence
    $ Task.fail
    <$> [ 1, 2, 3 ]
  assertLeft
    "ParTask error callback is called only once (async)"
    (eq 2)
    $ parSequence
    $ [ wait 750 *> Task.fail 1
      , wait 500 *> Task.fail 2
      , wait 750 *> Task.fail 3
      ]
  assertLeft
    "ParTask error callback is called only once (mixed)"
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

throw :: ∀ x. String -> Task x Unit
throw = liftEffect <. MP.throw

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
