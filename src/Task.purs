module Task
  ( Task
  , run
  , Callback
  , capture
  , Canceler
  , makeTask
  , bindError
  , ForeignCallback
  , fromForeign
  , Promise
  , fromPromise
  , ParTask
  , module Exports
  ) where

import MasonPrelude
import Control.Parallel (class Parallel)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Data.Bifunctor (class Bifunctor)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Control.Monad.Error.Class (throwError) as Exports
import Data.Bifunctor (lmap) as Exports

type Callback a
  = a -> Effect Unit

-- | This type alias is used to show the `Effect Unit` should be used to cancel a task. See `makeTask` for an example.
type Canceler
  = Effect Unit

-- | A `Task x a` represents an asynchronous computation that can either succeed with a value of type `a`, or fail with a value of type `x`.
-- |
-- | If you're coming from Elm and it seems like a lot of the functions are missing, that's because their equivalents are hiding in the type class instances. They have been annotated below.
newtype Task x a
  = Task (Callback a -> Callback x -> Ref Canceler -> Effect Unit)

-- this is used instead of a Newtype instance to keep Task opaque
unwrap :: ∀ x a. Task x a -> (Callback a -> Callback x -> Ref Canceler -> Effect Unit)
unwrap (Task t) = t

-- | `Task.map` -> `map`
instance functorTask :: Functor (Task x) where
  map f (Task t) = Task $ t <. (.>) f

-- | `Task.mapN` -> `liftN`
instance applyTask :: Apply (Task x) where
  apply (Task tf) (Task ta) = Task \bC xC ref -> tf (\f -> ta (bC <. f) xC ref) xC ref

-- | `Task.succeed` -> `pure`
-- |
-- | `Task.sequence` -> For this, we use the `sequence` function that is a member of the [Traversable](https://pursuit.purescript.org/packages/purescript-foldable-traversable/docs/Data.Traversable#t:Traversable) type class. Note: we are not using a `Traversable` instance of `Task`, we're using the `Traversable` instance of whatever we want to sequence over. So while Elm's `Task.sequence` only works over `List`s, we can sequence over anything that has a `Traversable` instance.
instance applicativeTask :: Applicative (Task x) where
  pure a = Task \aC _ _ -> aC a

-- | `Task.andThen` -> `bind` (the only difference is the order of the arguments)
instance bindTask :: Bind (Task x) where
  bind (Task ta) f = Task \bC xC ref -> ta (\a -> unwrap (f a) bC xC ref) xC ref

instance monadTask :: Monad (Task x)

instance altTask :: Alt (Task x) where
  alt = bindError <~. const

instance semigroupTask :: Semigroup a => Semigroup (Task x a) where
  append = lift2 append

instance monoidTask :: Monoid a => Monoid (Task x a) where
  mempty = pure mempty

-- | `Task.mapError`: `lmap`, which isn't a member of `Bifunctor` directly, but uses the `Bifunctor` instance.
instance bifunctorTask :: Bifunctor Task where
  bimap lmap rmap task = task <#> rmap # mapError lmap

instance monadEffectTask :: MonadEffect (Task x) where
  liftEffect aEff = Task \aC _ _ -> aEff >>= aC

-- | `Task.fail` -> `throwError`
instance monadThrowTask :: MonadThrow x (Task x) where
  throwError x = Task \_ xC _ -> xC x

instance monadErrorTask :: MonadError x (Task x) where
  catchError task f = bindError task f

-- | ParTask is the applicative that lets you run tasks in parallel via the [parallel](https://pursuit.purescript.org/packages/purescript-parallel) library.
-- |
-- | You may not need to work with any `ParTask` values directly. One of the most useful functions [parSequence](https://pursuit.purescript.org/packages/purescript-parallel/docs/Control.Parallel#v:parSequence), which can take an array of `Task x a` and executes them all in parallel, returning a `Task x (Array a)`.
-- |
-- | If tasks are executing in parallel via `ParTask`'s `Apply` instance (as is the case with `parSequence`), when one of them fails, the cancelers are called for all the task that are currently running.
newtype ParTask x a
  = ParTask (Callback a -> Callback x -> Ref Canceler -> Effect Unit)

instance functorParTask :: Functor (ParTask x) where
  map f (ParTask t) = ParTask $ t <. (.>) f

instance applyParTask :: Apply (ParTask x) where
  apply (ParTask tf) (ParTask ta) =
    ParTask \bC xC ref -> do
      fRef <- Ref.new Nothing
      fErrorRef <- Ref.new false
      fCancelerRef <- Ref.new $ pure unit
      aRef <- Ref.new Nothing
      aErrorRef <- Ref.new false
      aCancelerRef <- Ref.new $ pure unit
      let
        errorCallback :: Ref Boolean -> Ref Boolean -> Ref Canceler -> Callback x
        errorCallback myErrorRef otherErrorRef otherCanceler x = do
          otherError <- Ref.read otherErrorRef
          if otherError then
            pure unit
          else do
            join $ Ref.read otherCanceler
            Ref.write true myErrorRef
            xC x
      tf
        ( \f -> do
            ma <- Ref.read aRef
            case ma of
              Just a -> bC $ f a
              Nothing -> Ref.write (Just f) fRef
        )
        (errorCallback fErrorRef aErrorRef aCancelerRef)
        fCancelerRef
      ta
        ( \a -> do
            mf <- Ref.read fRef
            case mf of
              Just f -> bC $ f a
              Nothing -> Ref.write (Just a) aRef
        )
        (errorCallback aErrorRef fErrorRef fCancelerRef)
        aCancelerRef
      Ref.write
        ( do
            join $ Ref.read fCancelerRef
            join $ Ref.read aCancelerRef
        )
        ref

instance applicativePartask :: Applicative (ParTask x) where
  pure a = ParTask \aC _ _ -> aC a

instance altParTask :: Alt (ParTask x) where
  alt = alt

alt :: ∀ a x. ParTask x a -> ParTask x a -> ParTask x a
alt (ParTask t1) (ParTask t2) =
  ParTask \aC xC ref -> do
    t1Ref <- Ref.new false
    t1ErrorRef <- Ref.new false
    t1CancelerRef <- Ref.new $ pure unit
    t2Ref <- Ref.new false
    t2ErrorRef <- Ref.new false
    t2CancelerRef <- Ref.new $ pure unit
    let
      successCallback :: Ref Boolean -> Ref Boolean -> Ref Canceler -> Callback a
      successCallback myARef otherARef otherCancelerRef a = do
        otherA <- Ref.read otherARef
        if otherA then
          pure unit
        else do
          join $ Ref.read otherCancelerRef
          Ref.write true myARef
          aC a

      errorCallback :: Ref Boolean -> Ref Boolean -> Ref Canceler -> Callback x
      errorCallback myErrorRef otherErrorRef otherCancelerRef x = do
        otherError <- Ref.read otherErrorRef
        if otherError then do
          join $ Ref.read otherCancelerRef
          xC x
        else
          Ref.write true myErrorRef
    t1
      (successCallback t1Ref t2Ref t2CancelerRef)
      (errorCallback t1ErrorRef t2ErrorRef t2CancelerRef)
      t1CancelerRef
    t2
      (successCallback t2Ref t1Ref t1CancelerRef)
      (errorCallback t2ErrorRef t1ErrorRef t1CancelerRef)
      t2CancelerRef
    Ref.write
      ( do
          join $ Ref.read t1CancelerRef
          join $ Ref.read t2CancelerRef
      )
      ref

instance parallelTask :: Parallel (ParTask x) (Task x) where
  parallel (Task t) = ParTask t
  sequential (ParTask p) = Task p

mapError :: ∀ a x y. (x -> y) -> Task x a -> Task y a
mapError f (Task t) = Task $ t <~. (.>) f

bindError :: ∀ a x y. Task x a -> (x -> Task y a) -> Task y a
bindError (Task tx) f = Task \aC yC ref -> tx aC (\x -> unwrap (f x) aC yC ref) ref

-- | Execute a task, capturing the result with an effectual function.
capture :: ∀ a x. Callback (x \/ a) -> Task x a -> Effect Unit
capture handler (Task t) = do
  ref <- Ref.new $ pure unit
  t (handler <. Right) (handler <. Left) ref

-- | Execute a task, disregarding its result.
run :: ∀ a x. Task x a -> Effect Unit
run = capture $ const $ pure unit

-- | To use `makeTask`, you need to create a function that takes a callback for both success and failure, and returns an effect that will cancel a task in the event of an error during parallel execution. If you do not need or do not care about cancelling a task, you can use `pure $ pure unit`.
-- |
-- | Here is how you would make a simple waiting task using [js-timers](https://pursuit.purescript.org/packages/purescript-js-timers):
-- |
-- | ```
-- | wait :: ∀ x. Int -> Task x Unit
-- | wait ms =
-- |   makeTask \aC _ -> do
-- |     id <- setTimeout ms $ aC unit
-- |     pure $ clearTimeout id
-- | ```
makeTask :: ∀ a x. (Callback a -> Callback x -> Effect Canceler) -> Task x a
makeTask f = Task \aC xC ref -> f aC xC >>= Ref.write ~$ ref

-- | A type that represents JavaScript promises. Use this with the FFI and `fromPromise` to turn promises into tasks.
-- |
-- | ```
-- | foreign import fetchImpl :: String -> Effect (Promise Error Json)
-- | ```
foreign import data Promise :: Type -> Type -> Type

foreign import fromPromiseImpl ::
  ∀ a x.
  (∀ b y. (ForeignCallback b -> ForeignCallback y -> Effect Canceler) -> Task y b) ->
  Effect Unit ->
  Effect (Promise x a) ->
  Task x a

-- | A convenience function for creating tasks from JavaScript promises.
-- |
-- | ```
-- | fetch :: String -> Task Error Json
-- | fetch = fromPromise <<< fetchImpl
-- | ```
fromPromise :: ∀ x a. Effect (Promise x a) -> Task x a
fromPromise = fromPromiseImpl fromForeign $ pure unit

type ForeignCallback a
  = EffectFn1 a Unit

-- | A convenience function for creating tasks from JavaScript functions that take callbacks.
-- |
-- | ```
-- | // JavaScript
-- | exports.waitImpl = ms => cb => () => {
-- |     const id = setTimeout(cb, ms);
-- |     return () => clearTimeout(id);
-- | };
-- |
-- | -- PureScript
-- | foreign import waitImpl :: Int -> ForeignCallback Unit -> Effect Canceler
-- |
-- | wait :: ∀ x. Int -> Task x Unit
-- | wait ms = fromForeign \cb _ -> waitImpl ms cb
-- | ```
fromForeign ::
  ∀ x a.
  (ForeignCallback a -> ForeignCallback x -> Effect Canceler) ->
  Task x a
fromForeign f = Task \aC xC ref -> f (mkEffectFn1 aC) (mkEffectFn1 xC) >>= Ref.write ~$ ref
