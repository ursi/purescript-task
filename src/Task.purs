module Task
  ( Callback
  , ForeignCallback
  , ParTask
  , Promise
  , Task
  , Canceller
  , bindError
  , capture
  , fail
  , fromForeign
  , fromPromise
  , makeTask
  , run
  ) where

import MasonPrelude
import Control.Parallel (class Parallel)
import Data.Bifunctor (class Bifunctor)
import Effect.Ref (Ref)
import Effect.Ref as Ref

type Callback a
  = a -> Effect Unit

type Canceller
  = Effect Unit

newtype Task x a
  = Task (Callback a -> Callback x -> Ref Canceller -> Effect Unit)

-- this is used instead of a Newtype instance to keep Task opaque
unwrap :: ∀ x a. Task x a -> (Callback a -> Callback x -> Ref Canceller -> Effect Unit)
unwrap (Task t) = t

instance functorTask :: Functor (Task x) where
  map f (Task t) = Task $ t <. (.>) f

instance applyTask :: Apply (Task x) where
  apply (Task tf) (Task ta) = Task \bC xC ref -> tf (\f -> ta (bC <. f) xC ref) xC ref

instance applicativeTask :: Applicative (Task x) where
  pure a = Task \aC _ _ -> aC a

instance bindTask :: Bind (Task x) where
  bind (Task ta) f = Task \bC xC ref -> ta (\a -> unwrap (f a) bC xC ref) xC ref

instance monadTask :: Monad (Task x)

instance altTask :: Alt (Task x) where
  alt = bindError <~. const

instance semigroupTask :: Semigroup a => Semigroup (Task x a) where
  append = lift2 append

instance monoidTask :: Monoid a => Monoid (Task x a) where
  mempty = pure mempty

instance bifunctorTask :: Bifunctor Task where
  bimap lmap rmap task = task <#> rmap # mapError lmap

instance monadEffectTask :: MonadEffect (Task x) where
  liftEffect aEff = Task \aC _ _ -> aEff >>= aC

newtype ParTask x a
  = ParTask (Callback a -> Callback x -> Ref Canceller -> Effect Unit)

instance functorParTask :: Functor (ParTask x) where
  map f (ParTask t) = ParTask $ t <. (.>) f

instance applyParTask :: Apply (ParTask x) where
  apply (ParTask tf) (ParTask ta) =
    ParTask \bC xC ref -> do
      fRef <- Ref.new Nothing
      fErrorRef <- Ref.new false
      fCancellerRef <- Ref.new $ pure unit
      aRef <- Ref.new Nothing
      aErrorRef <- Ref.new false
      aCancellerRef <- Ref.new $ pure unit
      let
        errorCallback :: Ref Boolean -> Ref Boolean -> Ref Canceller -> Callback x
        errorCallback myErrorRef otherErrorRef otherCanceller x = do
          otherError <- Ref.read otherErrorRef
          if otherError then
            pure unit
          else do
            join $ Ref.read otherCanceller
            Ref.write true myErrorRef
            xC x
      tf
        ( \f -> do
            ma <- Ref.read aRef
            case ma of
              Just a -> bC $ f a
              Nothing -> Ref.write (Just f) fRef
        )
        (errorCallback fErrorRef aErrorRef aCancellerRef)
        fCancellerRef
      ta
        ( \a -> do
            mf <- Ref.read fRef
            case mf of
              Just f -> bC $ f a
              Nothing -> Ref.write (Just a) aRef
        )
        (errorCallback aErrorRef fErrorRef fCancellerRef)
        aCancellerRef
      Ref.write
        ( do
            join $ Ref.read fCancellerRef
            join $ Ref.read aCancellerRef
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
    t1CancellerRef <- Ref.new $ pure unit
    t2Ref <- Ref.new false
    t2ErrorRef <- Ref.new false
    t2CancellerRef <- Ref.new $ pure unit
    let
      successCallback :: Ref Boolean -> Ref Boolean -> Ref Canceller -> Callback a
      successCallback myARef otherARef otherCancellerRef a = do
        otherA <- Ref.read otherARef
        if otherA then
          pure unit
        else do
          join $ Ref.read otherCancellerRef
          Ref.write true myARef
          aC a

      errorCallback :: Ref Boolean -> Ref Boolean -> Ref Canceller -> Callback x
      errorCallback myErrorRef otherErrorRef otherCancellerRef x = do
        otherError <- Ref.read otherErrorRef
        if otherError then do
          join $ Ref.read otherCancellerRef
          xC x
        else
          Ref.write true myErrorRef
    t1
      (successCallback t1Ref t2Ref t2CancellerRef)
      (errorCallback t1ErrorRef t2ErrorRef t2CancellerRef)
      t1CancellerRef
    t2
      (successCallback t2Ref t1Ref t1CancellerRef)
      (errorCallback t2ErrorRef t1ErrorRef t1CancellerRef)
      t2CancellerRef
    Ref.write
      ( do
          join $ Ref.read t1CancellerRef
          join $ Ref.read t2CancellerRef
      )
      ref

instance parallelTask :: Parallel (ParTask x) (Task x) where
  parallel (Task t) = ParTask t
  sequential (ParTask p) = Task p

mapError :: ∀ a x y. (x -> y) -> Task x a -> Task y a
mapError f (Task t) = Task $ t <~. (.>) f

fail :: ∀ a x. x -> Task x a
fail x = Task \_ xC _ -> xC x

bindError :: ∀ a x y. Task x a -> (x -> Task y a) -> Task y a
bindError (Task tx) f = Task \aC yC ref -> tx aC (\x -> unwrap (f x) aC yC ref) ref

capture :: ∀ a x. Callback (x \/ a) -> Task x a -> Effect Unit
capture handler (Task t) = do
  ref <- Ref.new $ pure unit
  t (handler <. Right) (handler <. Left) ref

run :: ∀ a x. Task x a -> Effect Unit
run = capture $ const $ pure unit

makeTask :: ∀ a x. (Callback a -> Callback x -> Effect Canceller) -> Task x a
makeTask f = Task \aC xC ref -> f aC xC >>= Ref.write ~$ ref

foreign import data Promise :: Type -> Type -> Type

foreign import fromPromiseImpl ::
  ∀ a x.
  (∀ b y. (ForeignCallback b -> ForeignCallback y -> Effect Canceller) -> Task y b) ->
  Effect Unit ->
  Effect (Promise x a) ->
  Task x a

fromPromise :: ∀ x a. Effect (Promise x a) -> Task x a
fromPromise = fromPromiseImpl fromForeign $ pure unit

type ForeignCallback a
  = EffectFn1 a Unit

fromForeign ::
  ∀ x a.
  (ForeignCallback a -> ForeignCallback x -> Effect Canceller) ->
  Task x a
fromForeign f = Task \aC xC ref -> f (mkEffectFn1 aC) (mkEffectFn1 xC) >>= Ref.write ~$ ref
