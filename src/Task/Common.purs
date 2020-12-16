module Task.Common where

import MasonPrelude
import Effect.Timer (clearTimeout, setTimeout)
import Task (Task, makeTask)

-- | wait `x` milliseconds
wait :: ∀ x. Int -> Task x Unit
wait ms =
  makeTask \aC _ -> do
    id <- setTimeout ms $ aC unit
    pure $ clearTimeout id
