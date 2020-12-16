module Task.Class where

import MasonPrelude
import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT)
import Control.Monad.List.Trans (ListT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.RWS (RWST)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Control.Monad.Trans.Class (lift)
import Task (Task)

-- purty >:(
class
  MonadEffect m <= MonadTask x m | m -> x where
  liftTask :: Task x ~> m

instance monadTaskTask :: MonadTask x (Task x) where
  liftTask = identity

instance monadTaskContT :: MonadTask x m => MonadTask x (ContT r m) where
  liftTask = lift <. liftTask

instance monadTaskExceptT :: MonadTask x m => MonadTask x (ExceptT e m) where
  liftTask = lift <. liftTask

instance monadTaskListT :: MonadTask x m => MonadTask x (ListT m) where
  liftTask = lift <. liftTask

instance monadTaskMaybeT :: MonadTask x m => MonadTask x (MaybeT m) where
  liftTask = lift <. liftTask

instance monadTaskReaderT :: MonadTask x m => MonadTask x (ReaderT r m) where
  liftTask = lift <. liftTask

instance monadTaskRWSTT :: (MonadTask x m, Monoid w) => MonadTask x (RWST r w s m) where
  liftTask = lift <. liftTask

instance monadTaskStateT :: MonadTask x m => MonadTask x (StateT s m) where
  liftTask = lift <. liftTask

instance monadTaskWriterT :: (MonadTask x m, Monoid w) => MonadTask x (WriterT w m) where
  liftTask = lift <. liftTask
