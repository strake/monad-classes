module Control.Monad.Classes.Reader where
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Lazy as SL
import qualified Control.Monad.Trans.State.Strict as SS
import Control.Monad.Morph (MFunctor, hoist)
import Control.Monad.Trans.Class
import GHC.Prim (Proxy#, proxy#)
import Control.Monad.Classes.Core
import Control.Monad.Classes.Effects
import Control.Monad.Classes.TypeErrors
import Data.Peano

type instance CanDo (R.ReaderT e m) eff = ReaderCanDo e eff

type instance CanDo ((->) e) eff = ReaderCanDo e eff

type family ReaderCanDo e eff where
  ReaderCanDo e (EffReader e) = True
  ReaderCanDo e (EffLocal e) = True
  ReaderCanDo e eff = False

class Monad m => MonadReaderN (n :: Peano) r m where
  askN :: Proxy# n -> m r

instance Monad m => MonadReaderN Zero r (R.ReaderT r m) where
  askN _ = R.ask

instance Monad m => MonadReaderN Zero r (SL.StateT r m) where
  askN _ = SL.get

instance Monad m => MonadReaderN Zero r (SS.StateT r m) where
  askN _ = SS.get

instance MonadReaderN Zero r ((->) r) where
  askN _ = id

instance (MonadTrans t, Monad (t m), MonadReaderN n r m, Monad m)
  => MonadReaderN (Succ n) r (t m)
  where
    askN _ = lift $ askN (proxy# :: Proxy# n)

instance {-# INCOHERENT #-} (InstanceNotFoundError "MonadReader" r m, Monad m)
  => MonadReaderN n r m
  where
    askN = error "unreachable"

class Monad m => MonadLocalN (n :: Peano) r m where
  localN :: Proxy# n -> ((r -> r) -> m a -> m a)

instance Monad m => MonadLocalN Zero r (R.ReaderT r m) where
  localN _ = R.local

stateLocal :: Monad m => (a -> m ()) -> m a -> (a -> a) -> m b -> m b
stateLocal putFn getFn f a = do
  s <- getFn
  putFn (f s)
  r <- a
  putFn s
  return r

instance (Monad m) => MonadLocalN Zero r (SL.StateT r m) where
  localN _ = stateLocal SL.put SL.get

instance (Monad m) => MonadLocalN Zero r (SS.StateT r m) where
  localN _ = stateLocal SS.put SS.get

instance MonadLocalN Zero r ((->) r) where
  localN _ = flip (.)

instance (MonadTrans t, Monad (t m), MFunctor t, MonadLocalN n r m, Monad m)
  => MonadLocalN (Succ n) r (t m)
  where
    localN _ = \f -> hoist (localN (proxy# :: Proxy# n) f)

instance {-# INCOHERENT #-} (InstanceNotFoundError "MonadLocal" r m, Monad m)
  => MonadLocalN n r m
  where
    localN = error "unreachable"

-- | The @'MonadReader' r m@ constraint asserts that @m@ is a monad stack
-- that supports a fixed environment of type @r@
type MonadReader e m = MonadReaderN (Find (EffReader e) m) e m

-- | The @'MonadLocal' r m@ constraint asserts that @m@ is a monad stack
-- that supports a fixed environment of type @r@ that can be changed
-- externally to the monad
type MonadLocal e m = MonadLocalN (Find (EffLocal e) m) e m

-- | Fetch the environment passed through the reader monad
ask :: forall m r . MonadReader r m => m r
ask = askN (proxy# :: Proxy# (Find (EffReader r) m))

-- | Executes a computation in a modified environment.
local :: forall a m r. MonadLocal r m
      => (r -> r)  -- ^ The function to modify the environment.
      -> m a       -- ^ @Reader@ to run in the modified environment.
      -> m a
local = localN (proxy# :: Proxy# (Find (EffLocal r) m))

runReader :: r -> R.ReaderT r m a -> m a
runReader = flip R.runReaderT
