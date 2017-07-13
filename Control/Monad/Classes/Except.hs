module Control.Monad.Classes.Except where
import qualified Control.Monad.Trans.Except as Exc
import qualified Control.Monad.Trans.Maybe as Mb
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Trans.Class
import GHC.Prim (Proxy#, proxy#)
import Control.Monad.Classes.Core
import Control.Monad.Classes.Effects
import Control.Monad.Classes.TypeErrors
import Data.Peano (Peano (..))

type instance CanDo IO (EffExcept e) = True

type instance CanDo (Exc.ExceptT e m) eff = ExceptCanDo e eff

type instance CanDo (Mb.MaybeT m) eff = ExceptCanDo () eff

type family ExceptCanDo e eff where
  ExceptCanDo e (EffExcept e) = True
  ExceptCanDo e eff = False

class Monad m => MonadExceptN (n :: Peano) e m where
  throwN :: Proxy# n -> (e -> m a)

instance Monad m => MonadExceptN Zero e (Exc.ExceptT e m) where
  throwN _ = Exc.throwE

instance E.Exception e => MonadExceptN Zero e IO where
  throwN _ = E.throwIO

instance Monad m => MonadExceptN Zero () (Mb.MaybeT m) where
  throwN _ _ = mzero

instance (MonadTrans t, Monad (t m), MonadExceptN n e m, Monad m)
  => MonadExceptN (Succ n) e (t m)
  where
    throwN _ = lift . throwN (proxy# :: Proxy# n)

instance {-# INCOHERENT #-} (InstanceNotFoundError "MonadExcept" e m, Monad m)
  => MonadExceptN n e m
  where
    throwN = error "unreachable"

-- | The @'MonadExcept' e m@ constraint asserts that @m@ is a monad stack
-- that supports throwing exceptions of type @e@
type MonadExcept e m = MonadExceptN (Find (EffExcept e) m) e m

-- | Throw an exception
throw :: forall a e m . MonadExcept e m => e -> m a
throw = throwN (proxy# :: Proxy# (Find (EffExcept e) m))

runExcept :: Exc.ExceptT e m a -> m (Either e a)
runExcept = Exc.runExceptT

runMaybe :: Mb.MaybeT m a -> m (Maybe a)
runMaybe = Mb.runMaybeT
