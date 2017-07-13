module Control.Monad.Classes.Exec
  ( MonadExec
  , exec
  , MonadExecN(..)
  , EffExec
  )
  where
import Control.Monad.Trans.Class
import GHC.Prim (Proxy#, proxy#)
import Control.Monad.Classes.Core
import Control.Monad.Classes.Effects
import Control.Monad.Classes.TypeErrors
import Data.Peano (Peano (..))

type instance CanDo IO (EffExec IO) = True

class Monad m => MonadExecN (n :: Peano) w m where
  execN :: Proxy# n -> (w a -> m a)

instance Monad w => MonadExecN Zero w w where
  execN _ = id

instance (MonadTrans t, Monad (t m), MonadExecN n w m, Monad m)
  => MonadExecN (Succ n) w (t m)
  where
    execN _ = lift . execN (proxy# :: Proxy# n)

instance {-# INCOHERENT #-} (InstanceNotFoundError "MonadExec" w m, Monad m)
  => MonadExecN n w m
  where
    execN = error "unreachable"

type MonadExec w m = MonadExecN (Find (EffExec w) m) w m

-- | Lift an 'IO' action
exec :: forall w m a . MonadExec w m => w a -> m a
exec = execN (proxy# :: Proxy# (Find (EffExec w) m))
