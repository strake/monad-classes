{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Classes.Core where

import GHC.Prim (Proxy#, proxy#)
import Control.Monad.Trans.Class
import Data.Peano (Peano (..))

-- Peano naturals; used at the type level to denote how far a computation should be lifted
-- ... but GHC not promotes type synonyms
-- type Peano = Peano

-- | @'CanDo' m eff@ describes whether the given effect can be performed in the
-- monad @m@ (without any additional lifting)
type family CanDo (m :: * -> *) (eff :: k) :: Bool

-- | @'MapCanDo' eff stack@ maps the type-level function @(\m -> 'CanDo'
-- m eff)@ over all layers that a monad transformer stack @stack@ consists of
type family MapCanDo (eff :: k) (stack :: * -> *) :: [Bool] where
  MapCanDo eff (t m) = CanDo (t m) eff ': MapCanDo eff m
  MapCanDo eff m = '[ CanDo m eff ]

-- | @'FindTrue' bs@ returns a (type-level) index of the first occurrence
-- of 'True' in a list of booleans
type family FindTrue
  (bs :: [Bool]) -- results of calling Contains
  :: Peano
  where
  FindTrue (True ': t) = Zero
  FindTrue (False ': t) = Succ (FindTrue t)

-- | @'Find' eff m@ finds the first transformer in a monad transformer
-- stack that can handle the effect @eff@
type Find eff (m :: * -> *) =
  FindTrue (MapCanDo eff m)

class MonadLiftN (n :: Peano) m
  where
    type Down n m :: * -> *
    liftN :: Proxy# n -> Down n m a -> m a

instance MonadLiftN Zero m
  where
    type Down Zero m = m
    liftN _ = id

instance
  ( MonadLiftN n m
  , MonadTrans t
  , Monad m
  ) => MonadLiftN (Succ n) (t m)
  where
    type Down (Succ n) (t m) = Down n m
    liftN _ = lift . liftN (proxy# :: Proxy# n)
