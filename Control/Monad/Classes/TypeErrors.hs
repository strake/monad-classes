module Control.Monad.Classes.TypeErrors where

import GHC.TypeLits

type family InstanceNotFoundError name param m where
  InstanceNotFoundError name param m =
    TypeError (Text "Control.Monad.Classes:" :$$:
               Text name :<>: Text " (" :<>: ShowType param :<>: Text ")" :<>:
               Text " instance not found for " :<>: ShowType m)
