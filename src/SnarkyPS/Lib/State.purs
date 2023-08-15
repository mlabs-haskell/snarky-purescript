module SnarkyPS.Lib.State where

import SnarkyPS.Lib.Context (Context, Void)

foreign import data State :: Type -> Type

foreign import setState :: forall a . a -> State a -> Context Void

foreign import getState :: forall a . State a -> Context a

foreign import assertEqState :: forall a . a -> State a -> Context Void

foreign import getAndAssertEqState :: forall a . State a -> Context a

foreign import newState :: forall a. Context (State a)