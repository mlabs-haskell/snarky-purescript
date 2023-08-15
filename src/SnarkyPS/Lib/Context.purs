module SnarkyPS.Lib.Context (Assertion, assertMany, assertAndThen) where

import Prelude hiding (Void)

{- This module defines a Context monad that allows us to properly represent computations in the
   'Snarky Context'. The underlying representation is essentially the Identity monad, but unlike
   Identity, `Context` is NOT a comonad, i.e., you cannot extract the value.

   This *should* (once we get all the type sigs correct) guard (to some extent) against
   mixing up computations in the object language (snarky) with computations in the model language.

   Really this should be a constrained monad, but those aren't pleasant to work with in PS.

   TEMPORARILY DEPRECATED: We need some monad eventually to control effects, but there's not much of a point
                           to it now as the computations are pure.

foreign import data Context :: Type -> Type

foreign import fmapContext :: forall (t1 :: Type) (t2 :: Type). (t1 -> t2) -> Context t1 -> Context t2

foreign import pureContext :: forall (t :: Type). t -> Context t

foreign import lift2Context :: forall f a b c. (a -> b -> c) -> Context a -> Context b -> Context c

foreign import applyContext :: forall b a. Context (a -> b) -> Context a -> Context b

foreign import bindContext :: forall a b. Context a -> (a -> Context b) -> Context b

-- TODO: Rename 'Void' to 'Assertion'
foreign import data Void :: Type

instance Functor Context where
  map = fmapContext

instance Apply Context where
  apply = applyContext

instance Applicative Context where
  pure = pureContext

instance Bind Context where
  bind = bindContext

instance Discard Void where
  discard = bind

instance Monad Context

assertAndThen :: forall a. Assertion -> Context a -> Context a
assertAndThen cv x = do
  void cv
  x


-}

-- Underlying type is JS void
foreign import data Assertion :: Type

foreign import assertMany :: Array Assertion -> Assertion

foreign import assertAndThen :: forall (t :: Type). Assertion -> t -> t
