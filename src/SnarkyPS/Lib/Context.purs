module SnarkyPS.Lib.Context (Assertion, assertMany, assertAndThen) where

import Prelude hiding (Void)
import Control.Monad (void)


{- DEPRECATED in favor of the ZkM Monad. TODO: Rename this module to SnarkyPS.Lib.Assertion

-}

{- | Opaque Assertion type -}
foreign import data Assertion :: Type

{- | Turn an array of `Assertion`s into a single `Assertion` -}
foreign import assertMany :: Array Assertion -> Assertion

{-
| Note an assertion and return the second argument.
|
| This allows sequencing assertions in pure code (i.e.
| computations outside of `ZkM` )
-}
foreign import assertAndThen :: forall (t :: Type). Assertion -> t -> t
