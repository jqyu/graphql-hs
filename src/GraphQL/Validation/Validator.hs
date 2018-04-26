-- this module is meant to be imported qualified
-- this is an Either that combines all errors seen

module GraphQL.Validation.Validator
  ( M(run)
  , fromEither
  , fromErrors
  , note
  , throw
  , throwErrors
  ) where

import Protolude hiding ((<>), note)

import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Semigroup ((<>))

newtype M e a
  = Validator { run :: Either (NonEmpty e) a }
  deriving (Eq, Ord, Functor, Monad)

instance Bifunctor M where
  first f (Validator v) = Validator (first (map f) v)
  second = map

instance Applicative (M e) where
  pure = Validator . Right
  Validator (Left e1) <*> Validator (Left e2) = Validator (Left (e1 <> e2))
  Validator (Left e) <*> _ = Validator (Left e)
  _ <*> Validator (Left e) = Validator (Left e)
  Validator (Right f) <*> Validator (Right x) = Validator (Right (f x))


fromEither :: Either e a -> M e a
fromEither = Validator . first (:| [])


fromErrors :: [e] -> M e ()
fromErrors = maybe (pure ()) throwErrors . nonEmpty


note :: e -> Maybe a -> M e a
note e = maybe (throw e) pure


throw :: e -> M e a
throw e = throwErrors (e :| [])


throwErrors :: NonEmpty e -> M e a
throwErrors = Validator . Left 
