module GraphQL.Validation.Name
  ( Name(..)
  , NameError(..)
  , makeName
  , isValidName
  ) where

import Protolude

import Data.Attoparsec.Text (parseOnly)
import GraphQL.Language.AST (Name(..))
import GraphQL.Language.Parser (nameParser)


makeName :: Text -> Either NameError Name
makeName name =
  first (const (NameError name)) (parseOnly nameParser name)


isValidName :: Text -> Bool
isValidName = isRight . parseOnly nameParser


newtype NameError = NameError Text deriving (Eq, Ord, Show)
