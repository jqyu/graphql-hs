module GraphQL.Validation.Schema.Rules.TypeName (Error, visit) where

import Protolude

import GraphQL.Validation.Name (isValidName)
import GraphQL.Type.Schema


data Error = Error
  { typeName :: Text
  }


visit :: TypeDefinition -> [Error]
visit def
    | isValidName typeName = []
    | otherwise = [ Error { typeName } ]
  where
    typeName = getTypeName def
