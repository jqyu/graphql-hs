module GraphQL.Validation.Schema.Rules.FieldsUnique (Error, visit) where

import Protolude

import qualified Data.Map as Map (empty, insert, lookup)
import GraphQL.Type.Schema


data Error = Error
  { typeName :: Text
  , fieldName :: Text
  , field1 :: FieldDefinition
  , field2 :: FieldDefinition
  }


visit :: TypeDefinition -> [Error]
visit = \case
  TypeDefinitionObject ObjectTypeDefinition{ name, fields } ->
    checkFields name fields
  TypeDefinitionInterface InterfaceTypeDefinition{ name, fields } ->
    checkFields name fields
  _ -> []


checkFields :: Text -> [FieldDefinition] -> [Error]
checkFields typeName =
    snd . foldr checkField (Map.empty, [])
  where
    checkField field1@FieldDefinition{ name = fieldName } (seen, errs) =
      case Map.lookup fieldName seen of
        Just field2 ->
          (seen, Error { typeName, fieldName, field1, field2 } : errs)
        Nothing ->
          (Map.insert fieldName field1 seen, errs)
