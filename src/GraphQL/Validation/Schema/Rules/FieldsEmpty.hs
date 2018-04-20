module GraphQL.Validation.Schema.Rules.FieldsEmpty (Error, visit) where

import Protolude

import GraphQL.Type.Schema


data Error = Error
  { typeName :: Text
  }


visit :: TypeDefinition -> [Error]
visit = \case
  TypeDefinitionObject ObjectTypeDefinition{ name = typeName, fields = [] } ->
    [ Error { typeName} ]
  TypeDefinitionInterface InterfaceTypeDefinition{ name = typeName, fields = [] } ->
    [ Error { typeName} ]
  _ -> []
