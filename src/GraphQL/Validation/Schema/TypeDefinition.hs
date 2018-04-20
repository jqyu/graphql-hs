-- Type definition for a ValidatedSchema used for introspection

module GraphQL.Validation.Schema.TypeDefinition
  ( withIntrospectionFields
  , __schema
  , __type
  ) where


import Protolude

import qualified Data.Map as Map (union)
import Data.Row ((.!))
import GraphQL.DSL
import GraphQL.Language.Encoder as Encoder (value)
import GraphQL.Type.Definition (Definition(ObjectDefinition))
import GraphQL.Type.Object as Object (Definition(..), fieldsFromList)
import GraphQL.Type.Schema
import GraphQL.Validation.Schema


withIntrospectionFields :: Monad m => ValidatedSchema -> ObjectDefinition m a -> ObjectDefinition m a
withIntrospectionFields validatedSchema (ObjectDefinition def@(Object.Definition{fields})) =
  ObjectDefinition def
    { Object.fields = fields `Map.union` Object.fieldsFromList
      [ field "__schema" (nonNull __schema) fieldSpec
        { description = "Access the current type schema of thisserver."
        , resolver = \_ _ -> pure validatedSchema
        }
      , field "__type" __type fieldSpec
        { description = "Request the type information of a single type."
        , arguments =
            arg @"name" (nonNull string) argSpec
              { description = "Name of the type" }
        , resolver = \args _ ->
            pure . map TypeNamed . lookupTypeDefinition validatedSchema $ args .! #name
        }
      ]
    }


__schema :: Monad m => ObjectDefinition m ValidatedSchema
__schema = defineObject @"__Schema" objectSpec
  { description =
      "A GraphQL Schema defines the capabilities of a GraphQL server. \
      \It exposes all available types and directives on the server, \
      \as well as the entry points for query, mutation, and subscription operations."
  , fields =
    [ field "types" (nonNull (listOf (nonNull __type))) fieldSpec
      { description =
          "A list of all types supported by this server."
      , resolver = \_ ->
          pure
          . map TypeNamed
          . getTypeDefinitions
      }
    , field "queryType" (nonNull __type) fieldSpec
      { description =
          "The type that query operations will be rooted at."
      , resolver = \_ ->
          pure
          . TypeNamed
          . TypeDefinitionObject
          . getRootQueryType
      }
    , field "mutationType" __type fieldSpec
      { description =
          "If the server supports mutation, the type that \
          \mutation operations will be rooted at."
      , resolver = \_ ->
          pure
          . map (TypeNamed . TypeDefinitionObject)
          . getRootMutationType
      }
    , field "subscriptionType" __type fieldSpec
      { description =
          "If the server supports subscriptions, the type that \
          \subscription operations will be rooted at."
      , resolver = \_ _ -> pure Nothing
      }
    -- TODO
    , field "directives" (nonNull (listOf string)) fieldSpec
      { resolver = \_ _ -> pure []
      }
    ]
  }


__type :: Monad m => ObjectDefinition m (AnnotatedType TypeDefinition)
__type = defineObject @"__Type" objectSpec
  { description =
      "The fundamental unit of any GraphQL Schema is the type. \
      \There are many kinds of types in GraphQL as represented by the __TypeKind enum.\n \
      \\n \
      \Depending on the kind of a type, certain fields describe information about that type. \
      \Scalar types provide no information beyond a name and description, while Enum types \
      \provide their values. Object and Interface types provide the fields they describe. \
      \Abstract types, Union and Interface, provide the Object types possible at runtime. \
      \List and NonNull types compose other types."
  , fields =
    [ field "kind" (nonNull string) fieldSpec
        -- TODO: make this an enum
        { resolver = \_ def ->
            pure $ case def of
              TypeNamed named ->
                case named of
                  TypeDefinitionObject _ -> "OBJECT"
                  TypeDefinitionScalar _ -> "SCALAR"
                  TypeDefinitionInterface _ -> "INTERFACE"
                  TypeDefinitionUnion _ -> "UNION"
                  TypeDefinitionEnum _ -> "ENUM"
                  TypeDefinitionInputObject _ -> "INPUT_OBJECT"
              TypeList _ -> "LIST"
              TypeNonNull _ -> "NON_NULL"
        }
    , field "name" string fieldSpec
      { description =
          "Type name for a named type. If the type is not named, such as a \
          \List or NonNull type, this field is null"
      , resolver = \_ -> 
          pure . map getTypeName . asNamedType
      }
    , field "description" string fieldSpec
      { description =
          "Type description for a named type. If the type is not named, such as a \
          \List or NonNull type, this field is null"
      , resolver = \_ -> 
          pure . nonEmptyToNothing . map getTypeDescription . asNamedType
      }
    , field "fields" (listOf (nonNull __field)) fieldSpec
      { description =
          "Fields of an Object or Interface type"
      , arguments =
          argWithDefault @"includeDeprecated" boolean False argSpec
            { description = "whether or not to include deprecated fields"
            }
      , resolver = \args def ->
          pure $ case def of
            TypeNamed (TypeDefinitionObject ObjectTypeDefinition{fields}) ->
              Just . filter (includeDeprecatedField (args .! #includeDeprecated)) $ fields
            TypeNamed (TypeDefinitionInterface InterfaceTypeDefinition{fields}) ->
              Just . filter (includeDeprecatedField (args .! #includeDeprecated)) $ fields
            _ ->
              Nothing
      }
    , field "interfaces" (listOf (nonNull __type)) fieldSpec
      { description =
          "Interfaces that an Object type implements"
      , resolver = \_ def ->
          pure $ case def of
            TypeNamed (TypeDefinitionObject ObjectTypeDefinition{interfaces}) ->
              Just $ map (TypeNamed . TypeDefinitionInterface) interfaces
            _ ->
              Nothing
      }
    , field "possibleTypes" (listOf (nonNull __type)) fieldSpec
      { description =
          "Possible types of a Union, or implementations of an Interface"
      , resolver = \_ def ->
          pure $ case def of
            TypeNamed (TypeDefinitionInterface InterfaceTypeDefinition{possibleTypes}) ->
              Just $ map (TypeNamed . TypeDefinitionObject) possibleTypes
            TypeNamed (TypeDefinitionUnion UnionTypeDefinition{possibleTypes}) ->
              Just $ map (TypeNamed . TypeDefinitionObject) possibleTypes
            _ ->
              Nothing
      }
    -- TODO
    , field "enumValues" (listOf string) fieldSpec
      { resolver = \_ _ -> pure Nothing
      }
    , field "inputFields" (listOf (nonNull __inputValue)) fieldSpec
      { description =
          "Input fields of an InputObject type"
      , resolver = \_ def ->
          pure $ case def of
            TypeNamed (TypeDefinitionInputObject InputObjectTypeDefinition{inputFields}) ->
              Just inputFields
            _ ->
              Nothing
      }
    , field "ofType" __type fieldSpec
      { description =
          "Wrapped type of a List or NonNull type. If the type is not a List or NonNull \
          \this field is null"
      , resolver = \_ def ->
          pure $ case def of
            TypeNamed _ -> Nothing
            TypeList (ListType t) -> Just t
            TypeNonNull (NonNullTypeNamed t) -> Just (TypeNamed t)
            TypeNonNull (NonNullTypeList t) -> Just (TypeList t)
      }
    ]
  }


__field :: Monad m => ObjectDefinition m FieldDefinition
__field = defineObject @"__Field" objectSpec
  { description =
      "Object and Interface types are described by a list of Fields, each of which \
      \has a name, potentially a list of arguments, and a return type."
  , fields =
    [ field "name" (nonNull string) fieldSpec
      { resolver = \_ FieldDefinition{name} ->
          pure name
      }
    , field "description" string fieldSpec
      { resolver = \_ FieldDefinition{description} ->
          pure . nonEmptyToNothing $ Just description
      }
    , field "args" (nonNull (listOf (nonNull __inputValue))) fieldSpec
      { resolver = \_ FieldDefinition{args} ->
          pure args
      }
    , field "type" (nonNull __type) fieldSpec
      { resolver = \_ FieldDefinition{_type} ->
          pure $ map fromOutputType _type
      }
    , field "isDeprecated" (nonNull boolean) fieldSpec
      { resolver = \_ FieldDefinition{deprecationReason} ->
          pure $ isJust deprecationReason
      }
    , field "deprecationReason" string fieldSpec
      { resolver = \_ FieldDefinition{deprecationReason} ->
          pure $ nonEmptyToNothing deprecationReason
      }
    ]
  }


__inputValue :: Monad m => ObjectDefinition m InputValueDefinition
__inputValue = defineObject @"__InputValue" objectSpec
  { description =
      "Arguments provided to Fields or Directives and the input fields of an \
      \InputObject are represented as Input Values which describe their type and \
      \optionally a default value."
  , fields =
    [ field "name" (nonNull string) fieldSpec
      { resolver = \_ InputValueDefinition{name} ->
          pure name
      }
    , field "description" string fieldSpec
      { resolver = \_ InputValueDefinition{description} ->
          pure . nonEmptyToNothing $ Just description
      }
    , field "type" (nonNull __type) fieldSpec
      { resolver = \_ InputValueDefinition{_type} ->
          pure $ map fromInputType _type
      }
    , field "defaultValue" string fieldSpec
      { resolver = \_ InputValueDefinition{defaultValue} ->
          pure $ map Encoder.value defaultValue
      }
    ]
  }


asNamedType :: AnnotatedType t -> Maybe t
asNamedType (TypeNamed t) = Just t
asNamedType _ = Nothing


nonEmptyToNothing :: Maybe Text -> Maybe Text
nonEmptyToNothing (Just "") = Nothing
nonEmptyToNothing t = t


includeDeprecatedField :: Bool -> FieldDefinition -> Bool
includeDeprecatedField True _ =
  True
includeDeprecatedField False FieldDefinition{deprecationReason} =
  isNothing deprecationReason
