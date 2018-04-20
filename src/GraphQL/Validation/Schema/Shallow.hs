-- shallow versions of schema type definitions to help break cyclical structures

module GraphQL.Validation.Schema.Shallow
  ( dependencies
  , equals
  ) where

import Protolude

import qualified GraphQL.Language.AST as AST
import GraphQL.Type.Schema


-- gets a list of TypeDefinitions 1 level deep
dependencies :: TypeDefinition -> [TypeDefinition]
dependencies = \case
  TypeDefinitionScalar _ ->
    []
  TypeDefinitionObject ObjectTypeDefinition{interfaces, fields} ->
    map TypeDefinitionInterface interfaces
    <> foldMap fieldDependencies fields
  TypeDefinitionInterface InterfaceTypeDefinition{fields, possibleTypes} ->
    foldMap fieldDependencies fields
    <> map TypeDefinitionObject possibleTypes
  TypeDefinitionUnion UnionTypeDefinition{possibleTypes} ->
    map TypeDefinitionObject possibleTypes
  TypeDefinitionEnum _ ->
    []
  TypeDefinitionInputObject InputObjectTypeDefinition{inputFields} ->
    foldMap inputValueDependencies inputFields

fieldDependencies :: FieldDefinition -> [TypeDefinition]
fieldDependencies FieldDefinition{args, _type} =
  [fromOutputType (getNamedType _type)]
  <> foldMap inputValueDependencies args

inputValueDependencies :: InputValueDefinition -> [TypeDefinition]
inputValueDependencies InputValueDefinition{_type} =
  [fromInputType (getNamedType _type)]


-- determines if two type definitions are equivalent, up to the names of
-- its dependencies, preventing non-termination for circular inputs

-- this check is not perfect but catches most practical cases 

-- to trick this function one would have to have to construct an input
equals :: TypeDefinition -> TypeDefinition -> Bool
equals def1 def2 = toShallow def1 == toShallow def2


-- this approach is very brittle with respect to changes to TypeDefinition
-- and really, TypeDefinition should be a functor with a fixed point

-- i suspsect that there's something clever to be done using an index type
-- similar to the way that GraphQL.Type.Definition works in order to create
-- a nice `TypeDefinitionF :: TypeKind -> Type -> Type` definition
data ShallowTypeDef
  = SScalar ScalarTypeDefinition
  | SObject
      { name :: Text
      , description :: Text
      , interfaces :: [Text]
      , fields :: [ShallowFieldDef]
      }
  | SInterface
      { name :: Text
      , description :: Text
      , fields :: [ShallowFieldDef]
      , possibleTypeNames :: [Text]
      }
  | SUnion
      { name :: Text
      , description :: Text
      , possibleTypeNames :: [Text]
      }
  | SEnum EnumTypeDefinition
  | SInputObject
      { name :: Text
      , description :: Text
      , inputFields :: [ShallowInputValueDef]
      }
  deriving (Eq)


data ShallowFieldDef =
  ShallowFieldDef
    { name :: Text
    , description :: Text
    , args :: [ShallowInputValueDef]
    , typeName :: AnnotatedType Text
    , deprecationReason :: Maybe Text
    } deriving (Eq)


data ShallowInputValueDef =
  ShallowInputValueDef
    { name :: Text
    , description :: Text
    , typeName :: AnnotatedType Text
    , defaultValue :: Maybe AST.Value
    } deriving (Eq)


toShallow :: TypeDefinition -> ShallowTypeDef
toShallow (TypeDefinitionScalar scalar) =
  SScalar scalar
toShallow (TypeDefinitionObject ObjectTypeDefinition{..}) =
  SObject
    { name
    , description
    , interfaces = map (getTypeName . TypeDefinitionInterface) interfaces
    , fields = map toShallowField fields
    }
toShallow (TypeDefinitionInterface InterfaceTypeDefinition{..}) =
  SInterface
    { name
    , description
    , fields = map toShallowField fields
    , possibleTypeNames = map (getTypeName . TypeDefinitionObject) possibleTypes
    }
toShallow (TypeDefinitionUnion UnionTypeDefinition{..}) =
  SUnion
    { name
    , description
    , possibleTypeNames = map (getTypeName . TypeDefinitionObject) possibleTypes
    }
toShallow (TypeDefinitionEnum enum) =
  SEnum enum
toShallow (TypeDefinitionInputObject InputObjectTypeDefinition{..}) =
  SInputObject
    { name
    , description
    , inputFields = map toShallowInputValue inputFields
    }


toShallowField :: FieldDefinition -> ShallowFieldDef
toShallowField FieldDefinition {..} =
  ShallowFieldDef
    { name
    , description
    , args = map toShallowInputValue args
    , typeName = map (getTypeName . fromOutputType) _type
    , deprecationReason
    }


toShallowInputValue :: InputValueDefinition -> ShallowInputValueDef
toShallowInputValue InputValueDefinition{..} =
  ShallowInputValueDef
    { name
    , description
    , typeName = map (getTypeName . fromInputType) _type
    , defaultValue
    }
