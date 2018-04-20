{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module GraphQL.Type.Schema
  ( Schema(..)
  , TypeDefinition(..)
  , OutputTypeDefinition(..)
  , OutputType
  , InputTypeDefinition(..)
  , InputType
  , AnnotatedType(..)
  , ListType(..)
  , NonNullType(..)
  , ObjectTypeDefinition(..)
  , ScalarTypeDefinition(..)
  , InterfaceTypeDefinition(..)
  , UnionTypeDefinition(..)
  , EnumTypeDefinition(..)
  , InputObjectTypeDefinition(..)
  , FieldDefinition(..)
  , InputValueDefinition(..)
  , getTypeName
  , getTypeDescription
  , getNamedType
  , fromOutputType
  , fromInputType
  ) where

import Protolude

import GraphQL.Type.Kind (TypeKind(..))
import qualified GraphQL.Language.AST as AST (Value)


data Schema = Schema
  { rootQueryType :: ObjectTypeDefinition
  , rootMutationType :: Maybe ObjectTypeDefinition
  , additionalTypes :: [TypeDefinition]
  }


data TypeDefinition
  = TypeDefinitionScalar ScalarTypeDefinition
  | TypeDefinitionObject ObjectTypeDefinition
  | TypeDefinitionInterface InterfaceTypeDefinition
  | TypeDefinitionUnion UnionTypeDefinition
  | TypeDefinitionEnum EnumTypeDefinition
  | TypeDefinitionInputObject InputObjectTypeDefinition


data OutputTypeDefinition
  = OutputTypeDefinitionScalar ScalarTypeDefinition
  | OutputTypeDefinitionObject ObjectTypeDefinition
  | OutputTypeDefinitionInterface InterfaceTypeDefinition
  | OutputTypeDefinitionUnion UnionTypeDefinition
  | OutputTypeDefinitionEnum EnumTypeDefinition

type OutputType = AnnotatedType OutputTypeDefinition


data InputTypeDefinition
  = InputTypeDefinitionScalar ScalarTypeDefinition
  | InputTypeDefinitionEnum EnumTypeDefinition
  | InputTypeDefinitionInputObject InputObjectTypeDefinition

type InputType = AnnotatedType InputTypeDefinition


data AnnotatedType t
  = TypeNamed t
  | TypeList (ListType t)
  | TypeNonNull (NonNullType t)
  deriving (Eq, Show, Functor)

newtype ListType t
  = ListType (AnnotatedType t)
  deriving (Eq, Show, Functor)

data NonNullType t
  = NonNullTypeNamed t
  | NonNullTypeList (ListType t)
  deriving (Eq, Show, Functor)


data ScalarTypeDefinition = ScalarTypeDefinition
  { name :: Text
  , description :: Text
  } deriving (Eq)


data ObjectTypeDefinition = ObjectTypeDefinition
  { name :: Text
  , description :: Text
  , interfaces :: [InterfaceTypeDefinition]
  , fields :: [FieldDefinition]
  }


data InterfaceTypeDefinition = InterfaceTypeDefinition
  { name :: Text
  , description :: Text
  , fields :: [FieldDefinition]
  , possibleTypes :: [ObjectTypeDefinition]
  }


data UnionTypeDefinition = UnionTypeDefinition
  { name :: Text
  , description :: Text
  , possibleTypes :: [ObjectTypeDefinition]
  }


data EnumTypeDefinition = EnumTypeDefinition
  { name :: Text
  , description :: Text
  , values :: [EnumValueDefinition]
  } deriving (Eq)


data InputObjectTypeDefinition = InputObjectTypeDefinition
  { name :: Text
  , description :: Text
  , inputFields :: [InputValueDefinition]
  }


data FieldDefinition = FieldDefinition
  { name :: Text
  , description :: Text
  , args :: [InputValueDefinition]
  , _type :: OutputType
  , deprecationReason :: Maybe Text
  }


data InputValueDefinition = InputValueDefinition
  { name :: Text
  , description :: Text
  , _type :: InputType
  , defaultValue :: Maybe AST.Value
  }


data EnumValueDefinition = EnumValueDefinition
  { name :: Text
  , description :: Text
  , deprecationReason :: Maybe Text
  } deriving (Eq)


getTypeName :: TypeDefinition -> Text
getTypeName = \case
  TypeDefinitionScalar ScalarTypeDefinition{..} -> name
  TypeDefinitionObject ObjectTypeDefinition{..} -> name
  TypeDefinitionInterface InterfaceTypeDefinition{..} -> name
  TypeDefinitionUnion UnionTypeDefinition{..} -> name
  TypeDefinitionEnum EnumTypeDefinition{..} -> name
  TypeDefinitionInputObject InputObjectTypeDefinition{..} -> name


getTypeDescription :: TypeDefinition -> Text
getTypeDescription = \case
  TypeDefinitionScalar ScalarTypeDefinition{..} -> description
  TypeDefinitionObject ObjectTypeDefinition{..} -> description
  TypeDefinitionInterface InterfaceTypeDefinition{..} -> description
  TypeDefinitionUnion UnionTypeDefinition{..} -> description
  TypeDefinitionEnum EnumTypeDefinition{..} -> description
  TypeDefinitionInputObject InputObjectTypeDefinition{..} -> description


getNamedType :: AnnotatedType t -> t
getNamedType (TypeNamed t) = t
getNamedType (TypeList (ListType ann)) = getNamedType ann
getNamedType (TypeNonNull (NonNullTypeNamed t)) = t
getNamedType (TypeNonNull (NonNullTypeList (ListType ann))) = getNamedType ann


fromOutputType :: OutputTypeDefinition -> TypeDefinition
fromOutputType (OutputTypeDefinitionScalar def) = TypeDefinitionScalar def
fromOutputType (OutputTypeDefinitionObject def) = TypeDefinitionObject def
fromOutputType (OutputTypeDefinitionInterface def) = TypeDefinitionInterface def
fromOutputType (OutputTypeDefinitionUnion def) = TypeDefinitionUnion def
fromOutputType (OutputTypeDefinitionEnum def) = TypeDefinitionEnum def


fromInputType :: InputTypeDefinition -> TypeDefinition
fromInputType (InputTypeDefinitionScalar def) = TypeDefinitionScalar def
fromInputType (InputTypeDefinitionEnum def) = TypeDefinitionEnum def
fromInputType (InputTypeDefinitionInputObject def) = TypeDefinitionInputObject def
