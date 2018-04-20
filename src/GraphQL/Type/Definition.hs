module GraphQL.Type.Definition
  ( Definition(..)
  , ScalarDefinition
  , ObjectDefinition
  , InterfaceDefinition
  , UnionDefinition
  , EnumDefinition
  , InputObjectDefinition
  , boolean
  , float
  , id
  , int
  , listOf
  , nonNull
  , string
  ) where

import Protolude

import Data.Maybe (Maybe(..))
import Data.Row (Row, Rec, Var)
import GHC.Types (Type)
import GraphQL.Type.Kind (TypeKind(..))
import qualified GraphQL.Type.Object as Object
import qualified GraphQL.Type.Scalar as Scalar


data Definition (kind :: TypeKind) (m :: Type -> Type) (value :: Type) where
  ScalarDefinition
    :: Scalar.Definition value
    -> Definition 'SCALAR m (Maybe value)
  ObjectDefinition
    :: Object.Definition m value 
    -> Definition 'OBJECT m (Maybe value)
  InterfaceDefinition
    :: Object.Interface m row
    -> Definition 'INTERFACE m (Maybe (Var row))
  UnionDefinition
    :: Object.Union m row
    -> Definition 'UNION m (Maybe (Var row))
  EnumDefinition
    :: EnumSpec value
    -> Definition 'ENUM m (Maybe value)
  InputObjectDefinition
    :: InputObjectSpec row
    -> Definition 'INPUT_OBJECT m (Maybe (Rec row))
  ListDefinition
    :: Definition kind m value
    -> Definition ('LIST_OF kind) m (Maybe [value])
  NonNullDefinition
    :: Definition kind m (Maybe value)
    -> Definition ('NON_NULL kind) m value


type ScalarDefinition m value = Definition 'SCALAR m (Maybe value)

type ObjectDefinition m value = Definition 'OBJECT m (Maybe value)

type InterfaceDefinition m row = Definition 'INTERFACE m (Maybe (Var row))

type UnionDefinition m row = Definition 'UNION m (Maybe (Var row))

type EnumDefinition m value = Definition 'ENUM m (Maybe value)

type InputObjectDefinition m row = Definition 'INPUT_OBJECT m (Maybe (Rec row))


listOf :: Definition kind m value -> Definition (LIST_OF kind) m (Maybe [value])
listOf = ListDefinition


nonNull :: Definition kind m (Maybe value) -> Definition (NON_NULL kind) m value
nonNull = NonNullDefinition


int :: ScalarDefinition m Int32
int = ScalarDefinition Scalar.int


float :: ScalarDefinition m Double
float = ScalarDefinition Scalar.float


string :: ScalarDefinition m Text
string = ScalarDefinition Scalar.string


boolean :: ScalarDefinition m Bool
boolean = ScalarDefinition Scalar.boolean


id :: ScalarDefinition m Scalar.Id
id = ScalarDefinition Scalar.id


data EnumSpec value = EnumSpec
  { name :: Text
  , description :: Text
  }


data InputObjectSpec (row :: Row Type) = InputObjectSpec
  { name :: Text
  , description :: Text
  }
