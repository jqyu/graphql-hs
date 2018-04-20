-- this module is intended to be imported qualified:
--
-- import qualified GraphQL.Execution.Resolve as Resolve
module GraphQL.Execution.Resolve
  ( Output(..)
  , Input(..)
  ) where

import Protolude hiding (TypeError, Text)

import GHC.TypeLits (TypeError(..), ErrorMessage(..))
import GraphQL.Execution.Context (ExecutionContext(..), ResponseSegment(..))
import GraphQL.Execution.Result (Result, resultValue, resultList)
import qualified GraphQL.Language.AST as AST
import GraphQL.Type.Definition (Definition(..))
import GraphQL.Type.Kind (TypeKind(..))
import qualified GraphQL.Type.Scalar as Scalar
import qualified GraphQL.Type.Object as Object
import qualified GraphQL.Type.Schema as Schema


-- We cannot output certain kinds, such as InputObjects and lists/non-nulls of said kinds
class Output kind m where
  outputType :: Definition kind m value -> Schema.OutputType
  output :: Definition kind m value -> ExecutionContext value -> m Result 

-- We can only receive Scalars, InputObjects, and list/non-nulls of said kinds
class Input kind where
  inputType :: Definition kind m value -> Schema.InputType
  deserialize :: Definition kind m value -> AST.Value -> Maybe value
  serialize :: Definition kind m value -> value -> AST.Value


-- SCALAR 

instance Applicative m => Output 'SCALAR m where
  outputType (ScalarDefinition (Scalar.Definition { name, description })) =
    Schema.TypeNamed
    . Schema.OutputTypeDefinitionScalar
    $ Schema.ScalarTypeDefinition { name, description }
  output (ScalarDefinition (Scalar.Definition { serialize })) ExecutionContext{ value } =
    pure
    . resultValue 
    $ case value of
        Just v -> serialize v
        _ -> AST.ValueNull


instance Input 'SCALAR where
  inputType (ScalarDefinition def) =
    Schema.TypeNamed
    . Schema.InputTypeDefinitionScalar
    $ Scalar.toSchemaDefinition def 
  deserialize (ScalarDefinition (Scalar.Definition { deserialize, serialize })) value =
    case value of
      AST.ValueNull -> Just Nothing
      _ -> Just <$> deserialize value
  serialize (ScalarDefinition (Scalar.Definition { deserialize, serialize })) =
    maybe AST.ValueNull serialize


-- OBJECT

instance Applicative m => Output 'OBJECT m where
  outputType (ObjectDefinition def) =
      Schema.TypeNamed
      . Schema.OutputTypeDefinitionObject
      $ Object.toSchemaDefinition def
  output (ObjectDefinition def) ctx@ExecutionContext { value } =
    case value of
      Nothing -> pure . resultValue $ AST.ValueNull 
      Just v -> Object.resolveObject def ctx { value = v }


-- NON_NULL

instance (Applicative m, Output kind m) => Output ('NON_NULL kind) m where
  outputType (NonNullDefinition def) =
    Schema.TypeNonNull
    $ case outputType def of
        Schema.TypeNamed t -> Schema.NonNullTypeNamed t
        Schema.TypeList t -> Schema.NonNullTypeList t
        Schema.TypeNonNull t -> t
  output (NonNullDefinition def) ctx@ExecutionContext{ value } =
    output def ctx { value = Just value }

instance Input kind => Input ('NON_NULL kind) where
  inputType (NonNullDefinition def) =
    Schema.TypeNonNull
    $ case inputType def of
        Schema.TypeNamed t -> Schema.NonNullTypeNamed t
        Schema.TypeList t -> Schema.NonNullTypeList t
        Schema.TypeNonNull t -> t
  deserialize (NonNullDefinition def) value =
    case value of
      AST.ValueNull -> Nothing
      _ -> join (deserialize def value)
  serialize (NonNullDefinition def) =
    serialize def . Just


-- LIST_OF

instance (Applicative m, Output kind m) => Output ('LIST_OF kind) m where
  outputType (ListDefinition def) =
    Schema.TypeList (Schema.ListType (outputType def))
  output (ListDefinition def) ctx@ExecutionContext{ value, path } =
    case value of
      Nothing -> pure $ resultValue AST.ValueNull
      Just values ->
        map resultList . for (zip [0..] values) $ \(idx, v) ->
          output def ctx
            { value = v
            , path = ResponseListIndex idx : path 
            }

instance Input kind => Input ('LIST_OF kind) where
  inputType (ListDefinition def) =
    Schema.TypeList (Schema.ListType (inputType def))
  deserialize (ListDefinition def) value =
    case value of
      AST.ValueNull ->
        Just Nothing
      AST.ValueList (AST.ListValue values) ->
        Just <$> traverse (deserialize def) values
      _ ->
        Nothing
  serialize (ListDefinition def) =
    maybe AST.ValueNull
    $ AST.ValueList
    . AST.ListValue
    . map (serialize def)
