module GraphQL.Type.InputValue
  ( Definition(..)
  , Collection(..)
  , toSchemaDefinitions
  , resolveArguments
  , resolveInputObject
  ) where


import Protolude

import qualified Data.Map as Map (fromList, findWithDefault)
import Data.Row (Empty, Label, Row, Rec, KnownSymbol, type (.+), type (.==), (.+), (.==), labels)
import qualified Data.Row.Records as Rec (empty)
import GHC.Types (Type)
import GraphQL.Execution.Result (ExecutionError(..))
import qualified GraphQL.Language.AST as AST
import qualified GraphQL.Type.Schema as Schema
import qualified GraphQL.Validation.Validator as Validator


data Definition value =
  Definition
    { description :: Text
    , definition :: Schema.InputType
    , defaultValue :: Maybe value
    , defaultGraphQLValue :: Maybe AST.Value
    , resolver :: AST.Value -> Maybe value
    }


data Collection (args :: Row Type) where
  None
    :: Collection Empty
  Merge
    :: Collection l
    -> Collection r
    -> Collection (l .+ r)
  Single
    :: (KnownSymbol sym)
    => Label sym
    -> Definition a
    -> Collection (sym .== a)


toSchemaDefinitions :: Collection args -> [Schema.InputValueDefinition]
toSchemaDefinitions None =
  []
toSchemaDefinitions (Merge l r) =
  toSchemaDefinitions l <> toSchemaDefinitions r
toSchemaDefinitions (Single label Definition{..}) =
  [ Schema.InputValueDefinition
    { name = show label
    , description
    , _type = definition
    , defaultValue = defaultGraphQLValue
    }
  ]



-- n.b. certain checks such as missing or duplicate arguments
-- are performed when the entire document is validated

resolveArguments
  :: Collection args
  -> [AST.Argument]
  -> Validator.M ExecutionError (Rec args)
resolveArguments def =
    resolveCollectionValues def
    . Map.fromList
    . map toAssoc
  where
    toAssoc (AST.Argument name value) =
      (AST.unName name, value)


resolveInputObject
  :: Collection args
  -> AST.ObjectValue
  -> Validator.M ExecutionError (Rec args)
resolveInputObject def =
  resolveCollectionValues def
    . Map.fromList
    . map toAssoc
    . unwrap
  where
    toAssoc (AST.ObjectField name value) = (AST.unName name, value)
    unwrap (AST.ObjectValue fields) = fields


resolveCollectionValues
  :: Collection args
  -> Map Text AST.Value
  -> Validator.M ExecutionError (Rec args)
resolveCollectionValues None _ =
  pure Rec.empty
resolveCollectionValues (Merge lhs rhs) values =
  (.+)
  <$> resolveCollectionValues lhs values
  <*> resolveCollectionValues rhs values
resolveCollectionValues (Single label (Definition{..})) values =
    case resolver value of
      Just v -> pure (label .== v)
      Nothing ->
        Validator.throw ArgumentInvalid
          { argumentName
          , expectedInputType = definition
          , value
          }
  where
    argumentName = show label
    value = Map.findWithDefault AST.ValueNull argumentName values
