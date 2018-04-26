module GraphQL.Type.Object
  ( Definition(..)
  , Field(..)
  , FieldContext(..)
  , fieldsFromList
  , toSchemaDefinition
  , resolveObject
  , InterfaceFor(..)
  , Interface(..)
  , Union(..)
  ) where

import Protolude

import qualified Data.Map as Map (elems, findWithDefault, fromList, lookup)
import Data.Row (Label, Rec, Row, KnownSymbol)
import qualified Data.Row.Internal as Row
import GraphQL.Execution.Context (ExecutionContext(..))
import GraphQL.Execution.Result (Result, ExecutionError(..), resultValidated, resultObject, resultValue)
import qualified GraphQL.Language.AST as AST
import qualified GraphQL.Type.InputValue as InputValue
import qualified GraphQL.Type.Schema as Schema
import GraphQL.Validation.Schema (satisfiesTypeCondition)
import qualified GraphQL.Validation.Validator as Validator


data Definition m value =
  Definition
    { name :: Text
    , description :: Text
    , interfaces :: [InterfaceFor m value]
    , fields :: Map Text (Field m value)
    }


data Field m context =
  forall (args :: Row Type).
    Field
      { name :: Text
      , description :: Text
      , deprecation :: Maybe Text
      , definition :: Schema.OutputType
      , arguments :: InputValue.Collection args
      , resolver :: FieldContext context args -> m Result
      }

data FieldContext context args =
  FieldContext
    { parentContext :: ExecutionContext context
    , arguments :: Rec args
    , directives :: [AST.Directive]
    , responseKey :: AST.Name
    , subSelection :: AST.SelectionSet
    }


fieldsFromList :: forall m value. [Field m value] -> Map Text (Field m value)
fieldsFromList = Map.fromList . map (\f@Field{name} -> (name, f))


toSchemaDefinition :: Definition m value -> Schema.ObjectTypeDefinition
toSchemaDefinition Definition{..} =
  Schema.ObjectTypeDefinition
    { name
    , description
    , interfaces = []
    , fields = map toFieldDefinition . Map.elems $ fields
    }


toFieldDefinition :: Field m context -> Schema.FieldDefinition
toFieldDefinition Field{..} =
  Schema.FieldDefinition
    { name
    , description
    , args = InputValue.toSchemaDefinitions arguments
    , _type = definition
    , deprecationReason = deprecation
    }


resolveObject
  :: forall m value
   . (Applicative m)
  => Definition m value
  -> ExecutionContext value
  -> m Result
resolveObject Definition{..} ctx@ExecutionContext{..} =
    map resultObject
    $ for (collectFields selectionSet) resolveField
  where
    collectFields :: AST.SelectionSet -> [AST.Field]
    collectFields = concatMap $ \case
      AST.SelectionField f ->
        [f]
      AST.SelectionFragmentSpread (AST.FragmentSpread fragmentName directives) ->
        case Map.lookup fragmentName fragmentDefinitions of
          Nothing -> []
          Just (AST.FragmentDefinition _ typeCondition directives selectionSet)
            | satisfiesTC typeCondition -> collectFields selectionSet
            | otherwise -> []
      AST.SelectionInlineFragment (AST.InlineFragment Nothing directives ss) ->
        collectFields ss
      AST.SelectionInlineFragment (AST.InlineFragment (Just typeCondition) directives ss)
        | satisfiesTC typeCondition -> collectFields ss
        | otherwise -> []

    satisfiesTC :: AST.TypeCondition -> Bool
    satisfiesTC = satisfiesTypeCondition validatedSchema name

    resolveField :: AST.Field -> m (AST.Name, Result)
    resolveField (AST.Field alias fieldName _ _ _) | AST.unName fieldName == "__typename" =
      pure (fromMaybe fieldName alias, resultValue $ AST.ValueString name)
    resolveField (AST.Field alias fieldName fieldArgs directives subSelection) =
        map (responseKey, ) . resultValidated $ do
          Field{ arguments, resolver } <- case Map.lookup (AST.unName fieldName) fields of
            Just f -> pure f
            Nothing ->
              Validator.throw FieldNotDefined
                { fieldName = AST.unName fieldName
                , objectTypeName = name
                }
          resolvedArgs <- InputValue.resolveArguments arguments (substArgs fieldArgs)
          pure $ resolver FieldContext
            { parentContext = ctx
            , directives
            , arguments = resolvedArgs
            , responseKey = responseKey
            , subSelection
            }
      where
        responseKey = fromMaybe fieldName alias


    -- TODO: consider performance here, this is likely a lot of redundant work
    substArgs :: [AST.Argument] -> [AST.Argument]
    substArgs = map (\(AST.Argument name value) -> AST.Argument name (substValue value))

    substValue :: AST.Value -> AST.Value
    substValue (AST.ValueVariable (AST.Variable v)) =
      Map.findWithDefault AST.ValueNull v variableValues
    substValue (AST.ValueList (AST.ListValue values)) =
      AST.ValueList . AST.ListValue $ map substValue values
    substValue (AST.ValueObject (AST.ObjectValue fields)) =
      AST.ValueObject . AST.ObjectValue $ map substField fields
    substValue v = v

    substField :: AST.ObjectField -> AST.ObjectField
    substField (AST.ObjectField name value) =
      AST.ObjectField name (substValue value)


-- TODO: clean this up
-- likely the best thing to do is shove all the types into Definition.hs


data InterfaceFor m value =
  forall label row. (KnownSymbol label, Row.HasType label value row) =>
    InterfaceFor (Label label) (Interface m row)

data Interface m r =
  Interface
    { name :: Text
    , description :: Text
    , possibleTypes :: Rec (Row.Map (Definition m) r)
    , interfaceFields :: [Text]
    }

data Union m r =
  Union
    { name :: Text
    , description :: Text
    , possibleTypes :: Rec (Row.Map (Definition m) r)
    }
