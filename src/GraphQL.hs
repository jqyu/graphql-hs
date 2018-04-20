module GraphQL
  ( ValidatedSchema
  , Spec(..)
  , makeSchema
  -- , ExecutionParams(..)
  -- , executeQuery
  , ExecutionError
  , handleRequest
  ) where

import Protolude

import qualified Data.Attoparsec.Text as Attoparsec (parseOnly, endOfInput)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified GraphQL.Execution.Resolve as Resolver
import GraphQL.Execution.Context (ExecutionContext(..))
import GraphQL.Execution.Result (Result, resultValue)
import qualified GraphQL.Language.AST as AST
import qualified GraphQL.Language.Parser as Parser (queryDocument)
import GraphQL.Type.Definition (Definition(..), ObjectDefinition)
import GraphQL.Type.Kind (TypeKind(..))
import qualified GraphQL.Type.Schema as Unvalidated (Schema(..), TypeDefinition(..), ObjectTypeDefinition)
import qualified GraphQL.Type.Object as Object (toSchemaDefinition)
import GraphQL.Validation.Schema (ValidatedSchema, SchemaValidationError, validateSchema)
import GraphQL.Validation.Schema.TypeDefinition (withIntrospectionFields, __schema, __type)
import qualified GraphQL.Validation.Validator as Validator 


data Spec m query mutation = Spec
  { rootQueryType :: ObjectDefinition m query
  , rootMutationType :: Maybe (ObjectDefinition m mutation)
  }


makeSchema
  :: forall m query mutation. Monad m
  => Spec m query mutation
  -> Validator.M SchemaValidationError ValidatedSchema
makeSchema Spec{..} =
  validateSchema Unvalidated.Schema
    { rootQueryType =
        toObjectTypeDefinition rootQueryType
    , rootMutationType =
        map toObjectTypeDefinition rootMutationType
    , additionalTypes =
      [ Unvalidated.TypeDefinitionObject (toObjectTypeDefinition (__schema @m))
      , Unvalidated.TypeDefinitionObject (toObjectTypeDefinition (__type @m))
      ]
    }


toObjectTypeDefinition :: ObjectDefinition m value -> Unvalidated.ObjectTypeDefinition
toObjectTypeDefinition (ObjectDefinition def) =
  Object.toSchemaDefinition def


data ExecutionError
  = SchemaValidationError SchemaValidationError
  | DocumentParsingError Text
  | DocumentValidationError
  | OperationNotFound (Maybe Text)
  | JSONParsingError



data RequestParams = RequestParams
  { operationName :: Maybe Text
  , query :: Text
  , variableValues :: Maybe (Map Text Text)
  } deriving (Generic)

instance Aeson.FromJSON RequestParams


handleRequest
  :: Monad m
  => Spec m query mutation
  -> LByteString
  -> query
  -> Validator.M ExecutionError (m Result)
handleRequest spec@Spec{rootQueryType} requestBody rootObject = do
  validatedSchema <- first SchemaValidationError (makeSchema spec)
  RequestParams{..} <- Validator.note JSONParsingError (Aeson.decode requestBody)
  queryDocument <- Validator.fromEither
    . first (DocumentParsingError . toS)
    . Attoparsec.parseOnly (Parser.queryDocument <* Attoparsec.endOfInput)
    $ query
  operation <- Validator.note (OperationNotFound operationName) (extractOperation operationName queryDocument)
  let context =
        ExecutionContext
          { validatedSchema
          , variableValues = Map.empty
          , fragmentDefinitions = extractFragments queryDocument
          , path = []
          , value = Just rootObject
          , selectionSet =
              case operation of
                AST.Query (AST.Node _ _ _ ss) -> ss
                AST.Mutation (AST.Node _ _ _ ss) -> ss
                AST.AnonymousQuery ss -> ss
          }
  pure (Resolver.output (withIntrospectionFields validatedSchema rootQueryType) context)


extractOperation :: Maybe Text -> AST.QueryDocument -> Maybe AST.OperationDefinition
extractOperation name document =
    case getOperations (AST.getDefinitions document) of
      [op] -> Just op
      ops -> find isSelectedOperation ops
  where
    getOperations = concatMap $ \case
      AST.DefinitionOperation op -> [op]
      _ -> []

    isSelectedOperation = \case
      AST.Query (AST.Node queryName _ _ _) ->
        map AST.unName queryName == name
      AST.Mutation (AST.Node mutationName _ _ _) ->
        map AST.unName mutationName == name
      AST.AnonymousQuery _ ->
        isNothing name
      

extractFragments :: AST.QueryDocument -> Map AST.Name AST.FragmentDefinition
extractFragments =
    Map.fromList . concatMap toFragmentAssoc . AST.getDefinitions
  where
    toFragmentAssoc (AST.DefinitionFragment f@(AST.FragmentDefinition name _ _ _)) =
      [(name, f)]
    toFragmentAssoc _ =
      []
