module GraphQL.Validation.Schema
  ( ValidatedSchema
  , getRootQueryType
  , getRootMutationType
  , getTypeDefinitions
  , lookupTypeDefinition
  , satisfiesTypeCondition
  , SchemaValidationError
  , validateSchema
  ) where

import Protolude

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GraphQL.Language.AST as AST
import GraphQL.Type.Schema
import qualified GraphQL.Validation.Validator as Validator
import qualified GraphQL.Validation.Schema.Shallow as Shallow
import qualified GraphQL.Validation.Schema.Rules.FieldsEmpty as FieldsEmpty
import qualified GraphQL.Validation.Schema.Rules.FieldsUnique as FieldsUnique
import qualified GraphQL.Validation.Schema.Rules.TypeName as TypeName


data ValidatedSchema = ValidatedSchema
  { rootQueryType :: ObjectTypeDefinition
  , rootMutationType :: Maybe ObjectTypeDefinition
  , typeDefinitions :: Map Text TypeDefinition
  , typeConditions :: Map Text (Set Text)
  }


getRootQueryType :: ValidatedSchema -> ObjectTypeDefinition
getRootQueryType ValidatedSchema{..} = rootQueryType


getRootMutationType :: ValidatedSchema -> Maybe ObjectTypeDefinition
getRootMutationType ValidatedSchema{..} = rootMutationType


getTypeDefinitions :: ValidatedSchema -> [TypeDefinition]
getTypeDefinitions ValidatedSchema{..} = Map.elems typeDefinitions


lookupTypeDefinition :: ValidatedSchema -> Text -> Maybe TypeDefinition
lookupTypeDefinition ValidatedSchema{..} typeName =
  Map.lookup typeName typeDefinitions


satisfiesTypeCondition :: ValidatedSchema -> Text -> AST.TypeCondition -> Bool
satisfiesTypeCondition ValidatedSchema{..} typeName (AST.NamedType (AST.Name typeCondition)) =
  Set.member typeCondition (Map.findWithDefault Set.empty typeName typeConditions)


data SchemaValidationError
  = FieldsEmpty FieldsEmpty.Error
  | FieldsUnique FieldsUnique.Error
  | TypeName TypeName.Error


validateSchema :: Schema -> Validator.M SchemaValidationError ValidatedSchema
validateSchema Schema{..} = do
  let initialTypes =
        [TypeDefinitionObject rootQueryType]
        ++ map TypeDefinitionObject (maybeToList rootMutationType)
        ++ additionalTypes
  let typeDefinitions =
        collectNamedTypes Map.empty initialTypes
  traverse validateTypeDefinition typeDefinitions
  let typeConditions =
        foldMap collectTypeConditions typeDefinitions
  pure ValidatedSchema
    { rootQueryType
    , rootMutationType
    , typeDefinitions
    , typeConditions
    }


collectTypeConditions :: TypeDefinition -> Map Text (Set Text)
collectTypeConditions = \case
  TypeDefinitionObject ObjectTypeDefinition{name} ->
    Map.singleton name (Set.singleton name)
  TypeDefinitionInterface InterfaceTypeDefinition{name = interfaceName, possibleTypes} ->
    flip foldMap possibleTypes $ \ObjectTypeDefinition{name = objectName} ->
      Map.singleton objectName (Set.singleton interfaceName)
  TypeDefinitionUnion UnionTypeDefinition{name = unionName, possibleTypes} ->
    flip foldMap possibleTypes $ \ObjectTypeDefinition{name = objectName} ->
      Map.singleton objectName (Set.singleton unionName)
  _ -> Map.empty


validateTypeDefinition :: TypeDefinition -> Validator.M SchemaValidationError ()
validateTypeDefinition def =
  Validator.fromErrors (concatMap ($ def) validationRules)


validationRules :: [TypeDefinition -> [SchemaValidationError]]
validationRules =
  [ map FieldsEmpty . FieldsEmpty.visit
  , map FieldsUnique . FieldsUnique.visit
  , map TypeName . TypeName.visit
  ]


-- TODO: check that types don't have conflicting names
collectNamedTypes :: Map Text TypeDefinition -> [TypeDefinition] -> Map Text TypeDefinition
collectNamedTypes =
    foldr collect
  where
    collect :: TypeDefinition -> Map Text TypeDefinition -> Map Text TypeDefinition
    collect typeDefinition collected =
      case insertNew typeDefinition collected of
        Just c ->
          collectNamedTypes c (Shallow.dependencies typeDefinition)
        Nothing ->
          collected

    insertNew :: TypeDefinition -> Map Text TypeDefinition -> Maybe (Map Text TypeDefinition)
    insertNew typeDefinition collected =
        case Map.lookup typeName collected of
          Nothing ->
            Just (Map.insert typeName typeDefinition collected)
          -- TODO: check that types don't have conflicting names
          Just _ ->
            Nothing
      where
        typeName = getTypeName typeDefinition
