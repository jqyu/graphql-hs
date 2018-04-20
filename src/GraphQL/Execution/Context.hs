module GraphQL.Execution.Context
  ( ExecutionContext(..)
  , ResponsePath
  , ResponseSegment(..)
  ) where


import Protolude

import GraphQL.Validation.Schema (ValidatedSchema) 
import qualified GraphQL.Language.AST as AST


data ExecutionContext value =
  ExecutionContext
    { validatedSchema :: ValidatedSchema
    , variableValues :: Map AST.Name AST.Value
    , fragmentDefinitions :: Map AST.Name AST.FragmentDefinition
    , path :: ResponsePath 
    , value :: value
    , selectionSet :: AST.SelectionSet
    }


type ResponsePath = [ResponseSegment]

data ResponseSegment
  = ResponseKey AST.Name
  | ResponseListIndex Int
