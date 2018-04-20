module GraphQL.Execution.Result
  ( Result
  , ExecutionError(..)
  , resultValidated
  , resultValue
  , resultObject
  , resultList
  , encode
  ) where

import Protolude

import qualified Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Text as Text (intercalate)
import qualified GraphQL.Validation.Validator as Validator
import qualified GraphQL.Language.AST as AST
import qualified GraphQL.Language.Encoder as Encoder (value)
import qualified GraphQL.Type.Schema as Schema


data Result =
  Result
    { errors :: [ExecutionError]
    , value :: AST.Value
    }


resultValidated :: Applicative f => Validator.M ExecutionError (f Result) -> f Result
resultValidated v =
  case Validator.run v of
    Left errs -> pure Result{ errors = NonEmpty.toList errs, value = AST.ValueNull }
    Right validated -> validated


resultValue :: AST.Value -> Result
resultValue value = Result { errors = [], value }


resultObject :: [(AST.Name, Result)] -> Result
resultObject results =
    Result
      { errors = concatMap (errors . snd) results
      , value = AST.ValueObject . AST.ObjectValue . map getValue $ results
      }
  where
    getValue (name, Result{ value }) = AST.ObjectField name value


resultList :: [Result] -> Result
resultList results =
    Result
      { errors = concatMap errors results
      , value = AST.ValueList . AST.ListValue . map getValue $ results
      }
  where
    getValue (Result{ value }) = value


encode :: Result -> Text
encode Result{ errors, value } =
  case errors of
    [] ->
      "{ \"data\": " <> Encoder.value value <> " }"
    _ ->
      "{ \"data\": " <> Encoder.value value
      <> ", \"errors\": [" <> Text.intercalate "," (map encodeError errors) <> "]"
      <> " }"


encodeError :: ExecutionError -> Text
encodeError _ = "\"TODO\""



data ExecutionError
  = ArgumentInvalid
      { argumentName :: Text
      , expectedInputType :: Schema.InputType
      , value :: AST.Value
      }
  | ArgumentNonUnique
      { argumentName :: Text
      , value1 :: AST.Value
      , value2 :: AST.Value
      }
  | ArgumentMissing
      { argumentName :: Text
      , expectedInputType :: Schema.InputType
      }
  | FieldNotDefined
      { fieldName :: Text
      , objectTypeName :: Text
      }
  | NotYetImplemented
