module GraphQL.Type.Scalar
  ( Definition(..)
  , Id(..)
  , toSchemaDefinition
  , int
  , float
  , string
  , boolean
  , id
  ) where


import Protolude 
import qualified GraphQL.Language.AST as AST (Value(..))
import qualified GraphQL.Type.Schema as Schema


data Definition value = Definition
  { name :: Text
  , description :: Text
  , serialize :: value -> AST.Value
  , deserialize :: AST.Value -> Maybe value
  }


toSchemaDefinition :: Definition value -> Schema.ScalarTypeDefinition
toSchemaDefinition Definition{ name, description } =
  Schema.ScalarTypeDefinition { name, description }


newtype Id = Id Text


int :: Definition Int32
int =
  Definition
    { name =
        "Int"
    , description =
        "The `Int` scalar type represents non-fractional signed whole numeric \
        \values. Int can represent values between -(2^31) and 2^31 - 1"
    , serialize =
        AST.ValueInt
    , deserialize =
        \case
          AST.ValueInt v -> Just v
          _ -> Nothing
    }


float :: Definition Double
float =
  Definition
    { name =
        "Double"
    , description =
        "The `Float` scalar type represents signed double-precision fractional \
        \values as specified by \
        \[IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point). "
    , serialize =
        AST.ValueFloat
    , deserialize =
        \case
          AST.ValueInt v -> Just (fromIntegral v)
          AST.ValueFloat v -> Just v
          _ -> Nothing
    }


string :: Definition Text
string =
  Definition
    { name =
        "String"
    , description =
        "The `String` scalar type represents textual data, represented as UTF-8 \
        \character sequences. The String type is most often used by GraphQL to \
        \represent free-form human-readable text."
    , serialize =
        AST.ValueString
    , deserialize =
        \case
          AST.ValueString v -> Just v
          _ -> Nothing
    }


boolean :: Definition Bool
boolean =
  Definition
    { name =
        "Boolean"
    , description =
        "The `Boolean` scalar type represents `true` or `false`"
    , serialize =
        AST.ValueBoolean
    , deserialize =
        \case
          AST.ValueBoolean v -> Just v
          _ -> Nothing
    }


id :: Definition Id
id =
  Definition
    { name =
        "ID"
    , description =
        "The `ID` scalar type represents a unique identifier, often used to \
        \refetch an object or as a key for a cache. The ID type appears in a JSON \
        \response as a String; however, it is not intended to be human-readable. \
        \When expected as an input type, any string (such as `\"4\"`) or integer \
        \(such as `4`) input value will be accepted as an ID."
    , serialize =
        \(Id v) -> AST.ValueString v
    , deserialize =
        \case
          AST.ValueString v -> Just (Id v)
          AST.ValueInt v -> Just (Id (show v))
          _ -> Nothing
    }
