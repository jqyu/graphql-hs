{-# LANGUAGE AllowAmbiguousTypes #-}

-- a DSL for creating type definitions

module GraphQL.DSL
  ( ObjectSpec(..)
  , objectSpec
  , defineObject
  , FieldSpec(..)
  , field
  , fieldSpec
  , ArgSpec(..)
  , argSpec
  , arg
  , argWithDefault
  , (++++)
  , getArg
  -- reexported types
  , ScalarDefinition
  , ObjectDefinition
  , InterfaceDefinition
  , UnionDefinition
  , EnumDefinition
  , InputObjectDefinition
  , int
  , float
  , string
  , boolean
  , id
  , listOf
  , nonNull
  ) where


import Protolude

import Data.Row (Empty, Rec, Label(..), HasType, type (.+), type (.==), (.!))
import GraphQL.Execution.Context (ExecutionContext(..), ResponseSegment(..))
import GraphQL.Type.Definition
import qualified GraphQL.Execution.Resolve as Resolve
import qualified GraphQL.Language.AST as AST
import qualified GraphQL.Type.InputValue as InputValue
import qualified GraphQL.Type.Object as Object


interface
  :: forall label m value row. (KnownSymbol label, HasType label value row)
  => InterfaceDefinition m row 
  -> Label label
  -> Object.InterfaceFor m value
interface (InterfaceDefinition def) = flip Object.InterfaceFor def


-- Objects

-- foo :: ObjectDefinition IO Foo
-- foo = defineObject #Foo objectSpec
--   { interfaces =
--       [ interface node
--       , interface dummyInterface
--       ]
--   , description =
--       "Dummy object"
--   , fields =
--       [ field "id" (nonNull idType) fieldSpec
--           { resolver = \_ Foo{ id } ->
--               pure id
--           }
--       , ...
--       ]
--   }

data ObjectSpec m label context =
  ObjectSpec
    { description :: Text
    , interfaces :: [Label label -> Object.InterfaceFor m context]
    , fields :: [Object.Field m context]
    }


objectSpec :: ObjectSpec m label context
objectSpec =
  ObjectSpec
    { description = ""
    , interfaces = []
    , fields = []
    }


defineObject :: forall label m context. KnownSymbol label => ObjectSpec m label context -> ObjectDefinition m context
defineObject (ObjectSpec{..}) =
    ObjectDefinition $ Object.Definition
      { name = show (Label :: Label label)
      , description
      , interfaces = map ($ label) interfaces
      , fields = Object.fieldsFromList fields
      }
  where
    label = Label :: Label label



-- Fields

-- field "myField" (nonNull (listOf int)) fieldSpec
--   { description =
--       "my description"
--   , deprecation =
--       Just "don't use this field anymore"
--   , resolver =
--       \args root ->
--         pure (getMaybeInts root)
--   }


data FieldSpec m context args value =
  FieldSpec
    { description :: Text
    , deprecation :: Maybe Text 
    , arguments :: InputValue.Collection args
    , resolver :: Rec args -> context -> m value
    }


fieldSpec :: FieldSpec (Const ()) root Empty ()
fieldSpec = FieldSpec
  { description = ""
  , deprecation = Nothing
  , arguments = InputValue.None
  , resolver = \_ _ -> pure () 
  }


field
  :: (Monad m, Resolve.Output kind m)
  => Text
  -> Definition kind m value
  -> FieldSpec m context args value
  -> Object.Field m context
field name def (FieldSpec {..}) =
    Object.Field 
      { name
      , description
      , deprecation
      , definition =
          Resolve.outputType def
      , arguments
      , resolver = \ctx -> do
          let Object.FieldContext{ parentContext, responseKey, arguments, subSelection } = ctx
          let ExecutionContext { value, path } = parentContext
          result <- resolver arguments value
          resolveOutput parentContext
            { value = result
            , path = ResponseKey responseKey : path
            , selectionSet = subSelection
            }
      }
  where
    resolveOutput = Resolve.output def


-- Arguments

-- ...
-- , arguments =
--     arg @"string" (nonNull string) argSpec
--       { description =
--           "String to repeat"
--       }
--     ++++
--     argWithDefault @"times" int 1 argSpec
--       { description =
--           "Number of times to repeat the string (defaults to 1)"
--       } 
--  , resolve =
--      \args root -> do
--        let str = root .! #string
--        let times = root .! #times
--        pure (replicate times str)
-- ...


data ArgSpec =
  ArgSpec
    { description :: Text
    }


argSpec :: ArgSpec
argSpec = ArgSpec
  { description = ""
  }


arg
  :: forall label m kind value
   . (KnownSymbol label, Resolve.Input kind)
  => Definition kind m value
  -> ArgSpec
  -> InputValue.Collection (label .== value)
arg def (ArgSpec {..}) =
    InputValue.Single (Label :: Label label)
    $ InputValue.Definition
        { description
        , defaultValue = Nothing
        , defaultGraphQLValue = Nothing
        , definition = Resolve.inputType def
        , resolver = resolveInput
        }
  where
    resolveInput = Resolve.deserialize def


getArg
  :: forall label value row
   . (KnownSymbol label, HasType label value row)
  => Rec row
  -> value
getArg rec = rec .! (Label :: Label label)
 

argWithDefault
  :: forall label m kind value
   . (KnownSymbol label, Resolve.Input kind)
  => Definition kind m (Maybe value)
  -> value
  -> ArgSpec
  -> InputValue.Collection (label .== value)
argWithDefault def defaultValue (ArgSpec {..}) =
    InputValue.Single (Label :: Label label)
    $ InputValue.Definition
        { description
        , defaultValue = Just defaultValue
        , defaultGraphQLValue =
            Just (Resolve.serialize def (Just defaultValue))
        , definition = Resolve.inputType def
        , resolver = \case
            AST.ValueNull -> Just defaultValue
            value -> join (resolveInput value)
        }
  where
    resolveInput = Resolve.deserialize def


infixl 6 ++++
(++++) :: InputValue.Collection l -> InputValue.Collection r -> InputValue.Collection (l .+ r) 
(++++) = InputValue.Merge
