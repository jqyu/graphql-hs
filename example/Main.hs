
module Main where

import Protolude hiding (lift)
import Data.Row
import qualified GraphQL as GraphQL
import qualified GraphQL.Execution.Result as Result
import GraphQL.DSL
import qualified GraphQL.Validation.Validator as Validator
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)


app :: Application
app request respond =
  respond =<< case rawPathInfo request of
    "/" -> pure index
    "/graphql" -> graphql request
    _ -> pure notFound


index :: Response
index =
  responseFile
    status200
    [("Content-Type", "text/html")]
    "static/index.html"
    Nothing


graphql :: Request -> IO Response
graphql request = do
  body <- strictRequestBody request
  case Validator.run (GraphQL.handleRequest spec body rootFoo) of
    Left _ ->
      pure $
        responseLBS
          status200
          [("Content-Type", "text/plain")]
          "error!"
    Right getResult -> do
      result <- getResult
      pure $
        responseLBS
          status200
          [("Content-Type", "application/json")]
          (toS (Result.encode result))


notFound :: Response
notFound =
  responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 not found"


port :: Int
port = 1337


main :: IO ()
main = do
  putStrLn ("http://127.0.0.1:" ++ show port)
  run port app


spec :: GraphQL.Spec IO Foo ()
spec =
  GraphQL.Spec
    { rootQueryType = foo
    , rootMutationType = Nothing
    }


rootFoo :: Foo
rootFoo =
    Foo{ qux = 123, baz = Nothing, bars }
  where
    bars = map mkBar [1..100]

    mkBar :: Int32 -> Bar
    mkBar v =
      let qux = v `mod` 2 == 1 in
      let baz | qux = Just "bar"
              | otherwise = Nothing in
      Bar{ qux, baz, idx = v, parentFoo=rootFoo }


data Foo = Foo
  { qux :: Int32
  , baz :: Maybe Text
  , bars :: [Bar]
  }

foo :: ObjectDefinition IO Foo
foo = defineObject @"Foo" objectSpec
  { description =
    "Dummy object"
  , fields =
    [ field "qux" (nonNull int) fieldSpec
      { description = "qux"
      , resolver = \_ Foo{ qux } -> do
        putStrLn ("whatever" :: Text)
        pure qux
      }
    , field "baz" string fieldSpec
      { description = "baz"
      , resolver = \_ Foo{ baz } ->
        pure baz
      }
    , field "bazWithDefault" (nonNull string) fieldSpec
      { description =
        "baz, or the `default` argument if not provided"
      , arguments =
          argWithDefault @"defaultValue" string "" argSpec
            { description = "default value if Foo.baz is null"
            }
          ++++
          arg @"denoteNonEmptyWith" string argSpec
            { description = "result to use if the value is the empty string"
            }
      , deprecation =
          Just "do not use this"
      , resolver = \args Foo{ baz } ->
        let defaultValue = args .! #defaultValue in
        let denoteNonEmptyWith = args .! #denoteNonEmptyWith in
        pure $ case fromMaybe defaultValue baz of
          "" -> fromMaybe "" denoteNonEmptyWith
          v -> v
      }
    , field "bars" (nonNull (listOf (nonNull bar))) fieldSpec
      { description =
        "all bars"
      , arguments =
        argWithDefault @"limit" int 100 argSpec
          { description = "how many bars to show"
          }
        ++++
        argWithDefault @"offset" int 0 argSpec
          { description = "what index to start from"
          }
      , resolver = \args Foo{ bars } -> do
        let limit = fromIntegral $  args .! #limit
        let offset = fromIntegral $ args .! #offset
        pure . take limit . drop offset $ bars
      }
    ]
  }


data Bar = Bar
  { qux :: Bool
  , baz :: Maybe Text
  , idx :: Int32
  , parentFoo :: Foo
  }

bar :: ObjectDefinition IO Bar
bar = defineObject @"Bar" objectSpec
  { fields =
    [ field "qux" (nonNull boolean) fieldSpec
      { resolver = \_ Bar{ qux } -> pure qux
      }
    , field "baz" string fieldSpec
      { resolver = \_ Bar{ baz } -> pure baz
      }
    , field "index" (nonNull int) fieldSpec
      { resolver = \_ Bar{ idx } -> pure idx
      }
    , field "foo" (nonNull foo) fieldSpec
      { resolver = \_ Bar{ parentFoo } -> pure parentFoo
      }
    ]
  }
