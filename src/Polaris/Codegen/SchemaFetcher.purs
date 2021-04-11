module Polaris.Codegen.SchemaFetcher
       ( fetchSchema
       ) where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (throwError)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Exception (error)
import Polaris.Codegen.PrinterUtils (lines)
import Polaris.Codegen.Types (RawComponent, RawProp)

fetchSchema :: Aff (Array RawComponent)
fetchSchema = do
  { body } <- either throwAXError pure =<< AX.post ResponseFormat.json "https://polaris.shopify.com/api" (Just (RequestBody.json req))
  readBody body

  where
    throwAXError e = throwError $ error $ AX.printError e

req :: Json
req =
  encodeJson
  { operationName: "ComponentsQuery"
  , query: lines
    [ "query ComponentsQuery {"
    , "  componentQuery {"
    , "    components {"
    , "      web {"
    , "        reactName"
    , "        properties {"
    , "          name"
    , "          type"
    , "          mandatory"
    , "          types {"
    , "            name"
    , "            type"
    , "            mandatory"
    , "            types {"
    , "              name"
    , "              type"
    , "              mandatory"
    , "            }"
    , "          }"
    , "        }"
    , "      }"
    , "    }"
    , "  }"
    , "}"
    ]
  }

readBody :: Json -> Aff (Array RawComponent)
readBody j = do
  r <- either (throwError <<< error <<< printJsonDecodeError) pure decoded

  pure $ readR <$> r.data.componentQuery.components
  where
    decoded :: Either JsonDecodeError { data :: { componentQuery :: { components :: Array { web :: { reactName :: String, properties :: Array RawProp } } } } }
    decoded = decodeJson j

    readR { web: w } =
      { name: w.reactName
      , rawProps: w.properties
      }
