module React.Docgen.Typescript
       ( ComponentDoc
       , Props
       , PropItem
       , Method
       , MethodParameter
       , Component
       , PropItemType
       , ParentType
       , PropFilter
       , TsSymbol
       , TsSourceFile
       , ComponentNameResolver
       , StaticPropFilter
       , ParserOptions
       , FileParser
       , parse_
       , parse
       , parsePaths_
       , parsePaths
       , withCustomConfig
       , parserParse
       , parserParsePaths
       ) where

import Data.Function.Uncurried (Fn2)
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Foreign (Foreign)
import Foreign.Object (Object)
import Literals (BooleanLit)
import Literals.Null (Null)
import Node.Path (FilePath)
import Untagged.Castable (class Castable, cast)
import Untagged.Union (type (|+|), UndefinedOr)

type ComponentDoc =
  { displayName :: String
  , description :: String
  , props :: Props
  , methods :: Array Method
  }

type Props = Object PropItem

--staticPropFilter ::

type PropItem =
  { name :: String
  , required :: Boolean
  , "type" :: PropItemType
  , description :: String
  , defaultValue :: Foreign
  , parent :: Nullable ParentType
  , declarations :: Nullable (Array ParentType)
  , tags :: Nullable {}
  }

type Method =
  { name :: String
  , docblock :: String
  , modifiers :: Array String
  , params :: Array MethodParameter
  , returns :: Nullable
       { description :: Nullable String
       , type :: String
       }
  , description :: String
  }

type MethodParameter =
  { name :: String
  , description :: Nullable String
  }

type Component =
  { name :: String
  }

type PropItemType =
  { name :: String
  , value :: Nullable Foreign
  , raw :: Nullable String
  }

type ParentType =
  { name :: String
  , fileName :: String
  }

type PropFilter = Fn2 PropItem Component Boolean

foreign import data TsSymbol :: Type
foreign import data TsSourceFile :: Type

type ComponentNameResolver =
  Fn2 TsSymbol TsSourceFile (UndefinedOr (String |+| Null |+| BooleanLit "false"))

type StaticPropFilter =
  { skipPropsWithName :: UndefinedOr (Array String |+| String)
  , skipPropsWithoutDoc :: Boolean
  }

type ParserOptions =
  { propFilter :: UndefinedOr (StaticPropFilter |+| PropFilter)
  , componentNameResolver :: UndefinedOr ComponentNameResolver
  , shouldExtractLiteralValuesFromEnum :: UndefinedOr Boolean
  , shouldRemoveUndefinedFromOptional :: UndefinedOr Boolean
  , shouldExtractValuesFromUnion :: UndefinedOr Boolean
  , skipChildrenPropWithoutDoc :: UndefinedOr Boolean
  , savePropValueAsString :: UndefinedOr Boolean
  , shouldIncludePropTagMap :: UndefinedOr Boolean
  , customComponentTypes :: UndefinedOr (Array String)
  }

foreign import data FileParser :: Type

parse_ :: FilePath -> Effect (Array ComponentDoc)
parse_ path = parse path {}

parse :: forall opts. Castable opts ParserOptions => FilePath -> opts -> Effect (Array ComponentDoc)
parse path opts = runEffectFn2 _parse (cast path) (cast opts)

parsePaths_ :: Array FilePath -> Effect (Array ComponentDoc)
parsePaths_ paths = parsePaths paths {}

parsePaths :: forall opts. Castable opts ParserOptions => Array FilePath -> opts -> Effect (Array ComponentDoc)
parsePaths paths opts = runEffectFn2 _parse (cast paths) (cast opts)

withCustomConfig :: forall opts. Castable opts ParserOptions => FilePath -> opts -> Effect FileParser
withCustomConfig tsconfigPath opts =
  runEffectFn2 _withCustomConfig tsconfigPath (cast opts)

parserParse :: FilePath -> FileParser -> Effect (Array ComponentDoc)
parserParse path parser =
  runEffectFn1 (_parserParse parser) (cast path)

parserParsePaths :: Array FilePath -> FileParser -> Effect (Array ComponentDoc)
parserParsePaths paths parser =
  runEffectFn1 (_parserParse parser) (cast paths)

foreign import _parse :: EffectFn2 (FilePath |+| Array FilePath) ParserOptions (Array ComponentDoc)
foreign import _withCustomConfig :: EffectFn2 FilePath ParserOptions FileParser
foreign import _parserParse :: FileParser -> EffectFn1 (FilePath |+| Array FilePath) (Array ComponentDoc)
