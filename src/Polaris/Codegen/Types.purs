module Polaris.Codegen.Types where

import Prelude

import CST.Simple (Type)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Foreign (Foreign)
import Foreign.Object (Object)
import Polaris.Codegen.Defs (typ_BooleanLit, typ_JSX, typ_StringLit)
import Simple.JSON (class ReadForeign, readImpl)

newtype RawProp =
  RawProp { name :: String
           , "type" :: String
           , mandatory :: Boolean
           , "types" :: Maybe (Array RawProp)
           }

instance rawPropReadForeign :: ReadForeign RawProp where
  readImpl f = RawProp <$> readImpl f

type RawComponent =
  { name :: String
  , rawProps :: Array RawProp
  }

type RawModule =
  { name :: String
  , rawProps :: Array RawProp
  , rawSubComponents :: Array RawComponent
  }

type ModuleExtras =
  { rawProps :: Maybe (Array (Object Foreign))
  , rawSubComponents :: Maybe (Array RawComponent)
  }

--  {"type":"string","kind":"string","mandatory":false,"tags":null,"description":"Space separated list of character encodings","defaultValue":null,"types":null,"__typename":"Property"}

data PSImport =
  PSIPrelude
  | PSIModule String (Array PSImportEntry)

derive instance psiEq :: Eq PSImport
derive instance psiOrd :: Ord PSImport

data PSImportEntry =
  PSIEClass String
  | PSIEType String
  | PSIEFn String

derive instance psieEq :: Eq PSImportEntry
derive instance psieOrd :: Ord PSImportEntry

type Module =
  { name :: String
  , typeDefs :: Array TypeDef
  , specs :: Array ComponentSpec
  }

type TypeDef = { name :: String, typ :: Maybe Typ }

type ComponentSpec =
  { namePath :: Array String
  , baseProps :: Array Prop
  , hasJSXChildren :: Boolean
  }

type Prop =
  { name :: String
  , typ :: Typ
  , required :: Boolean
  }

data Typ
  = TypSType Type
  | TypUnion (NonEmptyArray Typ)
  | TypFn { params :: Array Typ, out :: Typ }
  | TypRef String
  | TypArray Typ
  | TypRecord (Array Prop)

derive instance eqTyp :: Eq Typ

derive instance typGeneric :: Generic Typ _
instance typShow :: Show Typ where
  show x = genericShow x

typJSX :: Typ
typJSX = TypSType typ_JSX

typBooleanLiteral :: Boolean -> Typ
typBooleanLiteral b =
  TypSType $ typ_BooleanLit b

typStringLiteral :: String -> Typ
typStringLiteral s =
  TypSType $ typ_StringLit s
