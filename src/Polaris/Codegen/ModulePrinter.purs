module Polaris.Codegen.ModulePrinter
       ( printModule
       ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldMap)
import Data.String.Extra (camelCase)
import Polaris.Codegen.PrinterUtils (lines, printRefName)
import Polaris.Codegen.Types (Module, PSJSContent, Typ(..), PropEntry)

printModule :: Module -> PSJSContent
printModule { name, props, subcomponents } =
  { psContent: printPSContent { name, exports, psCodes }
  , jsContent: printJSContent jsCodes
  }

  where
    mainSpec =
      { namePath: [ name ]
      , props: props
      }

    subSpecs = subcomponents <#> \sub ->
      { namePath: [ name, sub.name ]
      , props: sub.props
      }

    specs = Array.cons mainSpec subSpecs
    codes = mkComponentCode <$> specs

    exports = foldMap _.exports codes
    psCodes = _.psCode <$> codes
    jsCodes = _.jsCode <$> codes

printPSContent
  :: { name :: String, exports :: Array String, psCodes :: Array String }
     -> String
printPSContent { name, exports, psCodes } = lines
  [ "module Polaris.Components." <> name
  , "  ( " <> exportsPart
  , "  ) where"
  , ""
  , "import Prelude"
  , ""
  , "import Effect"
  , "import Effect.Uncurried"
  , "import Foreign"
  , "import Literals"
  , "import React.Basic.Hooks"
  , "import Polaris.Types"
  , "import Untagged.Coercible"
  , "import Untagged.Union"
  , ""
  , psCodesPart
  , ""
  ]
  where
    exportsPart = Array.intercalate "\n  , " exports
    psCodesPart = lines psCodes

printJSContent :: Array String -> String
printJSContent jsCodes =
  lines jsCodes <> "\n"

type ComponentSpec =
  { namePath :: Array String
  , props :: Array PropEntry
  }

type ComponentCode =
  { exports :: Array String
  , psCode :: String
  , jsCode :: String
  }

mkComponentCode :: ComponentSpec -> ComponentCode
mkComponentCode { namePath, props } =
  { exports: [ propsName, elFnName, rcFnName ]
  , psCode: lines
    [ "type " <> propsName <> " ="
    , "  { " <> propsContent
    , "  }"
    , ""
    , elFnName <> " :: forall r. Coercible r " <> propsName <> " => r -> JSX"
    , elFnName <> " = element " <> rcFnName <> " <<< coerce"
    , ""
    , "foreign import " <> rcFnName <> " :: ReactComponent " <> propsName
    ]
  , jsCode:
    "exports."
    <> rcFnName
    <> " = require(\"@shopify/polaris\")."
    <> jsPath
    <> ";"
  }

  where
    name = Array.fold namePath

    propsName = name <> "Props"
    elFnName = camelCase name
    rcFnName = elFnName <> "RC"

    jsPath = Array.intercalate "." namePath

    propsContent = Array.intercalate "\n  , " $ printPropEntry <$> props



printPropEntry :: PropEntry -> String
printPropEntry { name, description, required, typ } =
  "\"" <> name <> "\" :: " <> t
  where
    t' = printTyp typ

    t = if required
        then t'
        else "UndefinedOr (" <> t' <> ")"

printTyp :: Typ -> String
printTyp TypUnit = "Unit"
printTyp TypString = "String"
printTyp TypBoolean = "Boolean"
printTyp TypNumber = "Number"
printTyp TypJSX = "JSX"
printTyp (TypStringLiteral s) = "StringLit \"" <> s <> "\""
printTyp (TypBooleanLiteral b) = "BooleanLit \"" <> show b <> "\""
printTyp (TypArray t) = "Array " <> printTypWrapped t
printTyp (TypUnion ts) =
  Array.intercalate " |+| " $ printTypWrapped <$> (NonEmptyArray.toArray ts)
printTyp (TypRecord rs) =
  "{"
  <> ( Array.intercalate " , "
       $ (\ {name, typ} -> "\"" <> name <> "\" :: (" <> printTyp typ <> ")") <$> rs
     )
  <> "}"
printTyp (TypRef ns) =
  printRefName ns

printTyp TypForeign = "Foreign"
printTyp (TypFn { params, out }) = case params of
  [] -> "Effect " <> outPart
  _ ->
    "EffectFn" <> (show $ Array.length params) <> " "
    <> ( Array.intercalate " " $ printTypWrapped <$> params) <> " "
    <> outPart
  where
    outPart = printTypWrapped out

printTypWrapped :: Typ -> String
printTypWrapped t = "(" <> printTyp t <> ")"

--
