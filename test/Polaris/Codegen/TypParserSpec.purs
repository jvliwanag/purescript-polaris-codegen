module Polaris.Codegen.TypParserSpec
       ( typParserSpec
       ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Exception (Error)
import Polaris.Codegen.Defs (typ_Boolean, typ_Number, typ_String)
import Polaris.Codegen.TypParser (parseTyp)
import Polaris.Codegen.Types (Typ(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (eof)

typParserSpec :: Spec Unit
typParserSpec = describe "typ parser" do
  recordSpec
  arraySpec
  refSpec
  unionSpec
  fnSpec

recordSpec :: Spec Unit
recordSpec = describe "record parser" do
  it "should parse empty record" do
    "{ }" `shouldParseAs` TypRecord []

  it "should parse single entry record" do
    "{ foo: string; }" `shouldParseAs`
      TypRecord [ { name: "foo"
                  , typ: TypSType typ_String
                  , required: false
                  , description: Nothing
                  }
                ]

  it "should parse multi entry record" do
    "{ foo: string; bar: number; }" `shouldParseAs`
      TypRecord [ { name: "foo"
                  , typ: TypSType typ_String
                  , required: false
                  , description: Nothing
                  }
                , { name: "bar"
                  , typ: TypSType typ_Number
                  , required: false
                  , description: Nothing
                  }
                ]

arraySpec :: Spec Unit
arraySpec = describe "array parser" do
  it "should parse single dim array" do
    "string[]" `shouldParseAs` TypArray (TypSType typ_String)

  it "should parse double dim array" do
    "string[][]" `shouldParseAs` TypArray (TypArray (TypSType typ_String))

refSpec :: Spec Unit
refSpec = describe "ref parser" do
  it "should parse single ref name" do
    "Foo" `shouldParseAs` TypRef "Foo" []

  it "should parse single ref name with on type param" do
    "Foo<string>" `shouldParseAs` TypRef "Foo" [ TypSType typ_String ]

  it "should parse single ref name with two type params" do
    "Foo<string, number>" `shouldParseAs`
      TypRef "Foo" [ TypSType typ_String
                   , TypSType typ_Number
                   ]

  it "should parse ref name with type param in type param" do
    "Foo<Bar<string>>" `shouldParseAs`
      TypRef "Foo" [ TypRef "Bar"
                     [ TypSType typ_String
                     ]
                   ]

unionSpec :: Spec Unit
unionSpec = describe "union parser" do
  it "should parse unions" do
    "string | boolean" `shouldParseAs`
      TypUnion (NonEmptyArray.cons' (TypSType typ_String) [(TypSType typ_Boolean)])

fnSpec :: Spec Unit
fnSpec = describe "fn parser" do
  it "should parse fn without param" do
    "() => string" `shouldParseAs`
      TypFn { params: [], out: TypSType typ_String }

  it "should parse fn with one param" do
    "(foo: string) => string" `shouldParseAs`
      TypFn { params: [ TypSType typ_String ]
            , out: TypSType typ_String
            }

  it "should parse fn with two params" do
    "(foo: string, bar: boolean) => string" `shouldParseAs`
      TypFn { params: [ TypSType typ_String
                      , TypSType typ_Boolean
                      ]
            , out: TypSType typ_String
            }

  it "should parse fn with optional param" do
    "(foo?: string) => string" `shouldParseAs`
      TypFn { params: [ TypSType typ_String ]
            , out: TypSType typ_String
            }

-- (selectionType: SelectionType, toggleType: boolean, selection?: string | Range | undefined) => void

shouldParseAs :: forall m. MonadThrow Error m => String -> Typ -> m Unit
shouldParseAs str typ =
  runParser str (parseTyp <* eof) `shouldEqual` Right typ
