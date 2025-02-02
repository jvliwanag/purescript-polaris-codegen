module Polaris.Codegen.TypParser
       ( parseTyp
       ) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.CodePoint.Unicode (isAlpha, isAlphaNum, isLower)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar)
import Data.String.CodeUnits as CodeUnits
import Polaris.Codegen.Defs (typ_Boolean, typ_Foreign, typ_Number, typ_String, typ_Unit)
import Polaris.Codegen.Types (Prop, Typ(..), typBooleanLiteral, typJSX, typStringLiteral)
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (asErrorMessage, between, option, sepBy, sepBy1, try, (<?>))
import Text.Parsing.Parser.String (noneOf, satisfy, string)
import Text.Parsing.Parser.Token (alphaNum, upper)

type P a = Parser String a

parseTyp :: P Typ
parseTyp = fix \p -> do
  parseTypUnions p >>= \ts -> case NonEmptyArray.fromFoldable ts of
    Nothing -> fail "no typ found"
    Just ts' -> pure $ case NonEmptyArray.uncons ts' of
      { head, tail: [] } -> head
      _ -> TypUnion ts'

  where
    parseOneInnerTyp p = asErrorMessage "typ format"
      $ (TypSType typ_String <$ string "string")
      <|> (TypSType typ_Boolean <$ string "boolean")
      <|> (TypSType typ_Number <$ string "number")
      <|> (TypSType typ_Foreign <$ string "any")
      <|> (TypSType typ_Unit <$ string "void")
      <|> (typJSX <$ string "React.ReactNode")
      <|> (typJSX <$ string "React.ReactElement")
      <|> (typJSX <$ string "ReactElement")
      <|> (typBooleanLiteral true <$ string "true")
      <|> (typBooleanLiteral false <$ string "false")
      <|> (typStringLiteral <$> parseStringLiteral)
      <|> (TypRecord <$> parseRecordEntries p)
      <|> (TypRef <$> parseRefNames)
      <|> (TypFn <$> parseFnParts p)

    parseOneTyp p =
      (try (between (string "(") (string ")") p))
      <|> parseOneInnerTyp p

    parseOneOrArrayTyp p = do
      g <- parseOneTyp p
      f <- option identity (string "[]" $> TypArray)
      pure $ f g

    parseTypUnions p =
      sepBy1 (parseOneOrArrayTyp p) (string " | ")

parseStringLiteral :: P String
parseStringLiteral =
  between (string "\"") (string "\"") lit
  where
    lit = (CodeUnits.fromCharArray <$> Array.many (noneOf ['"']))

parseRefNames :: P String
parseRefNames = List.intercalate " & " <$> sepBy1 parseRefName (string " & ")
  where
    parseRefName =
      (CodeUnits.singleton <$> (satisfy charIsAlpha))
      <> (CodeUnits.fromCharArray <$> Array.many (satisfy (\c -> (charIsAlphaNum c) || (c == '.')))) -- todo prevent end with '.'

parseFnParts :: P Typ -> P { params :: Array Typ, out :: Typ }
parseFnParts parseTyp' = ado
  params <- parseFnParamsPart parseTyp'
  _ <- string " => "
  out <- parseTyp'
  in { params, out }

parseFnParamsPart :: P Typ -> P (Array Typ)
parseFnParamsPart parseTyp' =
  map _.typ <$> (parseParamsPart "(" ")" parseTyp')

parseRecordEntries :: P Typ -> P (Array Prop)
parseRecordEntries parseTyp' =
  map toProp <$> parseParamsPart "{" "}" parseTyp'

  where
    toProp { name, typ } =
      { name
      , typ
      , required: false
      }

parseParamsPart
  :: String
     -> String
     -> P Typ
     -> P (Array { name :: String, typ :: Typ })
parseParamsPart open close parseTyp' =
  Array.fromFoldable <$>
  (between
   (string open)
   (string close)
   (sepBy parseFnSingleParamPart (string ", "))
  )
  where
    parseFnSingleParamPart = ado
      name <- uncapitalizedWord
      _ <- string ": "
      typ <- parseTyp'
      in { name, typ }

parseUnion :: P Typ -> P (NonEmptyArray Typ)
parseUnion parseTyp' = opts >>= case _ of
  Just na | NonEmptyArray.length na > 1 -> pure na
  _ -> fail "minimum union options is 2"
  where
    opts = NonEmptyArray.fromFoldable <$> sepBy parseTyp' (string " | ")

capitalizedWord :: P String
capitalizedWord =
  (CodeUnits.singleton <$> upper)
  <> (CodeUnits.fromCharArray <$> Array.many alphaNum)

uncapitalizedWord :: P String
uncapitalizedWord =
  (CodeUnits.singleton <$> (satisfy charIsLower <?> "lowercase letter"))
  <> (CodeUnits.fromCharArray <$> Array.many alphaNum)

word :: P String
word = (CodeUnits.singleton <$> alphaNum)
  <> (CodeUnits.fromCharArray <$> Array.many alphaNum)

-- Utils

charIsAlpha :: Char -> Boolean
charIsAlpha = isAlpha <<< codePointFromChar

charIsAlphaNum :: Char -> Boolean
charIsAlphaNum = isAlphaNum <<< codePointFromChar

charIsLower :: Char -> Boolean
charIsLower = isLower <<< codePointFromChar
