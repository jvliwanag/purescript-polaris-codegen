module Polaris.Codegen.TypParser
       ( parseTyp
       ) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (many)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.CodePoint.Unicode (isAlpha, isAlphaNum, isLower)
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar)
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested (type (/\))
import Polaris.Codegen.Defs (typ_Boolean, typ_Foreign, typ_Number, typ_String, typ_Unit)
import Polaris.Codegen.Types (Prop, Typ(..), typBooleanLiteral, typJSX, typStringLiteral)
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (asErrorMessage, between, option, optional, sepBy, sepBy1, try, (<?>))
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
-- TODO
      <|> (typJSX <$ string "ReactElement<any, string | JSXElementConstructor<any>>")
      <|> (typJSX <$ string "ReactElement")
      <|> (typBooleanLiteral true <$ string "true")
      <|> (typBooleanLiteral false <$ string "false")
      <|> (typStringLiteral <$> parseStringLiteral)

-- TODO
      <|> (TypRecord <$> parseRecordEntries p)
      <|> (uncurry TypRef <$> parseTypRef p)
      <|> (TypFn <$> parseFnParts p)

    parseOneTyp p =
      (try (between (string "(") (string ")") p))
      <|> parseOneInnerTyp p

    parseOneOrArrayTyp p = do
      g <- parseOneTyp p
      loop g
      where
        loop b = (string "[]" >>= \_ -> (loop (TypArray b))) <|> pure b

    parseTypUnions p =
      sepBy1 (parseOneOrArrayTyp p) (string " | ")

parseStringLiteral :: P String
parseStringLiteral =
  between (string "\"") (string "\"") lit
  where
    lit = (CodeUnits.fromCharArray <$> Array.many (noneOf ['"']))

-- TODO handle intersections
parseTypRef :: P Typ -> P (String /\ Array Typ)
parseTypRef p = Tuple
  <$> parseRefName
  <*> parseArgs
  where
    parseRefName =
      (CodeUnits.singleton <$> satisfy charIsAlpha)
      <> (CodeUnits.fromCharArray <$> Array.many (satisfy (\c -> (charIsAlphaNum c) || (c == '.'))))

    parseArgs =
      option [] $
      between (string "<") (string ">")
      (Array.fromFoldable <$> sepBy1 p (string ", "))

{-
parseRefNames :: P String
parseRefNames = List.intercalate " & " <$> sepBy1 parseRefName (string " & ")
  where
    parseRefName =
      (CodeUnits.singleton <$> (satisfy charIsAlpha))
      <> (CodeUnits.fromCharArray <$> Array.many (satisfy (\c -> (charIsAlphaNum c) || (c == '.')))) -- todo prevent end with '.'
-}

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
  map toProp <$>
  ( between
    (string "{ ")
    (string "}")
    (many (parseFnSingleParamPart (parseTyp' <* string "; ")))
  )

  where
    toProp { name, typ } =
      { name
      , typ
      , required: false
      , description: Nothing
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
   (sepBy (parseFnSingleParamPart parseTyp') (string ", "))
  )

parseFnSingleParamPart :: P Typ -> P { name :: String , typ :: Typ }
parseFnSingleParamPart parseTyp' = ado
  name <- uncapitalizedWord <* optional (string "?")
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
