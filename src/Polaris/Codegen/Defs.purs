module Polaris.Codegen.Defs
       where

import Prelude

import CST.Simple (Expr, Kind, cnst2, exprIdent, exprIdent1, knd, typCons, typCons1, typConsN, typOp, typString)
import CST.Simple as C
import Data.Array as Array

cnst_Coercible :: C.Type -> C.Type -> C.Constraint
cnst_Coercible =
  cnst2 "Untagged.Castable(class Castable)"

expr_coerce :: Expr
expr_coerce =
  exprIdent "Untagged.Castable(cast)"

expr_elem :: Expr -> Expr
expr_elem =
  exprIdent1 "Polaris.Internal(elem)"

expr_elemWithChildren :: Expr -> Expr
expr_elemWithChildren =
  exprIdent1 "Polaris.Internal(elemWithChildren)"

knd_Type :: Kind
knd_Type =
  knd "Type"

typ_Array :: C.Type -> C.Type
typ_Array =
  typCons1 "Array"

typ_Boolean :: C.Type
typ_Boolean =
  typCons "Boolean"

typ_BooleanLit :: Boolean -> C.Type
typ_BooleanLit b =
  typCons1 "Literals(BooleanLit)" (typString $ show b)

typ_Effect :: C.Type -> C.Type
typ_Effect =
  typCons1 "Effect(Effect)"

typ_EffectFn :: Int -> Array C.Type -> C.Type -> C.Type
typ_EffectFn n params out =
  typConsN ("Effect.Uncurried(EffectFn" <> show n <> ")") (Array.snoc params out)

typ_Foreign :: C.Type
typ_Foreign =
  typCons "Foreign(Foreign)"

typ_Number :: C.Type
typ_Number =
  typCons "Number"

typ_JSX :: C.Type
typ_JSX =
  typCons "React.Basic.Hooks(JSX)"

typ_PropsWithChildren :: C.Type -> C.Type
typ_PropsWithChildren =
  typCons1 "Polaris.Internal(PropsWithChildren)"

typ_ReactComponent :: C.Type -> C.Type
typ_ReactComponent =
  typCons1 "React.Basic.Hooks(ReactComponent)"

typ_String :: C.Type
typ_String =
  typCons "String"

typ_StringLit :: String -> C.Type
typ_StringLit s =
  typCons1 "Literals(StringLit)" (typString s)

typ_TranslationDictionary :: C.Type
typ_TranslationDictionary =
  typCons "Polaris.Components.AppProvider(TranslationDictionary)"

typ_UndefinedOr :: C.Type -> C.Type
typ_UndefinedOr =
  typCons1 "Untagged.Union(UndefinedOr)"

typ_Unit :: C.Type
typ_Unit =
  typCons "Prelude(Unit)"

typOp_OneOf :: C.Type -> C.Type -> C.Type
typOp_OneOf t1 t2 =
  typOp t1 "Untagged.Union(type (|+|))" t2
