module Polaris.Codegen.ModulePlanner
       ( planModule
       ) where

import Prelude

import Control.Monad.Except (lift)
import Control.Monad.State (StateT, get, put, runStateT, state)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Polaris.Codegen.TypParser (parseTyp)
import Polaris.Codegen.Types (ComponentSpec, Module, Prop, RawComponent, RawProp(..), Typ(..), TypeDef)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (eof)

planModule
  :: { name :: String
     , rawProps :: Array RawProp
     , rawSubComponents :: Array RawComponent
     }
     -> Either String Module
planModule { name, rawProps, rawSubComponents } = uncurry mkModule <$> runStateT plan Map.empty
  where
    plan = do
      mainSpec <- readRawComponent' mainToNamePath { name, rawProps }
      subSpec <- traverse (readRawComponent' subToNamePath) rawSubComponents
      let specs = Array.cons mainSpec subSpec
      pure { name, specs }

    mkComponentSpec
      :: (String -> Array String)
         -> { name :: String, props :: Array Prop }
         -> ComponentSpec
    mkComponentSpec toNamePath { name: n, props } =
      { namePath: toNamePath n
      , props
      }

    readRawComponent' toNamePath r = mkComponentSpec toNamePath <$> readRawComponent r
    mainToNamePath = Array.singleton
    subToNamePath n = [ name, n ]

    mkModule { name: n, specs } typeDefMap =
      { name: n
      , specs
      , typeDefs: Array.fromFoldable $ Map.values typeDefMap
      }

type St = Map String TypeDef
type F a = StateT St (Either String) a

readRawComponent :: RawComponent -> F { name :: String, props :: Array Prop }
readRawComponent { name, rawProps } =
  (traverse readRawProp) rawProps <#> { name, props: _ }

readRawProp :: RawProp -> F Prop
readRawProp (RawProp r) = do
  typ <- (lift $ readTyp' r."type") >>= fillInTypDef r.types
  pure
    { name: r.name
    , typ
    , required: r.mandatory
    , description: r.description
    }

  where
    readTyp' s = lmap (showParseError s) (runParser s (parseTyp <* eof))
    showParseError s e = "Given: " <> s <> ", Error: " <> show e

fillInTypDef :: Maybe (Array RawProp) -> Typ -> F Typ
fillInTypDef (Just rawProps) (TypRef name) =
  (traverse readRawProp) rawProps >>= \props -> do
    name' <- recordTypDef { name, typ: Just (toRecord props) }
    pure $ TypRef name'
  where
    -- TODO add mandatory / required info
    toRecord props =
      TypRecord $ (\ { name: name', typ } ->
                    { name: name', typ }) <$> props
fillInTypDef Nothing (TypRef name) = do
  name' <- recordTypDef { name, typ: Nothing }
  pure $ TypRef name'
fillInTypDef rp (TypArray tr@(TypRef _)) = do
  -- for array types, the rawprops on the current node
  tr' <- fillInTypDef rp tr
  pure $ TypArray tr'
fillInTypDef _ typ =
  pure typ

recordTypDef :: TypeDef -> F String
recordTypDef { name, typ } = do
  typDefMap <- get
  case Map.lookup name typDefMap, typ of
    Just { typ: Just t1 }, Just t2 ->
      if t1 == t2
      then pure name
      else recordTypDef { name: name <> "'",  typ }
    _, Just t2 -> do
      put $ Map.insert name { name, typ } typDefMap
      pure name
    _, _ ->
      pure name

collectTypeDefs :: Array ComponentSpec -> Array TypeDef
collectTypeDefs ms = toTypeDef <$> Set.toUnfoldable set
  where
    set = foldr collectFromModule Set.empty ms
    collectFromModule {props} s = foldr (\p s' -> collectFromTyp p.typ s') s props

    collectFromTyp :: Typ -> Set String -> Set String
    collectFromTyp (TypRef name) s = Set.insert name s
    collectFromTyp (TypUnion ts) s = foldr collectFromTyp s ts
    collectFromTyp (TypFn { params, out }) s = foldr collectFromTyp s (Array.cons out params)
    collectFromTyp (TypArray t) s = collectFromTyp t s
    collectFromTyp (TypRecord es) s = foldr (collectFromTyp <<< _.typ) s es
    collectFromTyp _ s = s

    toTypeDef name = { name, typ: Nothing }
