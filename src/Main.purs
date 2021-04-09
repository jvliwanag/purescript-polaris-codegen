module Main where

import Prelude

import CST.Simple.Project (defaultProjectSettings, runProject)
import CST.Simple.ProjectBuilder (addModule)
import Control.Monad.Error.Class (throwError)
import Control.Monad.ST (ST)
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Either (Either(..))
import Data.Foldable (fold, intercalate)
import Data.List (foldMap)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String (Pattern(..), Replacement(..), stripSuffix)
import Data.String as String
import Data.Traversable (traverse, traverse_)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler, runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, warn)
import Effect.Exception (error)
import Effect.Exception as Exception
import Foreign (Foreign, renderForeignError, unsafeToForeign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.ChildProcess (defaultExecOptions, execFile)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir)
import Node.FS.Sync as FS
import Node.Path (FilePath, basenameWithoutExt)
import Node.Path as Path
import Polaris.Codegen.LocalesModulePrinter (localesModuleBuilder)
import Polaris.Codegen.ModulePlanner (planModule)
import Polaris.Codegen.ModulePrinter (componentModuleBuilder)
import Polaris.Codegen.Types (Module, ModuleExtras, RawProp)
import Simple.JSON (class ReadForeign, read, readJSON, read_)

main :: Effect Unit
main = runAff_ logResult do
  initProjectDir
  modules <- listDataFiles >>= traverse readModuleFilePaths
  locales <- listLocales
  runProject projectSettings do
    addModule "Polaris.Components.Locales" $ localesModuleBuilder locales
    traverse_ addComponentModule modules
  where
    logResult (Left e) = warn $ "ERROR: " <> (Exception.message e)
    logResult (Right a) = log "OK"

    projectSettings =
      defaultProjectSettings { outputDirectory = generatedSrcDir
                             , rmDirectoryFilesPreRun = true
                             }

    addComponentModule m@{ name } =
      addModule ("Polaris.Components." <> name) $ componentModuleBuilder m

type F = Aff

type ModuleFilePaths =
  { propsFilePath :: FilePath
  , extrasFilePath :: Maybe FilePath
  }

listDataFiles :: F (Array ModuleFilePaths)
listDataFiles =
  readDirRel "./data" >>= traverse readDirRel <#> (join <<< map getModuleFilePaths)

  where
    readDirRel path = map (\s -> path <> "/" <> s) <$> readdir path

    allFilePaths = readDirRel "./data" >>= traverse readDirRel <#> join

getModuleFilePaths :: Array FilePath -> Array ModuleFilePaths
getModuleFilePaths allFilePaths = foldMap f allFilePaths
  where
    isExtrasFilePath =
      isJust <<< stripSuffix (Pattern "-extras.json")

    f path
      | isExtrasFilePath path = mempty
      | otherwise = pure $
        { propsFilePath: path
        , extrasFilePath: extrasFilePath path
        }

    extrasFilePath propsFilePath =
      if Array.elem p allFilePaths
      then Just p
      else Nothing

      where
        p = String.replace
            (Pattern ".json")
            (Replacement "-extras.json")
            propsFilePath

listLocales :: F (Array FilePath)
listLocales =
  map (\p -> basenameWithoutExt p ".json") <$> readdir "./node_modules/@shopify/polaris/locales"

readModuleFilePaths :: ModuleFilePaths -> F Module
readModuleFilePaths { propsFilePath, extrasFilePath } = do
  log $ "Reading " <> propsFilePath
  os' <- readPropObjects propsFilePath

  extra <- traverse readExtra extrasFilePath
  let extraProps = fold $ extra >>= _.rawProps
      rawSubComponents = fold $ extra >>= _.rawSubComponents

      os = applyExtras extraProps os'

  rawProps <- traverse readPropObject os

  rightToF $ planModule {name, rawProps, rawSubComponents}

  where
    readPropObjects :: FilePath -> F (Array (Object Foreign))
    readPropObjects = readContent

    name = basenameWithoutExt propsFilePath ".json"

    readExtra :: FilePath -> F ModuleExtras
    readExtra e = do
      log $ "Reading " <> e
      readContent e

    applyExtras :: Array (Object Foreign) -> Array (Object Foreign) -> Array (Object Foreign)
    applyExtras overrides os =
      STArray.run do
        rsSt <- STArray.thaw os
        traverse_ (applyExtraSt rsSt) overrides
        pure rsSt

    applyExtraSt
      :: forall r
         . STArray r (Object Foreign)
         -> Object Foreign
         -> ST r Unit
    applyExtraSt rsSt o = do
      os <- STArray.unsafeFreeze rsSt
      case (Array.findIndex (eq oName <<< getName) os) of
        Just ndx -> void $ STArray.modify ndx (Object.union o) rsSt
        Nothing -> void $ STArray.push o rsSt

      where
        oName = getName o

    getName :: Object Foreign -> Maybe String
    getName = Object.lookup "name" >=> read_

    readPropObject :: Object Foreign -> F RawProp
    readPropObject o = case read (unsafeToForeign o) of
      Left e ->
        errMessage $ intercalate ", " $ map renderForeignError $ e
      Right a ->
        pure a

readContent :: forall a. ReadForeign a => FilePath -> F a
readContent path =
  readTextFile UTF8 path >>= \s -> case readJSON s of
    Left e ->
      errMessage $ intercalate ", " $ map renderForeignError $ e
    Right a ->
      pure a

initProjectDir :: Aff Unit
initProjectDir =
  unlessM (liftEffect $ FS.exists generatedProjectDir) $
  makeAff \cb -> do
    log $ "Creating " <> generatedProjectDir <> "..."
    FS.mkdir generatedProjectDir
    log $ "Cloning " <> generatedGitRepo <> " to " <> generatedProjectDir <> "..."
    _ <- execFile "git"
         [ "clone", generatedGitRepo, generatedProjectDir ]
         defaultExecOptions \res ->
         cb $ maybe (pure unit) Left res.error
    pure nonCanceler

generatedGitRepo :: FilePath
generatedGitRepo = "git@github.com:jvliwanag/purescript-polaris.git"

generatedProjectDir :: FilePath
generatedProjectDir = "purescript-polaris"

generatedSrcDir :: FilePath
generatedSrcDir =
  Path.concat [ generatedProjectDir, "src", "generated" ]

errMessage :: forall a. String -> F a
errMessage = throwError <<< error

rightToF :: forall a. Either String a -> F a
rightToF (Left e) = errMessage e
rightToF (Right a) = pure a
