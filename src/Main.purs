module Main where

import Prelude

import CST.Simple.Project (defaultProjectSettings, runProject)
import CST.Simple.ProjectBuilder (addModule)
import Control.Monad.Error.Class (throwError)
import Control.Monad.ST (ST)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, parseJson, printJsonDecodeError)
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Either (Either(..))
import Data.Foldable (fold)
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
import Node.ChildProcess (defaultExecOptions, execFile)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir)
import Node.FS.Sync as FS
import Node.Path (FilePath, basenameWithoutExt)
import Node.Path as Path
import Polaris.Codegen.LocalesModulePrinter (localesModuleBuilder)
import Polaris.Codegen.ModulePlanner (planModule)
import Polaris.Codegen.ModulePrinter (componentModuleBuilder)
import Polaris.Codegen.SchemaFetcher (fetchSchema)
import Polaris.Codegen.Types (Module, ModuleExtras, RawProp(..), RawComponent)

main :: Effect Unit
main = runAff_ logResult do
  initProjectDir
  locales <- listLocales
  components <- Array.filter isValidComponent <$> fetchSchema
  modules <- traverse assembleModule components
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

    addComponentModule m@{ name } = do
      addModule ("Polaris.Components." <> name) $ componentModuleBuilder m

    isValidComponent rc =
      rc.name /= "." && rc.name /= "components"

assembleModule :: RawComponent -> Aff Module
assembleModule rc = do
  extras <- ifM (liftEffect $ FS.exists extrasFilePath)
    (Just <$> readExtras)
    (pure Nothing)

  let extraProps = fold $ extras >>= _.rawProps
      rawSubComponents = fold $ extras >>= _.rawSubComponents

      rawProps = applyExtraProps extraProps

  rightToF $ planModule {name: rc.name, rawProps, rawSubComponents}

  where
    extrasFilePath = "./data/" <> rc.name <> "-extras.json"

    readExtras :: Aff ModuleExtras
    readExtras = readContent extrasFilePath

    applyExtraProps :: Array RawProp -> Array RawProp
    applyExtraProps overrides =
      STArray.run do
        rsSt <- STArray.thaw rc.rawProps
        traverse_ (applyOverride rsSt) overrides
        pure rsSt

    applyOverride
      :: forall r
         . STArray r RawProp
         -> RawProp
         -> ST r Unit
    applyOverride rsSt orp@(RawProp o) = do
      os <- STArray.unsafeFreeze rsSt
      case (Array.findIndex (eq o.name <<< getName) os) of
        Just ndx -> void $ STArray.modify ndx (const orp) rsSt
        Nothing -> void $ STArray.push orp rsSt

    getName :: RawProp -> String
    getName (RawProp { name }) = name

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
  map (\p -> basenameWithoutExt p ".json") <$> readdir "polaris-react/locales"

readContent :: forall a. DecodeJson a => FilePath -> F a
readContent path =
  readTextFile UTF8 path >>= \s -> case decodeJson =<< parseJson s of
    Left e ->
      errMessage $ printJsonDecodeError e
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
