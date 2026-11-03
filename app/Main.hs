module Main (main) where

import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative
import System.Directory
import System.FilePath ((</>), takeDirectory)
import System.Exit (exitFailure, exitSuccess)

import Types

newtype TeXFilePath     = TeXFilePath     FilePath
newtype ProjectRootPath = ProjectRootPath FilePath

hasLTeXFile :: FilePath -> IO Bool
hasLTeXFile directory = do
    hasLTeXRoot      <- doesFileExist      (directory </> ".ltex-root")
    hasLTeXDirectory <- doesDirectoryExist (directory </> ".ltex")

    pure (hasLTeXRoot || hasLTeXDirectory)

hasGitFile :: FilePath -> IO Bool
hasGitFile directory = do
    hasDotGitDirectory <- doesDirectoryExist (directory </> ".git")
    hasDotGitFile      <- doesFileExist      (directory </> ".git")

    pure (hasDotGitDirectory || hasDotGitFile)

searchDirectoryUpwards :: (FilePath -> IO Bool) -> FilePath -> IO (Maybe FilePath)
searchDirectoryUpwards predicate directory =
    predicate directory >>= \case
        True  -> pure (Just directory)
        False ->
            let parent = takeDirectory directory in
                if parent /= directory
                    then searchDirectoryUpwards predicate parent
                    else pure Nothing

searchForProjectRoot :: TeXFilePath -> IO ProjectRootPath
searchForProjectRoot (TeXFilePath texFilePath) = do
    absolutePath  <- makeAbsolute texFilePath
    canonicalPath <- canonicalizePath absolutePath
    let startDirectory = takeDirectory canonicalPath

    searchDirectoryUpwards hasLTeXFile startDirectory >>= \case
        Just projRoot -> pure (ProjectRootPath projRoot)
        Nothing       ->
            searchDirectoryUpwards hasGitFile startDirectory >>= \case
                Just projRoot -> pure (ProjectRootPath projRoot)
                Nothing       -> pure (ProjectRootPath startDirectory)

withProjectRoot :: TeXFilePath -> (ProjectRootPath -> IO ()) -> IO ()
withProjectRoot texFilePath programme =
    searchForProjectRoot texFilePath >>=
        programme

getLTeXConfigOrDefault :: ProjectRootPath -> IO (Either String LTeXConfig)
getLTeXConfigOrDefault (ProjectRootPath projRoot) =
    let configFilePath = projRoot </> ".ltex" </> "config.json" in
        doesFileExist configFilePath >>= \case
            True ->
                BS.readFile configFilePath >>=
                    pure . parseLTeXConfig

            False ->
                pure (Right defaultLTeXConfig)

withLTeXConfig :: ProjectRootPath -> (LTeXConfig -> IO ()) -> IO ()
withLTeXConfig projRoot programme =
    getLTeXConfigOrDefault projRoot >>= \case
        Right config -> programme config
        Left  err    -> putStrLn ("Failed to parse config file\n" <> err) >> exitFailure

withLTeXConfigAndProjectRoot :: TeXFilePath -> ((LTeXConfig, ProjectRootPath) -> IO()) -> IO ()
withLTeXConfigAndProjectRoot texFilePath programme =
    withProjectRoot texFilePath $ \projRoot ->
        withLTeXConfig projRoot $ \ltexConfig ->
            programme (ltexConfig, projRoot)

writeLTeXConfig :: LTeXConfig -> ProjectRootPath -> IO ()
writeLTeXConfig newConfig (ProjectRootPath projRoot) =
    let ltexDir = projRoot </> ".ltex"
        configFile = ltexDir </> "config.json" in
            createDirectoryIfMissing True ltexDir >>
                BS.writeFile configFile (showItemInJson (removeDefaultValues newConfig))

data Command = GetCommand             TeXFilePath SomeConfigPath
             | SetCommand             TeXFilePath SomeConfigPath Text
             | AddCommand             TeXFilePath SomeConfigPath Text
             | RemoveCommand          TeXFilePath SomeConfigPath Text
             | ProjectRootCommand     TeXFilePath
             | WorkspaceConfigCommand TeXFilePath

texFileOptionParser :: Parser TeXFilePath
texFileOptionParser = TeXFilePath <$> strOption
    ( long "texFile"
   <> metavar "FILEPATH"
   <> help "The path to the .tex (or .bib) file that you are currently editing"
    )

configPathParser :: Parser SomeConfigPath
configPathParser =
    argument (maybeReader (configPathFromText . Text.pack))
        (metavar "CONFIG_PATH")

valueParser :: Parser Text
valueParser = Text.pack <$> argument str (metavar "VALUE")

commandParser :: Parser Command
commandParser =
    hsubparser
        ( command "get"
            (info (GetCommand <$> texFileOptionParser <*> configPathParser)
                (progDesc "Get a configuration value"))
       <> command "set"
            (info (SetCommand <$> texFileOptionParser <*> configPathParser <*> valueParser)
                (progDesc "Set a configuration value"))
       <> command "add"
            (info (AddCommand <$> texFileOptionParser <*> configPathParser <*> valueParser)
                (progDesc "Add a configuration value"))
       <> command "remove"
            (info (RemoveCommand <$> texFileOptionParser <*> configPathParser <*> valueParser)
                (progDesc "Remove a configuration value"))
       <> command "projectRoot"
            (info (ProjectRootCommand <$> texFileOptionParser)
                (progDesc "Show a calculated project root path"))
       <> command "workspaceConfig"
            (info (WorkspaceConfigCommand <$> texFileOptionParser)
                (progDesc "Show current workspace config in json"))
        )

commandLineParser :: ParserInfo Command
commandLineParser =
    info (commandParser <**> helper)
        (fullDesc <> progDesc "A command-line tool for managing project-specific LTeX+ configurations with a focus on Vim integration.")

main :: IO ()
main = execParser commandLineParser >>= \case
    (GetCommand texFilePath (SomeConfigPath configPath)) ->
        withLTeXConfigAndProjectRoot texFilePath $ \(currentConf, _) ->
            case getLTeXConfigValue configPath currentConf of
                Right currentVal ->
                    BS.putStr (showItemInJson currentVal) >>
                        exitSuccess

                Left GetLTeXConfigValueNotFound ->
                    putStrLn "The configuration path was inaccesible." >>
                        exitFailure

                Left GetLTeXConfigValueConfigInvalid ->
                    putStrLn "Current configuration is invalid for this path." >>
                        exitFailure

    (SetCommand texFilePath (SomeConfigPath configPath) stringValue) ->
        withLTeXConfigAndProjectRoot texFilePath $ \(currentConf, projRoot) ->
            withValueTypeA configPath stringValue $ \jsonValue ->
                case setLTeXConfigValue configPath jsonValue currentConf of
                    Right newConfig -> do
                        writeLTeXConfig newConfig projRoot
                        putStrLn "The value of the path has been updated."
                        exitSuccess

                    Left SetLTeXConfigValueConfigInvalid ->
                        putStrLn "Current configuration is invalid for this path." >>
                            exitFailure

    (AddCommand texFilePath (SomeConfigPath configPath) stringValue) ->
        withLTeXConfigAndProjectRoot texFilePath $ \(currentConf, projRoot) ->
            withValueTypeB configPath stringValue $ \jsonValue ->
                case getLTeXConfigValue configPath currentConf of
                    Right currentVal ->
                        case doAppend currentVal jsonValue of
                            Right newVal -> do
                                Right newConfig <- pure $ setLTeXConfigValue configPath newVal currentConf
                                writeLTeXConfig newConfig projRoot
                                putStrLn "The value is added to the path."
                                exitSuccess

                            Left AppendNotSupported ->
                                putStrLn "Command add is not supported for this path." >>
                                    exitFailure

                            Left ItemAlreadyInList ->
                                putStrLn "The item is already registered." >>
                                    exitSuccess

                    Left GetLTeXConfigValueNotFound ->
                        case initialiseForAppend jsonValue of
                            Right newVal -> do
                                Right newConfig <- pure $ setLTeXConfigValue configPath newVal currentConf
                                writeLTeXConfig newConfig projRoot
                                putStrLn "The value is added to the path."
                                exitSuccess

                            Left AppendInitialiseNotSupported ->
                                putStrLn "Command add is not supported for this path." >>
                                    exitFailure
                            
                    Left GetLTeXConfigValueConfigInvalid ->
                        putStrLn "Current configuration is invalid for this path." >>
                            exitFailure

    (RemoveCommand texFilePath (SomeConfigPath configPath) stringValue) ->
        withLTeXConfigAndProjectRoot texFilePath $ \(currentConf, projRoot) ->
            withValueTypeB configPath stringValue $ \jsonValue ->
                case getLTeXConfigValue configPath currentConf of
                    Right currentVal ->
                        case doRemove currentVal jsonValue of
                            Right newVal -> do
                                Right newConfig <- pure $ setLTeXConfigValue configPath newVal currentConf
                                writeLTeXConfig newConfig projRoot
                                putStrLn "The value is removed from the path."
                                exitSuccess

                            Left RemoveNotSupported ->
                                putStrLn "Command remove is not supported for this path." >>
                                    exitFailure

                            Left ItemNotInList ->
                                putStrLn "The item was not registered." >>
                                    exitSuccess

                    Left GetLTeXConfigValueNotFound ->
                        putStrLn "The configuration path was inaccesible." >>
                            exitFailure

                    Left GetLTeXConfigValueConfigInvalid ->
                        putStrLn "Current configuration is invalid for this path." >>
                            exitFailure

    (ProjectRootCommand texFilePath) ->
        withProjectRoot texFilePath $ \(ProjectRootPath projRoot) ->
            putStrLn projRoot >>
                exitSuccess

    (WorkspaceConfigCommand texFilePath) ->
        withLTeXConfigAndProjectRoot texFilePath $ \(currentConf, _) ->
            BS.putStr (showConfigInJson currentConf) >>
                exitSuccess

