module Main (main) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative hiding (Success)
import System.Directory
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory)

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

getLTeXConfig :: ProjectRootPath -> IO (Either String LTeXConfig)
getLTeXConfig (ProjectRootPath projRoot) =
    let configFilePath = projRoot </> ".ltex" </> "config.json" in
        doesFileExist configFilePath >>= \case
            True ->
                BS.readFile configFilePath >>=
                    pure . eitherDecode

            False ->
                pure (Right defaultLTeXConfig)

withProjectRoot :: TeXFilePath -> (ProjectRootPath -> IO ()) -> IO ()
withProjectRoot texFilePath programme =
    searchForProjectRoot texFilePath >>=
        programme

withLTeXConfig :: TeXFilePath -> ((LTeXConfig, ProjectRootPath) -> IO()) -> IO ()
withLTeXConfig texFilePath programme =
    withProjectRoot texFilePath $ \projRoot ->
        getLTeXConfig projRoot >>= \case
            Right config -> programme (config, projRoot)
            Left err     -> putStrLn ("Failed to decode config file\n" <> err) >> exitFailure

data Command = GetCommand         TeXFilePath SomeConfigPath
             | SetCommand         TeXFilePath SomeConfigPath Text
             | AddCommand         TeXFilePath SomeConfigPath Text
             | RemoveCommand      TeXFilePath SomeConfigPath Text
             | ProjectRootCommand TeXFilePath

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
        )

commandLineParser :: ParserInfo Command
commandLineParser =
    info (commandParser <**> helper)
        (fullDesc <> progDesc "A command-line tool for managing project-specific LTeX+ configurations with a focus on Vim integration.")

main :: IO ()
main = execParser commandLineParser >>= \case
    (GetCommand texFilePath configPath) ->
        withLTeXConfig texFilePath $ \(currentConf, projRoot) ->
            putStrLn "GET" >> print currentConf

    (SetCommand texFilePath configPath configValue) ->
        withLTeXConfig texFilePath $ \(currentConf, projRoot) ->
            putStrLn "SET" >> print currentConf

    (AddCommand texFilePath configPath configValue) ->
        withLTeXConfig texFilePath $ \(currentConf, projRoot) ->
            putStrLn "ADD" >> print currentConf

    (RemoveCommand texFilePath configPath configValue) ->
        withLTeXConfig texFilePath $ \(currentConf, projRoot) ->
            putStrLn "REMOVE" >> print currentConf

    (ProjectRootCommand texFilePath) ->
        withProjectRoot texFilePath $ \(ProjectRootPath projRoot) ->
            putStrLn projRoot

