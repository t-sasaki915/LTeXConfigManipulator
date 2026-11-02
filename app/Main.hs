module Main (main) where

import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative hiding (Success)
import System.Directory
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

withProjectRoot :: TeXFilePath -> (ProjectRootPath -> IO ()) -> IO ()
withProjectRoot texFilePath programme =
    searchForProjectRoot texFilePath >>=
        programme

withLTeXConfigAndProjectRoot :: TeXFilePath -> ((LTeXConfig, ProjectRootPath) -> IO()) -> IO ()
withLTeXConfigAndProjectRoot texFilePath programme =
    withProjectRoot texFilePath $ \(ProjectRootPath projRoot) ->
        withLTeXConfig projRoot $ \ltexConfig ->
            programme (ltexConfig, ProjectRootPath projRoot)

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
            BS.putStr $ showItemInJson (getLTeXConfigValue configPath currentConf)

    (SetCommand texFilePath (SomeConfigPath configPath) configValue) ->
        withLTeXConfigAndProjectRoot texFilePath $ \(currentConf, _) ->
            withValueTypeA configPath configValue $ \configValue' ->
                BS.putStr $ showConfigInJson (setLTeXConfigValue configPath configValue' currentConf)

    (AddCommand texFilePath (SomeConfigPath configPath) configValue) ->
        withLTeXConfigAndProjectRoot texFilePath $ \(currentConf, _) ->
            withValueTypeB configPath configValue $ \configValue' ->
                case doAppend (getLTeXConfigValue configPath currentConf) configValue' of
                    Right val' ->
                        BS.putStr $ showConfigInJson (setLTeXConfigValue configPath val' currentConf)

                    Left AppendNotSupported ->
                        putStrLn "APPEND NOT SUPPORTED!!"

                    Left ItemAlreadyInList ->
                        putStrLn "ALREADY IN LIST!!"

    (RemoveCommand texFilePath (SomeConfigPath configPath) configValue) ->
        withLTeXConfigAndProjectRoot texFilePath $ \(currentConf, _) ->
            withValueTypeB configPath configValue $ \configValue' ->
                case doRemove (getLTeXConfigValue configPath currentConf) configValue' of
                    Right val' ->
                        BS.putStr $ showConfigInJson (setLTeXConfigValue configPath val' currentConf)

                    Left RemoveNotSupported ->
                        putStrLn "REMOVE NOT SUPPORTED!!"

                    Left ItemNotInList ->
                        putStrLn "ITEM NOT IN LIST!!"

    (ProjectRootCommand texFilePath) ->
        withProjectRoot texFilePath $ \(ProjectRootPath projRoot) ->
            putStrLn projRoot

    (WorkspaceConfigCommand texFilePath) ->
        withLTeXConfigAndProjectRoot texFilePath $ \(currentConf, _) ->
            BS.putStr (showConfigInJson currentConf)

