module Main (main) where

import Data.Aeson
import Data.Aeson.Types (prependFailure, unexpected)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Options.Applicative hiding (Success)
import System.Directory
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory)

data LTeXLanguage = Arabic | Asturian | Belarusian | Breton | Catalan | CatalanValencian | Chinese
                  | Danish | Dutch | DutchBelgium | English | EnglishAustralian | EnglishCanadian
                  | EnglishGB | EnglishNewZealand | EnglishSouthAfrican | EnglishUS | Esperanto
                  | French | Galician | German | GermanAustria | GermanGermany | GermanSwiss
                  | Greek | Irish | Italian | Japanese | Khmer | Persian | Polish | Portuguese
                  | PortugueseAngolaPreAO | PortugueseBrazil | PortugueseMozambiquePreAO
                  | PortuguesePortugal | Romanian | Russian | SimpleGerman | Slovak | Slovenian
                  | Spanish | SpanishVoseo | Swedish | Tagalog | Tamil | Ukrainian
                  deriving (Show, Eq)

instance ToJSON LTeXLanguage where
    toJSON Arabic                    = String "ar"
    toJSON Asturian                  = String "ast-ES"
    toJSON Belarusian                = String "be-BY"
    toJSON Breton                    = String "br-FR"
    toJSON Catalan                   = String "ca-ES"
    toJSON CatalanValencian          = String "ca-ES-valencia"
    toJSON Chinese                   = String "zh-CN"
    toJSON Danish                    = String "da-DK"
    toJSON Dutch                     = String "nl"
    toJSON DutchBelgium              = String "nl-BE"
    toJSON English                   = String "en"
    toJSON EnglishAustralian         = String "en-AU"
    toJSON EnglishCanadian           = String "en-CA"
    toJSON EnglishGB                 = String "en-GB"
    toJSON EnglishNewZealand         = String "en-NZ"
    toJSON EnglishSouthAfrican       = String "en-ZA"
    toJSON EnglishUS                 = String "en-US"
    toJSON Esperanto                 = String "eo"
    toJSON French                    = String "fr"
    toJSON Galician                  = String "gl-ES"
    toJSON German                    = String "de"
    toJSON GermanAustria             = String "de-AT"
    toJSON GermanGermany             = String "de-DE"
    toJSON GermanSwiss               = String "de-CH"
    toJSON Greek                     = String "el-GR"
    toJSON Irish                     = String "ga-IE"
    toJSON Italian                   = String "it"
    toJSON Japanese                  = String "ja-JP"
    toJSON Khmer                     = String "km-KH"
    toJSON Persian                   = String "fa"
    toJSON Polish                    = String "pl-PL"
    toJSON Portuguese                = String "pt"
    toJSON PortugueseAngolaPreAO     = String "pt-AO"
    toJSON PortugueseBrazil          = String "pt-BR"
    toJSON PortugueseMozambiquePreAO = String "pt-MZ"
    toJSON PortuguesePortugal        = String "pt-PT"
    toJSON Romanian                  = String "ro-RO"
    toJSON Russian                   = String "ru-RU" 
    toJSON SimpleGerman              = String "de-DE-x-simple-language"
    toJSON Slovak                    = String "sk-SK"
    toJSON Slovenian                 = String "sl-SI"
    toJSON Spanish                   = String "es"
    toJSON SpanishVoseo              = String "es-AR"
    toJSON Swedish                   = String "sv"
    toJSON Tagalog                   = String "tl-PH"
    toJSON Tamil                     = String "ta-IN"
    toJSON Ukrainian                 = String "uk-UA"

instance FromJSON LTeXLanguage where
    parseJSON (String "ar")                      = pure Arabic
    parseJSON (String "ast-ES")                  = pure Asturian
    parseJSON (String "be-BY")                   = pure Belarusian
    parseJSON (String "br-FR")                   = pure Breton
    parseJSON (String "ca-ES")                   = pure Catalan
    parseJSON (String "ca-ES-valencia")          = pure CatalanValencian
    parseJSON (String "zh-CN")                   = pure Chinese
    parseJSON (String "da-DK")                   = pure Danish
    parseJSON (String "nl")                      = pure Dutch
    parseJSON (String "nl-BE")                   = pure DutchBelgium
    parseJSON (String "en")                      = pure English
    parseJSON (String "en-AU")                   = pure EnglishAustralian
    parseJSON (String "en-CA")                   = pure EnglishCanadian
    parseJSON (String "en-GB")                   = pure EnglishGB
    parseJSON (String "en-NZ")                   = pure EnglishNewZealand
    parseJSON (String "en-ZA")                   = pure EnglishSouthAfrican
    parseJSON (String "en-US")                   = pure EnglishUS
    parseJSON (String "eo")                      = pure Esperanto
    parseJSON (String "fr")                      = pure French
    parseJSON (String "gl-ES")                   = pure Galician
    parseJSON (String "de")                      = pure German
    parseJSON (String "de-AT")                   = pure GermanAustria
    parseJSON (String "de-DE")                   = pure GermanGermany
    parseJSON (String "de-CH")                   = pure GermanSwiss
    parseJSON (String "el-GR")                   = pure Greek
    parseJSON (String "ga-IE")                   = pure Irish
    parseJSON (String "it")                      = pure Italian
    parseJSON (String "ja-JP")                   = pure Japanese
    parseJSON (String "km-KH")                   = pure Khmer
    parseJSON (String "fa")                      = pure Persian
    parseJSON (String "pl-PL")                   = pure Polish
    parseJSON (String "pt")                      = pure Portuguese
    parseJSON (String "pt-AO")                   = pure PortugueseAngolaPreAO
    parseJSON (String "pt-BR")                   = pure PortugueseBrazil
    parseJSON (String "pt-MZ")                   = pure PortugueseMozambiquePreAO
    parseJSON (String "pt-PT")                   = pure PortuguesePortugal
    parseJSON (String "ro-RO")                   = pure Romanian
    parseJSON (String "ru-RU")                   = pure Russian
    parseJSON (String "de-DE-x-simple-language") = pure SimpleGerman
    parseJSON (String "sk-SK")                   = pure Slovak
    parseJSON (String "sl-SI")                   = pure Slovenian
    parseJSON (String "es")                      = pure Spanish
    parseJSON (String "es-AR")                   = pure SpanishVoseo
    parseJSON (String "sv")                      = pure Swedish
    parseJSON (String "tl-PH")                   = pure Tagalog
    parseJSON (String "ta-IN")                   = pure Tamil
    parseJSON (String "uk-UA")                   = pure Ukrainian

    parseJSON invalid = prependFailure "parsing LTeXLanguage failed, " (unexpected invalid)

parseLTeXLanguage :: String -> Either String LTeXLanguage
parseLTeXLanguage s =
    case fromJSON (String (Text.pack s)) of
        Success a -> Right a
        Error err -> Left err

data LTeXConfig = LTeXConfig
    { language   :: LTeXLanguage
    , dictionary :: [Text]
    }
    deriving (Show, Eq)

instance ToJSON LTeXConfig where
    toJSON conf = object ["language" .= (language conf), "dictionary" .= (dictionary conf)]

instance FromJSON LTeXConfig where
    parseJSON = withObject "LTeXConfig" $ \v -> LTeXConfig
        <$> v .: "language"
        <*> v .: "dictionary"

defaultLTeXConfig :: LTeXConfig
defaultLTeXConfig = LTeXConfig
    { language   = EnglishGB
    , dictionary = []
    }

type TexFilePath     = FilePath
type ProjectRootPath = FilePath

data ShowConfigEntry = ShowAll | ShowLanguage | ShowDictionary deriving (Show, Eq)

parseShowConfigEntry :: String -> Either String ShowConfigEntry
parseShowConfigEntry "all"        = Right ShowAll
parseShowConfigEntry "language"   = Right ShowLanguage
parseShowConfigEntry "dictionary" = Right ShowDictionary
parseShowConfigEntry x            = Left ("Unexpected argument: " <> x)

texFileOptionParser :: Parser TexFilePath
texFileOptionParser = strOption
    ( long "texFile"
   <> metavar "FILEPATH"
   <> help "The path to the .tex (or .bib) file that you are currently editing"
    )

showConfigOptionParser :: Parser (TexFilePath, ShowConfigEntry)
showConfigOptionParser = (,)
    <$> texFileOptionParser
    <*> argument (eitherReader parseShowConfigEntry)
            (metavar "ENTRY" <> help "Config entry to show (all / language / dictionary)")

setLanguageOptionParser :: Parser (TexFilePath, LTeXLanguage)
setLanguageOptionParser = (,)
    <$> texFileOptionParser
    <*> argument (eitherReader parseLTeXLanguage)
            (metavar "LANGUAGE" <> help "Language to set (e.g. en-GB, ja-JP)")

registerCorrectWordOptionParser :: Parser (TexFilePath, Text)
registerCorrectWordOptionParser = (,)
    <$> texFileOptionParser
    <*> strArgument (metavar "WORD" <> help "Word to register as correct")

registerWrongWordOptionParser :: Parser (TexFilePath, Text)
registerWrongWordOptionParser = (,)
    <$> texFileOptionParser
    <*> strArgument (metavar "WORD" <> help "Word to register as wrong")

commandParser :: Parser (IO ())
commandParser =
    hsubparser
        ( command "projectRoot"
            (info (showProjectRoot <$> texFileOptionParser)
                (progDesc "Show project root"))

       <> command "show"
            (info (showConfig <$> showConfigOptionParser)
                (progDesc "Show current configuration"))

       <> command "language"
            (info (setLanguage <$> setLanguageOptionParser)
                (progDesc "Set LTeX language"))

       <> command "correctWord"
            (info (registerCorrectWord <$> registerCorrectWordOptionParser)
                (progDesc "Register a word to dictionary"))

       <> command "wrongWord"
            (info (registerWrongWord <$> registerWrongWordOptionParser)
                (progDesc "Unregister a word from dictionary"))
        )

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

searchForProjectRoot :: TexFilePath -> IO ProjectRootPath
searchForProjectRoot texFilePath = do
    absolutePath  <- makeAbsolute texFilePath
    canonicalPath <- canonicalizePath absolutePath
    let startDirectory = takeDirectory canonicalPath

    searchDirectoryUpwards hasLTeXFile startDirectory >>= \case
        Just projRoot -> pure projRoot
        Nothing       ->
            searchDirectoryUpwards hasGitFile startDirectory >>= \case
                Just projRoot -> pure projRoot
                Nothing       -> pure startDirectory

getLTeXConfig :: ProjectRootPath -> IO (Either String LTeXConfig)
getLTeXConfig projRoot =
    let configFilePath = projRoot </> ".ltex" </> "config.json" in
        doesFileExist configFilePath >>= \case
            True ->
                BS.readFile configFilePath >>=
                    pure . eitherDecode

            False ->
                pure (Right defaultLTeXConfig)

writeLTeXConfig :: ProjectRootPath -> LTeXConfig -> IO ()
writeLTeXConfig projRoot newConfig =
    let configFileDir = projRoot </> ".ltex"
        configFilePath = configFileDir </> "config.json" in
            createDirectoryIfMissing True configFileDir >>
                BS.writeFile configFilePath (encode newConfig)

withProjectRoot :: TexFilePath -> (FilePath -> IO ()) -> IO ()
withProjectRoot texFilePath programme =
    searchForProjectRoot texFilePath >>=
        programme

withLTeXConfig :: TexFilePath -> ((LTeXConfig, ProjectRootPath) -> IO()) -> IO ()
withLTeXConfig texFilePath programme =
    withProjectRoot texFilePath $ \projRoot ->
        getLTeXConfig projRoot >>= \case
            Right config -> programme (config, projRoot)
            Left err     -> putStrLn ("Failed to decode config file\n" <> err) >> exitFailure

showProjectRoot :: TexFilePath -> IO ()
showProjectRoot texFilePath = withProjectRoot texFilePath putStrLn

showConfig :: (TexFilePath, ShowConfigEntry) -> IO ()
showConfig (texFilePath, showConfigEntry) =
    withLTeXConfig texFilePath $ \(ltexConfig, _) ->
        case showConfigEntry of
            ShowAll ->
               BS.putStr (encode ltexConfig) 

            ShowLanguage ->
                case toJSON (language ltexConfig) of
                    String text -> TextIO.putStrLn text
                    _           -> undefined

            ShowDictionary ->
                mapM_ TextIO.putStrLn (dictionary ltexConfig)

setLanguage :: (TexFilePath, LTeXLanguage) -> IO ()
setLanguage (texFilePath, newLanguage) =
    withLTeXConfig texFilePath $ \(oldConfig, projRoot) ->
        let newConfig = oldConfig { language = newLanguage } in
            writeLTeXConfig projRoot newConfig

registerCorrectWord :: (TexFilePath, Text) -> IO ()
registerCorrectWord x = putStrLn $ "registerCorrectWord " <> show x

registerWrongWord :: (TexFilePath, Text) -> IO ()
registerWrongWord x = putStrLn $ "registerWrongWord " <> show x

main :: IO ()
main = do
    programme <- execParser opts

    programme
    where
        opts = info (commandParser <**> helper)
            (fullDesc <> progDesc "A small tool for vim-lsp to manipulate ltex configuration.")

