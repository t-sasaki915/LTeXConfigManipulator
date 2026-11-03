{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Types
    ( LTeXConfig
    , SomeConfigPath (..)
    , ConfigPath
    , AppendError (..)
    , RemoveError (..)
    , IsListActionDefined (..)
    , ConfigPathLensError (..)
    , withLTeXConfig
    , configPathFromText
    , getLTeXConfigValue
    , setLTeXConfigValue
    , withValueTypeA
    , withValueTypeB
    , showConfigInJson
    , showItemInJson
    ) where

import Control.Lens (at, view, set, lens, Lens')
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Exit (exitFailure)

import LTeXConfigTypes

data ConfigPath a b where
    Enabled                         :: ConfigPath LTeXEnabled                                                   LTeXEnabled
    Language                        :: ConfigPath LTeXNaturalOrAutoDetectLanguage                               ()
    Dictionary                      :: LTeXNaturalLanguage -> ConfigPath [Text]                                 Text
    DictionaryAll                   :: ConfigPath (Map LTeXNaturalLanguage [Text])                              (LTeXNaturalLanguage, [Text])
    DisabledRules                   :: LTeXNaturalLanguage -> ConfigPath [Text]                                 Text
    DisabledRulesAll                :: ConfigPath (Map LTeXNaturalLanguage [Text])                              (LTeXNaturalLanguage, [Text])
    EnabledRules                    :: LTeXNaturalLanguage -> ConfigPath [Text]                                 Text
    EnabledRulesAll                 :: ConfigPath (Map LTeXNaturalLanguage [Text])                              (LTeXNaturalLanguage, [Text])
    HiddenFalsePositives            :: LTeXNaturalLanguage -> ConfigPath [HiddenFalsePositivesValue]            HiddenFalsePositivesValue
    HiddenFalsePositivesAll         :: ConfigPath (Map LTeXNaturalLanguage [HiddenFalsePositivesValue])         (LTeXNaturalLanguage, [HiddenFalsePositivesValue])
    BibTeXAll                       :: ConfigPath BibTeXConfig                                                  ()
    BibTeXFields                    :: Text -> ConfigPath Bool                                                  ()
    BibTeXFieldsAll                 :: ConfigPath (Map Text Bool)                                               (Text, Bool)
    LaTeXAll                        :: ConfigPath LaTeXConfig                                                   ()
    LaTeXCommands                   :: Text -> ConfigPath LTeXAction                                            ()
    LaTeXCommandsAll                :: ConfigPath (Map Text LTeXAction)                                         (Text, LTeXAction)
    LaTeXEnvironments               :: Text -> ConfigPath LaTeXEnvironmentAction                                ()
    LaTeXEnvironmentsAll            :: ConfigPath (Map Text LaTeXEnvironmentAction)                             (Text, LaTeXEnvironmentAction)
    MarkdownAll                     :: ConfigPath MarkdownConfig                                                ()
    MarkdownNodes                   :: Text -> ConfigPath LTeXAction                                            ()
    MarkdownNodesAll                :: ConfigPath (Map Text LTeXAction)                                         (Text, LTeXAction)
    ConfigurationTarget             :: LTeXConfigurationTargetKey -> ConfigPath LTeXConfigurationTargetValue    ()
    ConfigurationTargetAll          :: ConfigPath (Map LTeXConfigurationTargetKey LTeXConfigurationTargetValue) (LTeXConfigurationTargetKey, LTeXConfigurationTargetValue)
    AdditionalRulesAll              :: ConfigPath AdditionalRules                                               ()
    AdditionalRulesEnablePickyRules :: ConfigPath Bool                                                          ()
    AdditionalRulesMotherTongue     :: ConfigPath LTeXNaturalOrNoneLanguage                                     ()
    AdditionalRulesLanguageModel    :: ConfigPath Text                                                          ()
    LanguageToolHttpServerUri       :: ConfigPath Text                                                          ()
    LanguageToolOrgAll              :: ConfigPath LanguageToolOrg                                               ()
    LanguageToolOrgUsername         :: ConfigPath Text                                                          ()
    LanguageToolOrgApiKey           :: ConfigPath Text                                                          ()
    LTeXLsAll                       :: ConfigPath LTeXLs                                                        ()
    LTeXLsPath                      :: ConfigPath Text                                                          ()
    LTeXLsLogLevel                  :: ConfigPath LTeXLsLogLevel                                                ()
    JavaAll                         :: ConfigPath JavaConfig                                                    ()
    JavaPath                        :: ConfigPath Text                                                          ()
    JavaInitialHeapSize             :: ConfigPath Int                                                           ()
    JavaMaximumHeapSize             :: ConfigPath Int                                                           ()
    SentenceCacheSize               :: ConfigPath Int                                                           ()
    MaxRequestSize                  :: ConfigPath Int                                                           ()
    ParagraphCacheTtlMinutes        :: ConfigPath Int                                                           ()
    ParagraphCacheEnabled           :: ConfigPath Bool                                                          ()
    CompletionEnabled               :: ConfigPath Bool                                                          ()
    DiagnosticSeverity              :: ConfigPath LTeXDiagnosticSeverity                                        ()
    CheckFrequency                  :: ConfigPath LTeXCheckFrequency                                            ()
    ClearDiagnosticsWhenClosingFile :: ConfigPath Bool                                                          ()
    StatusBarItem                   :: ConfigPath Bool                                                          ()
    TraceAll                        :: ConfigPath Trace                                                         ()
    TraceServer                     :: ConfigPath TraceServerMode                                               ()

data SomeConfigPath where
    SomeConfigPath :: (IsListActionDefined a b, ToJSON a, FromJSON a, ToJSON b, FromJSON b)
                   => ConfigPath a b
                   -> SomeConfigPath

data AppendError = AppendNotSupported | ItemAlreadyInList deriving (Show, Eq)
data RemoveError = RemoveNotSupported | ItemNotInList     deriving (Show, Eq)

class IsListActionDefined a b where
    doAppend :: a -> b -> Either AppendError a
    doRemove :: a -> b -> Either RemoveError a

instance IsListActionDefined LTeXEnabled LTeXEnabled where
    doAppend (LTeXEnabledArray a) (LTeXEnabledArray b) = Right (LTeXEnabledArray (a <> b))
    doAppend _ _                                       = Left AppendNotSupported
    doRemove (LTeXEnabledArray a) (LTeXEnabledArray b) = Right (LTeXEnabledArray (filter (`notElem` b) a))
    doRemove _ _                                       = Left RemoveNotSupported

instance IsListActionDefined LTeXNaturalOrAutoDetectLanguage () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined [Text] Text where
    doAppend = typicalAppendList
    doRemove = typicalRemoveList

instance IsListActionDefined (Map LTeXNaturalLanguage [Text]) (LTeXNaturalLanguage, [Text]) where
    doAppend = typicalAppendMap
    doRemove = typicalRemoveMap

instance IsListActionDefined [HiddenFalsePositivesValue] HiddenFalsePositivesValue where
    doAppend = typicalAppendList
    doRemove = typicalRemoveList

instance IsListActionDefined (Map LTeXNaturalLanguage [HiddenFalsePositivesValue]) (LTeXNaturalLanguage, [HiddenFalsePositivesValue]) where
    doAppend = typicalAppendMap
    doRemove = typicalRemoveMap

instance IsListActionDefined BibTeXConfig () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined Bool () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined (Map Text Bool) (Text, Bool) where
    doAppend = typicalAppendMap
    doRemove = typicalRemoveMap

instance IsListActionDefined LaTeXConfig () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined LTeXAction () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined (Map Text LTeXAction) (Text, LTeXAction) where
    doAppend = typicalAppendMap
    doRemove = typicalRemoveMap

instance IsListActionDefined LaTeXEnvironmentAction () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined (Map Text LaTeXEnvironmentAction) (Text, LaTeXEnvironmentAction) where
    doAppend = typicalAppendMap
    doRemove = typicalRemoveMap

instance IsListActionDefined MarkdownConfig () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined LTeXConfigurationTargetValue () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined (Map LTeXConfigurationTargetKey LTeXConfigurationTargetValue) (LTeXConfigurationTargetKey, LTeXConfigurationTargetValue)where
    doAppend = typicalAppendMap
    doRemove = typicalRemoveMap

instance IsListActionDefined AdditionalRules () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined LTeXNaturalOrNoneLanguage () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined Text () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined LanguageToolOrg () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined LTeXLs () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined LTeXLsLogLevel () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined JavaConfig () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined Int () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined LTeXDiagnosticSeverity () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined LTeXCheckFrequency () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined Trace () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined TraceServerMode () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove
    
configPathFromText :: Text -> Maybe SomeConfigPath
configPathFromText input =
    case Text.splitOn "." input of
        ["enabled"]                               -> Just (SomeConfigPath Enabled)
        ["language"]                              -> Just (SomeConfigPath Language)
        ["dictionary"]                            -> Just (SomeConfigPath DictionaryAll)
        ("dictionary" : rest : rest')             -> decodeRestAsNatLang (rest : rest') >>= pure . SomeConfigPath . Dictionary
        ["disabledRules"]                         -> Just (SomeConfigPath DisabledRulesAll)
        ("disabledRules" : rest : rest')          -> decodeRestAsNatLang (rest : rest') >>= pure . SomeConfigPath . DisabledRules
        ["enabledRules"]                          -> Just (SomeConfigPath EnabledRulesAll)
        ("enabledRules" : rest : rest')           -> decodeRestAsNatLang (rest : rest') >>= pure . SomeConfigPath . EnabledRules
        ["hiddenFalsePositives"]                  -> Just (SomeConfigPath HiddenFalsePositivesAll)
        ("hiddenFalsePositives" : rest : rest')   -> decodeRestAsNatLang (rest : rest') >>= pure . SomeConfigPath . HiddenFalsePositives
        ["bibtex"]                                -> Just (SomeConfigPath BibTeXAll)
        ["bibtex", "fields"]                      -> Just (SomeConfigPath BibTeXFieldsAll)
        ("bibtex" : "fields" : rest : rest')      -> Just (SomeConfigPath (BibTeXFields      (unsplitRest (rest : rest'))))
        ["latex"]                                 -> Just (SomeConfigPath LaTeXAll)
        ["latex", "commands"]                     -> Just (SomeConfigPath LaTeXCommandsAll)
        ("latex" : "commands" : rest : rest')     -> Just (SomeConfigPath (LaTeXCommands     (unsplitRest (rest : rest'))))
        ["latex", "environments"]                 -> Just (SomeConfigPath LaTeXEnvironmentsAll)
        ("latex" : "environments" : rest : rest') -> Just (SomeConfigPath (LaTeXEnvironments (unsplitRest (rest : rest'))))
        ["markdown"]                              -> Just (SomeConfigPath MarkdownAll)
        ["markdown", "nodes"]                     -> Just (SomeConfigPath MarkdownNodesAll)
        ("markdown" : "nodes" : rest : rest')     -> Just (SomeConfigPath (MarkdownNodes     (unsplitRest (rest : rest'))))
        ["configurationTarget"]                   -> Just (SomeConfigPath ConfigurationTargetAll)
        ("configurationTarget" : rest : rest')    -> decodeRestAsTargetKey (rest : rest') >>= pure . SomeConfigPath . ConfigurationTarget
        ["additionalRules"]                       -> Just (SomeConfigPath AdditionalRulesAll)
        ["additionalRules", "enablePickyRules"]   -> Just (SomeConfigPath AdditionalRulesEnablePickyRules)
        ["additionalRules", "motherTongue"]       -> Just (SomeConfigPath AdditionalRulesMotherTongue)
        ["additionalRules", "languageModel"]      -> Just (SomeConfigPath AdditionalRulesLanguageModel)
        ["languageToolHttpServerUri"]             -> Just (SomeConfigPath LanguageToolHttpServerUri)
        ["languageToolOrg"]                       -> Just (SomeConfigPath LanguageToolOrgAll)
        ["languageToolOrg", "username"]           -> Just (SomeConfigPath LanguageToolOrgUsername)
        ["languageToolOrg", "apiKey"]             -> Just (SomeConfigPath LanguageToolOrgApiKey)
        ["ltex-ls"]                               -> Just (SomeConfigPath LTeXLsAll)
        ["ltex-ls", "path"]                       -> Just (SomeConfigPath LTeXLsPath)
        ["ltex-ls", "logLevel"]                   -> Just (SomeConfigPath LTeXLsLogLevel)
        ["java"]                                  -> Just (SomeConfigPath JavaAll)
        ["java", "path"]                          -> Just (SomeConfigPath JavaPath)
        ["java", "initialHeapSize"]               -> Just (SomeConfigPath JavaInitialHeapSize)
        ["java", "maximumHeapSize"]               -> Just (SomeConfigPath JavaMaximumHeapSize)
        ["sentenceCacheSize"]                     -> Just (SomeConfigPath SentenceCacheSize)
        ["maxRequestSize"]                        -> Just (SomeConfigPath MaxRequestSize)
        ["paragraphCacheTtlMinutes"]              -> Just (SomeConfigPath ParagraphCacheTtlMinutes)
        ["paragraphCacheEnabled"]                 -> Just (SomeConfigPath ParagraphCacheEnabled)
        ["completionEnabled"]                     -> Just (SomeConfigPath CompletionEnabled)
        ["diagnosticSeverity"]                    -> Just (SomeConfigPath DiagnosticSeverity)
        ["checkFrequency"]                        -> Just (SomeConfigPath CheckFrequency)
        ["clearDiagnosticsWhenClosingFile"]       -> Just (SomeConfigPath ClearDiagnosticsWhenClosingFile)
        ["statusBarItem"]                         -> Just (SomeConfigPath StatusBarItem)
        ["trace"]                                 -> Just (SomeConfigPath TraceAll)
        ["trace", "server"]                       -> Just (SomeConfigPath TraceServer)
        _                                         -> Nothing
    where
        unsplitRest = Text.intercalate "."

        decodeRestAsNatLang   = parseMaybe parseJSON . String . unsplitRest
        decodeRestAsTargetKey = parseMaybe parseJSON . String . unsplitRest

data ConfigPathLensError = ValueNotFound | InvalidConfigForPath deriving (Show, Eq)

configPathLens :: ConfigPath a b -> LTeXConfig -> Either ConfigPathLensError (Lens' LTeXConfig a)
configPathLens confPath currentConf = case confPath of
    Enabled                         -> Right enabled
    Language                        -> Right language
    (Dictionary x)                  -> dictionary `atExisting` x
    DictionaryAll                   -> Right dictionary
    (DisabledRules x)               -> disabledRules `atExisting` x
    DisabledRulesAll                -> Right disabledRules
    (EnabledRules x)                -> enabledRules `atExisting` x
    EnabledRulesAll                 -> Right enabledRules
    (HiddenFalsePositives x)        -> hiddenFalsePositives `atExisting` x
    HiddenFalsePositivesAll         -> Right hiddenFalsePositives
    BibTeXAll                       -> Right bibtex
    (BibTeXFields x)                -> (bibtex . fields) `atExisting` x
    BibTeXFieldsAll                 -> Right (bibtex . fields)
    LaTeXAll                        -> Right latex
    (LaTeXCommands x)               -> (latex . commands) `atExisting` x
    LaTeXCommandsAll                -> Right (latex . commands)
    (LaTeXEnvironments x)           -> (latex . environments) `atExisting` x
    LaTeXEnvironmentsAll            -> Right (latex . environments)
    MarkdownAll                     -> Right markdown
    (MarkdownNodes x)               -> (markdown . nodes) `atExisting` x
    MarkdownNodesAll                -> Right (markdown . nodes)
    (ConfigurationTarget x)         -> configurationTarget `atExisting` x
    ConfigurationTargetAll          -> Right configurationTarget
    AdditionalRulesAll              -> Right additionalRules
    AdditionalRulesEnablePickyRules -> Right (additionalRules . enablePickyRules)
    AdditionalRulesMotherTongue     -> Right (additionalRules . motherTongue)
    AdditionalRulesLanguageModel    -> Right (additionalRules . languageModel)
    LanguageToolHttpServerUri       -> Right languageToolHttpServerUri
    LanguageToolOrgAll              -> Right languageToolOrg
    LanguageToolOrgUsername         -> Right (languageToolOrg . username)
    LanguageToolOrgApiKey           -> Right (languageToolOrg . apiKey)
    LTeXLsAll                       -> Right ltexLs
    LTeXLsPath                      -> Right (ltexLs . ltexLsPath)
    LTeXLsLogLevel                  -> Right (ltexLs . logLevel)
    JavaAll                         -> Right java
    JavaPath                        -> Right (java . javaPath)
    JavaInitialHeapSize             -> Right (java . initialHeapSize)
    JavaMaximumHeapSize             -> Right (java . maximumHeapSize)
    SentenceCacheSize               -> Right sentenceCacheSize
    MaxRequestSize                  -> Right maxRequestSize
    ParagraphCacheTtlMinutes        -> Right paragraphCacheTtlMinutes
    ParagraphCacheEnabled           -> Right paragraphCacheEnabled
    CompletionEnabled               -> Right completionEnabled
    DiagnosticSeverity              -> Right diagnosticSeverity
    CheckFrequency                  -> Right checkFrequency
    ClearDiagnosticsWhenClosingFile -> Right clearDiagnosticsWhenClosingFile
    StatusBarItem                   -> Right statusBarItem
    TraceAll                        -> Right trace
    TraceServer                     -> Right (trace . server)
    where
        atExisting :: Ord k
                   => Lens' LTeXConfig (Map k v)
                   -> k
                   -> Either ConfigPathLensError (Lens' LTeXConfig v)
        atExisting l key =
            case view (l . at key) currentConf of
                Nothing -> Left ValueNotFound
                Just _  -> Right $ lens
                    (\s' -> fromJust (view (l . at key) s'))
                    (\s' v' -> set (l . at key) (Just v') s')

getLTeXConfigOrDefault :: FilePath -> IO (Either String LTeXConfig)
getLTeXConfigOrDefault projRoot =
    let configFilePath = projRoot </> ".ltex" </> "config.json" in
        doesFileExist configFilePath >>= \case
            True ->
                BS.readFile configFilePath >>=
                    pure . eitherDecode

            False ->
                pure (Right defaultLTeXConfig)

withLTeXConfig :: FilePath -> (LTeXConfig -> IO ()) -> IO ()
withLTeXConfig projRoot programme =
    getLTeXConfigOrDefault projRoot >>= \case
        Right config -> programme config
        Left  err    -> putStrLn ("Failed to decode config file\n" <> err) >> exitFailure

getLTeXConfigValue :: ConfigPath a b -> LTeXConfig -> Either ConfigPathLensError a
getLTeXConfigValue configPath currentConf =
    configPathLens configPath currentConf >>= \pathLens ->
        Right (view pathLens currentConf)

setLTeXConfigValue :: ConfigPath a b -> a -> LTeXConfig -> Either ConfigPathLensError LTeXConfig
setLTeXConfigValue configPath newValue currentConf =
    configPathLens configPath currentConf >>= \pathLens ->
        Right (set pathLens newValue currentConf)

parseConfigValue :: FromJSON a => Text -> Either String a
parseConfigValue input =
    case eitherDecodeStrict (encodeUtf8 input) of
        Right jsonValue ->
            case fromJSON jsonValue of
                Success val -> Right val
                Error   err -> Left  err

        Left _ ->
            case fromJSON (String input) of
                Success val -> Right val
                Error   err -> Left  err

withValueTypeA :: FromJSON a => ConfigPath a b -> Text -> (a -> IO ()) -> IO ()
withValueTypeA _ stringValue programme =
    case parseConfigValue stringValue of
        Right jsonValue -> programme jsonValue
        Left  err       -> putStrLn ("Failed to parse input:\n" <> err) >> exitFailure

withValueTypeB :: FromJSON b => ConfigPath a b -> Text -> (b -> IO ()) -> IO ()
withValueTypeB _ stringValue programme =
    case parseConfigValue stringValue of
        Right jsonValue -> programme jsonValue
        Left  err       -> putStrLn ("Failed to parse input:\n" <> err) >> exitFailure

showConfigInJson :: LTeXConfig -> ByteString
showConfigInJson = encode

showItemInJson :: ToJSON a => a -> ByteString
showItemInJson = encode

unsupportedAppend :: a -> b -> Either AppendError a
unsupportedAppend _ _ = Left AppendNotSupported

unsupportedRemove :: a -> b -> Either RemoveError a
unsupportedRemove _ _ = Left RemoveNotSupported

typicalAppendList :: Eq a => [a] -> a -> Either AppendError [a]
typicalAppendList list newItem =
    case newItem `elem` list of
        False -> Right (list <> [newItem])
        True  -> Left ItemAlreadyInList

typicalRemoveList :: Eq a => [a] -> a -> Either RemoveError [a]
typicalRemoveList list targetItem =
    case targetItem `elem` list of
        True  -> Right (filter (/= targetItem) list)
        False -> Left ItemNotInList

typicalAppendMap :: Ord a => Map a b -> (a, b) -> Either AppendError (Map a b)
typicalAppendMap currentMap (newKey, newVal) =
    case newKey `Map.member` currentMap of
        False -> Right (Map.insert newKey newVal currentMap)
        True  -> Left ItemAlreadyInList

typicalRemoveMap :: Ord a => Map a b -> (a, b) -> Either RemoveError (Map a b)
typicalRemoveMap currentMap (targetKey, _) =
    case targetKey `Map.member` currentMap of
        True  -> Right (Map.delete targetKey currentMap)
        False -> Left ItemNotInList
        

