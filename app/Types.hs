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
    , AppendInitialiseError (..)
    , IsListActionDefined (..)
    , GetLTeXConfigValueError (..)
    , SetLTeXConfigValueError (..)
    , configPathFromText
    , getLTeXConfigValue
    , setLTeXConfigValue
    , withValueTypeA
    , withValueTypeB
    , removeDefaultValues
    , parseLTeXConfig
    , defaultLTeXConfig
    , showConfigInJson
    , showItemInJson
    ) where

import Control.Lens (at, view, set, lens, Lens')
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import System.Exit (exitFailure)

import LTeXConfigTypes

data LTeXCurrentOrNaturalLanguage = LTeXCurrentOrNaturalLanguageCurrent
                                  | LTeXCurrentOrNaturalLanguageNatural LTeXNaturalLanguage
                                  deriving (Show, Eq)

data ConfigPath a b where
    Enabled                         :: ConfigPath LTeXEnabled                                                   LTeXMarkupOrProgrammingLanguage
    Language                        :: ConfigPath LTeXNaturalOrAutoDetectLanguage                               ()
    Dictionary                      :: LTeXCurrentOrNaturalLanguage -> ConfigPath [Text]                        Text
    DictionaryAll                   :: ConfigPath (Map LTeXNaturalLanguage [Text])                              (LTeXNaturalLanguage, [Text])
    DisabledRules                   :: LTeXCurrentOrNaturalLanguage -> ConfigPath [Text]                        Text
    DisabledRulesAll                :: ConfigPath (Map LTeXNaturalLanguage [Text])                              (LTeXNaturalLanguage, [Text])
    EnabledRules                    :: LTeXCurrentOrNaturalLanguage -> ConfigPath [Text]                        Text
    EnabledRulesAll                 :: ConfigPath (Map LTeXNaturalLanguage [Text])                              (LTeXNaturalLanguage, [Text])
    HiddenFalsePositives            :: LTeXCurrentOrNaturalLanguage -> ConfigPath [HiddenFalsePositivesValue]   HiddenFalsePositivesValue
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

data AppendError           = AppendNotSupported | ItemAlreadyInList deriving (Show, Eq)
data RemoveError           = RemoveNotSupported | ItemNotInList     deriving (Show, Eq)
data AppendInitialiseError = AppendInitialiseNotSupported           deriving (Show, Eq)

class IsListActionDefined a b where
    doAppend            :: a -> b -> Either AppendError a
    doRemove            :: a -> b -> Either RemoveError a
    initialiseForAppend :: b -> Either AppendInitialiseError a

instance IsListActionDefined LTeXEnabled LTeXMarkupOrProgrammingLanguage where
    doAppend (LTeXEnabledArray a) b = LTeXEnabledArray <$> typicalAppendList a b
    doAppend _ _                    = Left AppendNotSupported
    doRemove (LTeXEnabledArray a) b = LTeXEnabledArray <$> typicalRemoveList a b
    doRemove _ _                    = Left RemoveNotSupported
    initialiseForAppend b           = Right (LTeXEnabledArray [b])

instance IsListActionDefined LTeXNaturalOrAutoDetectLanguage () where
    doAppend            = unsupportedAppend
    doRemove            = unsupportedRemove
    initialiseForAppend = unsupportedInitialiseAppend

instance IsListActionDefined [Text] Text where
    doAppend            = typicalAppendList
    doRemove            = typicalRemoveList
    initialiseForAppend = Right . pure

instance IsListActionDefined (Map LTeXNaturalLanguage [Text]) (LTeXNaturalLanguage, [Text]) where
    doAppend            = typicalAppendMap
    doRemove            = typicalRemoveMap
    initialiseForAppend = Right . makeMapFromPair

instance IsListActionDefined [HiddenFalsePositivesValue] HiddenFalsePositivesValue where
    doAppend            = typicalAppendList
    doRemove            = typicalRemoveList
    initialiseForAppend = Right . pure

instance IsListActionDefined (Map LTeXNaturalLanguage [HiddenFalsePositivesValue]) (LTeXNaturalLanguage, [HiddenFalsePositivesValue]) where
    doAppend            = typicalAppendMap
    doRemove            = typicalRemoveMap
    initialiseForAppend = Right . makeMapFromPair

instance IsListActionDefined BibTeXConfig () where
    doAppend            = unsupportedAppend
    doRemove            = unsupportedRemove
    initialiseForAppend = unsupportedInitialiseAppend

instance IsListActionDefined Bool () where
    doAppend            = unsupportedAppend
    doRemove            = unsupportedRemove
    initialiseForAppend = unsupportedInitialiseAppend

instance IsListActionDefined (Map Text Bool) (Text, Bool) where
    doAppend            = typicalAppendMap
    doRemove            = typicalRemoveMap
    initialiseForAppend = Right . makeMapFromPair

instance IsListActionDefined LaTeXConfig () where
    doAppend            = unsupportedAppend
    doRemove            = unsupportedRemove
    initialiseForAppend = unsupportedInitialiseAppend

instance IsListActionDefined LTeXAction () where
    doAppend            = unsupportedAppend
    doRemove            = unsupportedRemove
    initialiseForAppend = unsupportedInitialiseAppend

instance IsListActionDefined (Map Text LTeXAction) (Text, LTeXAction) where
    doAppend            = typicalAppendMap
    doRemove            = typicalRemoveMap
    initialiseForAppend = Right . makeMapFromPair

instance IsListActionDefined LaTeXEnvironmentAction () where
    doAppend            = unsupportedAppend
    doRemove            = unsupportedRemove
    initialiseForAppend = unsupportedInitialiseAppend

instance IsListActionDefined (Map Text LaTeXEnvironmentAction) (Text, LaTeXEnvironmentAction) where
    doAppend            = typicalAppendMap
    doRemove            = typicalRemoveMap
    initialiseForAppend = Right . makeMapFromPair

instance IsListActionDefined MarkdownConfig () where
    doAppend            = unsupportedAppend
    doRemove            = unsupportedRemove
    initialiseForAppend = unsupportedInitialiseAppend

instance IsListActionDefined LTeXConfigurationTargetValue () where
    doAppend            = unsupportedAppend
    doRemove            = unsupportedRemove
    initialiseForAppend = unsupportedInitialiseAppend

instance IsListActionDefined (Map LTeXConfigurationTargetKey LTeXConfigurationTargetValue) (LTeXConfigurationTargetKey, LTeXConfigurationTargetValue)where
    doAppend            = typicalAppendMap
    doRemove            = typicalRemoveMap
    initialiseForAppend = Right . makeMapFromPair

instance IsListActionDefined AdditionalRules () where
    doAppend            = unsupportedAppend
    doRemove            = unsupportedRemove
    initialiseForAppend = unsupportedInitialiseAppend

instance IsListActionDefined LTeXNaturalOrNoneLanguage () where
    doAppend            = unsupportedAppend
    doRemove            = unsupportedRemove
    initialiseForAppend = unsupportedInitialiseAppend

instance IsListActionDefined Text () where
    doAppend            = unsupportedAppend
    doRemove            = unsupportedRemove
    initialiseForAppend = unsupportedInitialiseAppend

instance IsListActionDefined LanguageToolOrg () where
    doAppend            = unsupportedAppend
    doRemove            = unsupportedRemove
    initialiseForAppend = unsupportedInitialiseAppend

instance IsListActionDefined LTeXLs () where
    doAppend            = unsupportedAppend
    doRemove            = unsupportedRemove
    initialiseForAppend = unsupportedInitialiseAppend

instance IsListActionDefined LTeXLsLogLevel () where
    doAppend            = unsupportedAppend
    doRemove            = unsupportedRemove
    initialiseForAppend = unsupportedInitialiseAppend

instance IsListActionDefined JavaConfig () where
    doAppend            = unsupportedAppend
    doRemove            = unsupportedRemove
    initialiseForAppend = unsupportedInitialiseAppend

instance IsListActionDefined Int () where
    doAppend            = unsupportedAppend
    doRemove            = unsupportedRemove
    initialiseForAppend = unsupportedInitialiseAppend

instance IsListActionDefined LTeXDiagnosticSeverity () where
    doAppend            = unsupportedAppend
    doRemove            = unsupportedRemove
    initialiseForAppend = unsupportedInitialiseAppend

instance IsListActionDefined LTeXCheckFrequency () where
    doAppend            = unsupportedAppend
    doRemove            = unsupportedRemove
    initialiseForAppend = unsupportedInitialiseAppend

instance IsListActionDefined Trace () where
    doAppend            = unsupportedAppend
    doRemove            = unsupportedRemove
    initialiseForAppend = unsupportedInitialiseAppend

instance IsListActionDefined TraceServerMode () where
    doAppend            = unsupportedAppend
    doRemove            = unsupportedRemove
    initialiseForAppend = unsupportedInitialiseAppend
    
configPathFromText :: Text -> Maybe SomeConfigPath
configPathFromText input =
    case Text.splitOn "." input of
        ["enabled"]                               -> Just (SomeConfigPath Enabled)
        ["language"]                              -> Just (SomeConfigPath Language)
        ["dictionary"]                            -> Just (SomeConfigPath DictionaryAll)
        ["dictionary", "currentLang"]             -> Just (SomeConfigPath (Dictionary LTeXCurrentOrNaturalLanguageCurrent))
        ("dictionary" : rest : rest')             -> decodeRestAsNatLang (rest : rest') >>= pure . SomeConfigPath . Dictionary . LTeXCurrentOrNaturalLanguageNatural
        ["disabledRules"]                         -> Just (SomeConfigPath DisabledRulesAll)
        ["disabledRules", "currentLang"]          -> Just (SomeConfigPath (DisabledRules LTeXCurrentOrNaturalLanguageCurrent))
        ("disabledRules" : rest : rest')          -> decodeRestAsNatLang (rest : rest') >>= pure . SomeConfigPath . DisabledRules . LTeXCurrentOrNaturalLanguageNatural
        ["enabledRules"]                          -> Just (SomeConfigPath EnabledRulesAll)
        ["enabledRules", "currentLang"]           -> Just (SomeConfigPath (EnabledRules LTeXCurrentOrNaturalLanguageCurrent))
        ("enabledRules" : rest : rest')           -> decodeRestAsNatLang (rest : rest') >>= pure . SomeConfigPath . EnabledRules . LTeXCurrentOrNaturalLanguageNatural
        ["hiddenFalsePositives"]                  -> Just (SomeConfigPath HiddenFalsePositivesAll)
        ["hiddenFalsePositives", "currentLang"]   -> Just (SomeConfigPath (HiddenFalsePositives LTeXCurrentOrNaturalLanguageCurrent))
        ("hiddenFalsePositives" : rest : rest')   -> decodeRestAsNatLang (rest : rest') >>= pure . SomeConfigPath . HiddenFalsePositives . LTeXCurrentOrNaturalLanguageNatural
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

type LensSetter a = (a -> LTeXConfig -> LTeXConfig)

data ConfigPathAccess a = PathFound     (Lens' LTeXConfig a)
                        | ValueNotFound (LensSetter a)
                        | InvalidConfigForPath

accessConfigPath :: ConfigPath a b -> LTeXConfig -> ConfigPathAccess a
accessConfigPath confPath currentConf = case confPath of
    Enabled                         -> PathFound enabled
    Language                        -> PathFound language
    (Dictionary x)                  -> resolveCurrentOrNatLang x (dictionary `atExisting`)
    DictionaryAll                   -> PathFound dictionary
    (DisabledRules x)               -> resolveCurrentOrNatLang x (disabledRules `atExisting`)
    DisabledRulesAll                -> PathFound disabledRules
    (EnabledRules x)                -> resolveCurrentOrNatLang x (enabledRules `atExisting`)
    EnabledRulesAll                 -> PathFound enabledRules
    (HiddenFalsePositives x)        -> resolveCurrentOrNatLang x (hiddenFalsePositives `atExisting`)
    HiddenFalsePositivesAll         -> PathFound hiddenFalsePositives
    BibTeXAll                       -> PathFound bibtex
    (BibTeXFields x)                -> (bibtex . fields) `atExisting` x
    BibTeXFieldsAll                 -> PathFound (bibtex . fields)
    LaTeXAll                        -> PathFound latex
    (LaTeXCommands x)               -> (latex . commands) `atExisting` x
    LaTeXCommandsAll                -> PathFound (latex . commands)
    (LaTeXEnvironments x)           -> (latex . environments) `atExisting` x
    LaTeXEnvironmentsAll            -> PathFound (latex . environments)
    MarkdownAll                     -> PathFound markdown
    (MarkdownNodes x)               -> (markdown . nodes) `atExisting` x
    MarkdownNodesAll                -> PathFound (markdown . nodes)
    (ConfigurationTarget x)         -> configurationTarget `atExisting` x
    ConfigurationTargetAll          -> PathFound configurationTarget
    AdditionalRulesAll              -> PathFound additionalRules
    AdditionalRulesEnablePickyRules -> PathFound (additionalRules . enablePickyRules)
    AdditionalRulesMotherTongue     -> PathFound (additionalRules . motherTongue)
    AdditionalRulesLanguageModel    -> PathFound (additionalRules . languageModel)
    LanguageToolHttpServerUri       -> PathFound languageToolHttpServerUri
    LanguageToolOrgAll              -> PathFound languageToolOrg
    LanguageToolOrgUsername         -> PathFound (languageToolOrg . username)
    LanguageToolOrgApiKey           -> PathFound (languageToolOrg . apiKey)
    LTeXLsAll                       -> PathFound ltexLs
    LTeXLsPath                      -> PathFound (ltexLs . ltexLsPath)
    LTeXLsLogLevel                  -> PathFound (ltexLs . logLevel)
    JavaAll                         -> PathFound java
    JavaPath                        -> PathFound (java . javaPath)
    JavaInitialHeapSize             -> PathFound (java . initialHeapSize)
    JavaMaximumHeapSize             -> PathFound (java . maximumHeapSize)
    SentenceCacheSize               -> PathFound sentenceCacheSize
    MaxRequestSize                  -> PathFound maxRequestSize
    ParagraphCacheTtlMinutes        -> PathFound paragraphCacheTtlMinutes
    ParagraphCacheEnabled           -> PathFound paragraphCacheEnabled
    CompletionEnabled               -> PathFound completionEnabled
    DiagnosticSeverity              -> PathFound diagnosticSeverity
    CheckFrequency                  -> PathFound checkFrequency
    ClearDiagnosticsWhenClosingFile -> PathFound clearDiagnosticsWhenClosingFile
    StatusBarItem                   -> PathFound statusBarItem
    TraceAll                        -> PathFound trace
    TraceServer                     -> PathFound (trace . server)
    where
        resolveCurrentOrNatLang :: LTeXCurrentOrNaturalLanguage
                                -> (LTeXNaturalLanguage -> ConfigPathAccess a)
                                -> ConfigPathAccess a
        resolveCurrentOrNatLang (LTeXCurrentOrNaturalLanguageNatural natLang) f = f natLang
        resolveCurrentOrNatLang LTeXCurrentOrNaturalLanguageCurrent f =
            case view language currentConf of
                (LTeXNaturalOrAutoDetectLanguageAuto _)          -> InvalidConfigForPath
                (LTeXNaturalOrAutoDetectLanguageNatural natLang) -> f natLang

        atExisting :: Ord k
                   => Lens' LTeXConfig (Map k v)
                   -> k
                   -> ConfigPathAccess v
        atExisting l key =
            case view (l . at key) currentConf of
                Nothing ->
                    let setter = \val conf -> set (l . at key) (Just val) conf in
                        ValueNotFound setter

                Just _  ->
                    PathFound $ lens
                        (\s' -> fromJust (view (l . at key) s'))
                        (\s' v' -> set (l . at key) (Just v') s')

data GetLTeXConfigValueError = GetLTeXConfigValueNotFound | GetLTeXConfigValueConfigInvalid

getLTeXConfigValue :: ConfigPath a b -> LTeXConfig -> Either GetLTeXConfigValueError a
getLTeXConfigValue configPath currentConf =
    case accessConfigPath configPath currentConf of
        PathFound pathLens   -> Right (view pathLens currentConf)
        ValueNotFound _      -> Left  GetLTeXConfigValueNotFound
        InvalidConfigForPath -> Left  GetLTeXConfigValueConfigInvalid

data SetLTeXConfigValueError = SetLTeXConfigValueConfigInvalid

setLTeXConfigValue :: ConfigPath a b -> a -> LTeXConfig -> Either SetLTeXConfigValueError LTeXConfig
setLTeXConfigValue configPath newValue currentConf =
    case accessConfigPath configPath currentConf of
        PathFound pathLens   -> Right (set pathLens newValue currentConf)
        ValueNotFound setter -> Right (setter newValue currentConf)
        InvalidConfigForPath -> Left  SetLTeXConfigValueConfigInvalid

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

removeDefaultValues :: LTeXConfig -> Value
removeDefaultValues ltexConfig =
    case removeDefaultValues' (toJSON ltexConfig) (toJSON defaultLTeXConfig) of
        Null -> Object KM.empty
        x    -> x
    where
        removeDefaultValues' :: Value -> Value -> Value
        removeDefaultValues' current defaultValue | current == defaultValue = Null
        removeDefaultValues' (Object current) (Object defaults) =
            let remaining = KM.filter (/= Null) $
                    flip KM.mapWithKey current $ \key value ->
                        maybe value (removeDefaultValues' value) (KM.lookup key defaults)
            in if KM.null remaining then Null else Object remaining
        removeDefaultValues' current _ = current

parseLTeXConfig :: ByteString -> Either String LTeXConfig
parseLTeXConfig = eitherDecode

showConfigInJson :: LTeXConfig -> ByteString
showConfigInJson = encode

showItemInJson :: ToJSON a => a -> ByteString
showItemInJson = encode

unsupportedAppend :: a -> b -> Either AppendError a
unsupportedAppend _ _ = Left AppendNotSupported

unsupportedRemove :: a -> b -> Either RemoveError a
unsupportedRemove _ _ = Left RemoveNotSupported

unsupportedInitialiseAppend :: b -> Either AppendInitialiseError a
unsupportedInitialiseAppend _ = Left AppendInitialiseNotSupported

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
        
makeMapFromPair :: (a, b) -> Map a b
makeMapFromPair (a, b) = Map.singleton a b

