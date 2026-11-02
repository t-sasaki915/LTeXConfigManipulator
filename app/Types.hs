{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types
    ( LTeXConfig
    , SomeConfigPath (..)
    , ConfigPath
    , AppendError (..)
    , RemoveError (..)
    , IsListActionDefined (..)
    , defaultLTeXConfig
    , configPathFromText
    , getValueFromConfig
    , setValueToConfig
    ) where

import Control.Lens (at, view, set, Lens')
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text

import LTeXConfigTypes

data ConfigPath a b where
    Enabled                         :: ConfigPath LTeXEnabled                                                        LTeXEnabled
    Language                        :: ConfigPath LTeXNaturalOrAutoDetectLanguage                                    ()
    Dictionary                      :: LTeXNaturalLanguage -> ConfigPath (Maybe [Text])                              Text
    DictionaryAll                   :: ConfigPath (Map LTeXNaturalLanguage [Text])                                   (LTeXNaturalLanguage, [Text])
    DisabledRules                   :: LTeXNaturalLanguage -> ConfigPath (Maybe [Text])                              Text
    DisabledRulesAll                :: ConfigPath (Map LTeXNaturalLanguage [Text])                                   (LTeXNaturalLanguage, [Text])
    EnabledRules                    :: LTeXNaturalLanguage -> ConfigPath (Maybe [Text])                              Text
    EnabledRulesAll                 :: ConfigPath (Map LTeXNaturalLanguage [Text])                                   (LTeXNaturalLanguage, [Text])
    HiddenFalsePositives            :: LTeXNaturalLanguage -> ConfigPath (Maybe [HiddenFalsePositivesValue])         HiddenFalsePositivesValue
    HiddenFalsePositivesAll         :: ConfigPath (Map LTeXNaturalLanguage [HiddenFalsePositivesValue])              (LTeXNaturalLanguage, [HiddenFalsePositivesValue])
    BibTeXAll                       :: ConfigPath BibTeXConfig                                                       ()
    BibTeXFields                    :: Text -> ConfigPath (Maybe Bool)                                               ()
    BibTeXFieldsAll                 :: ConfigPath (Map Text Bool)                                                    (Text, Bool)
    LaTeXAll                        :: ConfigPath LaTeXConfig                                                        ()
    LaTeXCommands                   :: Text -> ConfigPath (Maybe LTeXAction)                                         ()
    LaTeXCommandsAll                :: ConfigPath (Map Text LTeXAction)                                              (Text, LTeXAction)
    LaTeXEnvironments               :: Text -> ConfigPath (Maybe LaTeXEnvironmentAction)                             ()
    LaTeXEnvironmentsAll            :: ConfigPath (Map Text LaTeXEnvironmentAction)                                  (Text, LaTeXEnvironmentAction)
    MarkdownAll                     :: ConfigPath MarkdownConfig                                                     ()
    MarkdownNodes                   :: Text -> ConfigPath (Maybe LTeXAction)                                         ()
    MarkdownNodesAll                :: ConfigPath (Map Text LTeXAction)                                              (Text, LTeXAction)
    ConfigurationTarget             :: LTeXConfigurationTargetKey -> ConfigPath (Maybe LTeXConfigurationTargetValue) ()
    ConfigurationTargetAll          :: ConfigPath (Map LTeXConfigurationTargetKey LTeXConfigurationTargetValue)      (LTeXConfigurationTargetKey, LTeXConfigurationTargetValue)
    AdditionalRulesAll              :: ConfigPath AdditionalRules                                                    ()
    AdditionalRulesEnablePickyRules :: ConfigPath Bool                                                               ()
    AdditionalRulesMotherTongue     :: ConfigPath LTeXNaturalOrNoneLanguage                                          ()
    AdditionalRulesLanguageModel    :: ConfigPath Text                                                               ()
    LanguageToolHttpServerUri       :: ConfigPath Text                                                               ()
    LanguageToolOrgAll              :: ConfigPath LanguageToolOrg                                                    ()
    LanguageToolOrgUsername         :: ConfigPath Text                                                               ()
    LanguageToolOrgApiKey           :: ConfigPath Text                                                               ()
    LTeXLsAll                       :: ConfigPath LTeXLs                                                             ()
    LTeXLsPath                      :: ConfigPath Text                                                               ()
    LTeXLsLogLevel                  :: ConfigPath LTeXLsLogLevel                                                     ()
    JavaAll                         :: ConfigPath JavaConfig                                                         ()
    JavaPath                        :: ConfigPath Text                                                               ()
    JavaInitialHeapSize             :: ConfigPath Int                                                                ()
    JavaMaximumHeapSize             :: ConfigPath Int                                                                ()
    SentenceCacheSize               :: ConfigPath Int                                                                ()
    MaxRequestSize                  :: ConfigPath Int                                                                ()
    ParagraphCacheTtlMinutes        :: ConfigPath Int                                                                ()
    ParagraphCacheEnabled           :: ConfigPath Bool                                                               ()
    CompletionEnabled               :: ConfigPath Bool                                                               ()
    DiagnosticSeverity              :: ConfigPath LTeXDiagnosticSeverity                                             ()
    CheckFrequency                  :: ConfigPath LTeXCheckFrequency                                                 ()
    ClearDiagnosticsWhenClosingFile :: ConfigPath Bool                                                               ()
    StatusBarItem                   :: ConfigPath Bool                                                               ()
    TraceAll                        :: ConfigPath Trace                                                              ()
    TraceServer                     :: ConfigPath TraceServerMode                                                    ()

data SomeConfigPath where
    SomeConfigPath :: (IsListActionDefined a b, ToJSON a, FromJSON a, ToJSON b, FromJSON b)
                   => ConfigPath a b
                   -> SomeConfigPath

configPathLens :: ConfigPath a b -> Lens' LTeXConfig a
configPathLens = \case
    Enabled                         -> enabled
    Language                        -> language
    (Dictionary x)                  -> dictionary . at x
    DictionaryAll                   -> dictionary
    (DisabledRules x)               -> disabledRules . at x
    DisabledRulesAll                -> disabledRules
    (EnabledRules x)                -> enabledRules . at x
    EnabledRulesAll                 -> enabledRules
    (HiddenFalsePositives x)        -> hiddenFalsePositives . at x
    HiddenFalsePositivesAll         -> hiddenFalsePositives
    BibTeXAll                       -> bibtex
    (BibTeXFields x)                -> bibtex . fields . at x
    BibTeXFieldsAll                 -> bibtex . fields
    LaTeXAll                        -> latex
    (LaTeXCommands x)               -> latex . commands . at x
    LaTeXCommandsAll                -> latex . commands
    (LaTeXEnvironments x)           -> latex . environments . at x
    LaTeXEnvironmentsAll            -> latex . environments
    MarkdownAll                     -> markdown
    (MarkdownNodes x)               -> markdown . nodes . at x
    MarkdownNodesAll                -> markdown . nodes
    (ConfigurationTarget x)         -> configurationTarget . at x
    ConfigurationTargetAll          -> configurationTarget
    AdditionalRulesAll              -> additionalRules
    AdditionalRulesEnablePickyRules -> additionalRules . enablePickyRules
    AdditionalRulesMotherTongue     -> additionalRules . motherTongue
    AdditionalRulesLanguageModel    -> additionalRules . languageModel
    LanguageToolHttpServerUri       -> languageToolHttpServerUri
    LanguageToolOrgAll              -> languageToolOrg
    LanguageToolOrgUsername         -> languageToolOrg . username
    LanguageToolOrgApiKey           -> languageToolOrg . apiKey
    LTeXLsAll                       -> ltexLs
    LTeXLsPath                      -> ltexLs . ltexLsPath
    LTeXLsLogLevel                  -> ltexLs . logLevel
    JavaAll                         -> java
    JavaPath                        -> java . javaPath
    JavaInitialHeapSize             -> java . initialHeapSize
    JavaMaximumHeapSize             -> java . maximumHeapSize
    SentenceCacheSize               -> sentenceCacheSize
    MaxRequestSize                  -> maxRequestSize
    ParagraphCacheTtlMinutes        -> paragraphCacheTtlMinutes
    ParagraphCacheEnabled           -> paragraphCacheEnabled
    CompletionEnabled               -> completionEnabled
    DiagnosticSeverity              -> diagnosticSeverity
    CheckFrequency                  -> checkFrequency
    ClearDiagnosticsWhenClosingFile  -> clearDiagnosticsWhenClosingFile
    StatusBarItem                   -> statusBarItem
    TraceAll                        -> trace
    TraceServer                     -> trace . server

getValueFromConfig :: ConfigPath a b -> LTeXConfig -> a
getValueFromConfig = view . configPathLens

setValueToConfig :: ConfigPath a b -> a -> LTeXConfig -> LTeXConfig
setValueToConfig path val conf = set (configPathLens path) val conf

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

data AppendError = AppendNotSupported | ItemAlreadyInList deriving (Show, Eq)
data RemoveError = RemoveNotSupported | ItemNotInList     deriving (Show, Eq)

unsupportedAppend :: a -> b -> Either AppendError a
unsupportedAppend _ _ = Left AppendNotSupported

unsupportedRemove :: a -> b -> Either RemoveError a
unsupportedRemove _ _ = Left RemoveNotSupported

typicalAppendList :: Eq a => Maybe [a] -> a -> Either AppendError (Maybe [a])
typicalAppendList maybeList newItem =
    let list = fromMaybe [] maybeList in
        case newItem `elem` list of
            False -> Right (Just (list <> [newItem]))
            True  -> Left ItemAlreadyInList

typicalRemoveList :: Eq a => Maybe [a] -> a -> Either RemoveError (Maybe [a])
typicalRemoveList maybeList targetItem =
    let list = fromMaybe [] maybeList in
        case targetItem `elem` list of
            True  -> Right (Just (filter (/= targetItem) list))
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

instance IsListActionDefined (Maybe [Text]) Text where
    doAppend = typicalAppendList
    doRemove = typicalRemoveList

instance IsListActionDefined (Map LTeXNaturalLanguage [Text]) (LTeXNaturalLanguage, [Text]) where
    doAppend = typicalAppendMap
    doRemove = typicalRemoveMap

instance IsListActionDefined (Maybe [HiddenFalsePositivesValue]) HiddenFalsePositivesValue where
    doAppend = typicalAppendList
    doRemove = typicalRemoveList

instance IsListActionDefined (Map LTeXNaturalLanguage [HiddenFalsePositivesValue]) (LTeXNaturalLanguage, [HiddenFalsePositivesValue]) where
    doAppend = typicalAppendMap
    doRemove = typicalRemoveMap

instance IsListActionDefined BibTeXConfig () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined (Maybe Bool) () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined (Map Text Bool) (Text, Bool) where
    doAppend = typicalAppendMap
    doRemove = typicalRemoveMap

instance IsListActionDefined LaTeXConfig () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined (Maybe LTeXAction) () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined (Map Text LTeXAction) (Text, LTeXAction) where
    doAppend = typicalAppendMap
    doRemove = typicalRemoveMap

instance IsListActionDefined (Maybe LaTeXEnvironmentAction) () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined (Map Text LaTeXEnvironmentAction) (Text, LaTeXEnvironmentAction) where
    doAppend = typicalAppendMap
    doRemove = typicalRemoveMap

instance IsListActionDefined MarkdownConfig () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined (Maybe LTeXConfigurationTargetValue) () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined (Map LTeXConfigurationTargetKey LTeXConfigurationTargetValue) (LTeXConfigurationTargetKey, LTeXConfigurationTargetValue)where
    doAppend = typicalAppendMap
    doRemove = typicalRemoveMap

instance IsListActionDefined AdditionalRules () where
    doAppend = unsupportedAppend
    doRemove = unsupportedRemove

instance IsListActionDefined Bool () where
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

