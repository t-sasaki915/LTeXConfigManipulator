module Types
    ( LTeXLanguage (..)
    ) where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy (toStrict)
import Data.Map (Map)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Vector (fromList, toList)

data LTeXLanguage = Arabic | Asturian | Belarusian | Breton | Catalan | CatalanValencian | Chinese
                  | Danish | Dutch | DutchBelgium | English | EnglishAustralian | EnglishCanadian
                  | EnglishGB | EnglishNewZealand | EnglishSouthAfrican | EnglishUS | Esperanto
                  | French | Galician | German | GermanAustria | GermanGermany | GermanSwiss
                  | Greek | Irish | Italian | Japanese | Khmer | Persian | Polish | Portuguese
                  | PortugueseAngolaPreAO | PortugueseBrazil | PortugueseMozambiquePreAO
                  | PortuguesePortugal | Romanian | Russian | SimpleGerman | Slovak | Slovenian
                  | Spanish | SpanishVoseo | Swedish | Tagalog | Tamil | Ukrainian | AutoDetect
                  deriving (Show, Eq, Ord)

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
    toJSON AutoDetect                = String "auto"

instance ToJSONKey LTeXLanguage where
    toJSONKey = ToJSONKeyValue toJSON toEncoding

instance FromJSON LTeXLanguage where
    parseJSON (String "ar")                         = pure Arabic
    parseJSON (String "ast-ES")                     = pure Asturian
    parseJSON (String "be-BY")                      = pure Belarusian
    parseJSON (String "br-FR")                      = pure Breton
    parseJSON (String "ca-ES")                      = pure Catalan
    parseJSON (String "ca-ES-valencia")             = pure CatalanValencian
    parseJSON (String "zh-CN")                      = pure Chinese
    parseJSON (String "da-DK")                      = pure Danish
    parseJSON (String "nl")                         = pure Dutch
    parseJSON (String "nl-BE")                      = pure DutchBelgium
    parseJSON (String "en")                         = pure English
    parseJSON (String "en-AU")                      = pure EnglishAustralian
    parseJSON (String "en-CA")                      = pure EnglishCanadian
    parseJSON (String "en-GB")                      = pure EnglishGB
    parseJSON (String "en-NZ")                      = pure EnglishNewZealand
    parseJSON (String "en-ZA")                      = pure EnglishSouthAfrican
    parseJSON (String "en-US")                      = pure EnglishUS
    parseJSON (String "eo")                         = pure Esperanto
    parseJSON (String "fr")                         = pure French
    parseJSON (String "gl-ES")                      = pure Galician
    parseJSON (String "de")                         = pure German
    parseJSON (String "de-AT")                      = pure GermanAustria
    parseJSON (String "de-DE")                      = pure GermanGermany
    parseJSON (String "de-CH")                      = pure GermanSwiss
    parseJSON (String "el-GR")                      = pure Greek
    parseJSON (String "ga-IE")                      = pure Irish
    parseJSON (String "it")                         = pure Italian
    parseJSON (String "ja-JP")                      = pure Japanese
    parseJSON (String "km-KH")                      = pure Khmer
    parseJSON (String "fa")                         = pure Persian
    parseJSON (String "pl-PL")                      = pure Polish
    parseJSON (String "pt")                         = pure Portuguese
    parseJSON (String "pt-AO")                      = pure PortugueseAngolaPreAO
    parseJSON (String "pt-BR")                      = pure PortugueseBrazil
    parseJSON (String "pt-MZ")                      = pure PortugueseMozambiquePreAO
    parseJSON (String "pt-PT")                      = pure PortuguesePortugal
    parseJSON (String "ro-RO")                      = pure Romanian
    parseJSON (String "ru-RU")                      = pure Russian
    parseJSON (String "de-DE-x-simple-language")    = pure SimpleGerman
    parseJSON (String "de-DE-x-simple-language-DE") = pure SimpleGerman
    parseJSON (String "sk-SK")                      = pure Slovak
    parseJSON (String "sl-SI")                      = pure Slovenian
    parseJSON (String "es")                         = pure Spanish
    parseJSON (String "es-AR")                      = pure SpanishVoseo
    parseJSON (String "sv")                         = pure Swedish
    parseJSON (String "tl-PH")                      = pure Tagalog
    parseJSON (String "ta-IN")                      = pure Tamil
    parseJSON (String "uk-UA")                      = pure Ukrainian
    parseJSON (String "auto")                       = pure AutoDetect

    parseJSON invalid = prependFailure "parsing LTeXLanguage failed, " (unexpected invalid)

instance FromJSONKey LTeXLanguage where
    fromJSONKey = FromJSONKeyValue parseJSON

data LTeXEnabled = LTeXEnabledBool Bool
                 | LTeXEnabledArray [Text]
                 deriving (Show, Eq)

instance ToJSON LTeXEnabled where
    toJSON (LTeXEnabledBool bl)   = Bool bl
    toJSON (LTeXEnabledArray arr) = Array (fromList (map toJSON arr))

instance FromJSON LTeXEnabled where
    parseJSON (Bool bl)   = pure (LTeXEnabledBool bl)
    parseJSON (Array arr) = LTeXEnabledArray . toList <$> mapM parseJSON arr

    parseJSON invalid =
        prependFailure "parsing LTeXEnabled failed, "
            (typeMismatch "Bool or Array" invalid)

data HiddenFalsePositives = HiddenFalsePositives
    { rule     :: Text
    , sentence :: Text
    }
    deriving (Show, Eq)

instance ToJSON HiddenFalsePositives where
    toJSON x = object
        [ "rule"     .= rule x
        , "sentence" .= sentence x
        ]

instance FromJSON HiddenFalsePositives where
    parseJSON = withObject "HiddenFalsePositives" $ \obj -> HiddenFalsePositives
        <$> obj .: "rule"
        <*> obj .: "sentence"

hiddenFalsePositivesToText :: HiddenFalsePositives -> Text
hiddenFalsePositivesToText = decodeUtf8 . toStrict . encode

hiddenFalsePositivesFromText :: Text -> Parser HiddenFalsePositives
hiddenFalsePositivesFromText text =
    case eitherDecodeStrict' (encodeUtf8 text) of
        Right value -> pure value
        Left  err   -> fail err

data BibTeXConfig = BibTeXConfig
    { fields :: Map Text Bool
    }
    deriving (Show, Eq)

instance ToJSON BibTeXConfig where
    toJSON conf = object
        [ "fields" .= fields conf
        ]

instance FromJSON BibTeXConfig where
    parseJSON = withObject "BibTeXConfig" $ \obj -> BibTeXConfig
        <$> obj .: "fields"

data LTeXAction = LTeXActionDefault
                | LTeXActionIgnore
                | LTeXActionDummy
                | LTeXActionPluralDummy
                | LTeXActionVowelDummy
                deriving (Show, Eq)

instance ToJSON LTeXAction where
    toJSON LTeXActionDefault     = String "default"
    toJSON LTeXActionIgnore      = String "ignore"
    toJSON LTeXActionDummy       = String "dummy"
    toJSON LTeXActionPluralDummy = String "pluralDummy"
    toJSON LTeXActionVowelDummy  = String "vowelDummy"

instance FromJSON LTeXAction where
    parseJSON (String "default")     = pure LTeXActionDefault
    parseJSON (String "ignore")      = pure LTeXActionIgnore
    parseJSON (String "dummy")       = pure LTeXActionDummy
    parseJSON (String "pluralDummy") = pure LTeXActionPluralDummy
    parseJSON (String "vowelDummy")  = pure LTeXActionVowelDummy

    parseJSON invalid = prependFailure "parsing LTeXAction failed, " (unexpected invalid)

data LaTeXEnvironmentAction = LaTeXEnvironmentActionDefault
                            | LaTeXEnvironmentActionIgnore
                            deriving (Show, Eq)

instance ToJSON LaTeXEnvironmentAction where
    toJSON LaTeXEnvironmentActionDefault = String "default"
    toJSON LaTeXEnvironmentActionIgnore  = String "ignore"

instance FromJSON LaTeXEnvironmentAction where
    parseJSON (String "default") = pure LaTeXEnvironmentActionDefault
    parseJSON (String "ignore")  = pure LaTeXEnvironmentActionIgnore

    parseJSON invalid = prependFailure "parsing LaTeXEnvironmentAction failed, " (unexpected invalid)

data LaTeXConfig = LaTeXConfig
    { commands     :: Map Text LTeXAction
    , environments :: Map Text LaTeXEnvironmentAction
    }
    deriving (Show, Eq)

instance ToJSON LaTeXConfig where
    toJSON conf = object
        [ "commands"     .= commands conf
        , "environments" .= environments conf
        ]

instance FromJSON LaTeXConfig where
    parseJSON = withObject "LaTeXConfig" $ \obj -> LaTeXConfig
        <$> obj .: "commands"
        <*> obj .: "environments"

data MarkdownConfig = MarkdownConfig
    { nodes :: Map Text LTeXAction
    }
    deriving (Show, Eq)

instance ToJSON MarkdownConfig where
    toJSON conf = object
        [ "nodes" .= nodes conf
        ]

instance FromJSON MarkdownConfig where
    parseJSON = withObject "MarkdownConfig" $ \obj -> MarkdownConfig
        <$> obj .: "nodes"

data LTeXConfigurationTargetKey = LTeXConfigurationTargetKeyDictionary
                                | LTeXConfigurationTargetKeyDisabledRules
                                | LTeXConfigurationTargetKeyHiddenFalsePositives
                                deriving (Show, Eq, Ord)

instance ToJSON LTeXConfigurationTargetKey where
    toJSON LTeXConfigurationTargetKeyDictionary           = String "dictionary"
    toJSON LTeXConfigurationTargetKeyDisabledRules        = String "disabledRules"
    toJSON LTeXConfigurationTargetKeyHiddenFalsePositives = String "hiddenFalsePositives"

instance ToJSONKey LTeXConfigurationTargetKey where
    toJSONKey = ToJSONKeyValue toJSON toEncoding

instance FromJSON LTeXConfigurationTargetKey where
    parseJSON (String "dictionary")           = pure LTeXConfigurationTargetKeyDictionary
    parseJSON (String "disabledRules")        = pure LTeXConfigurationTargetKeyDisabledRules
    parseJSON (String "hiddenFalsePositives") = pure LTeXConfigurationTargetKeyHiddenFalsePositives

    parseJSON invalid = prependFailure "parsing LTeXConfigurationTargetKey failed, " (unexpected invalid)

instance FromJSONKey LTeXConfigurationTargetKey where
    fromJSONKey = FromJSONKeyValue parseJSON

data LTeXConfigurationTargetValue = LTeXConfigurationTargetValueUser
                                  | LTeXConfigurationTargetValueWorkspace
                                  | LTeXConfigurationTargetValueWorkspaceFolder
                                  | LTeXConfigurationTargetValueUserExternalFile
                                  | LTeXConfigurationTargetValueWorkspaceExternalFile
                                  | LTeXConfigurationTargetValueWorkspaceFolderExternalFile
                                  deriving (Show, Eq)

instance ToJSON LTeXConfigurationTargetValue where
    toJSON LTeXConfigurationTargetValueUser                        = String "user"
    toJSON LTeXConfigurationTargetValueWorkspace                   = String "workspace"
    toJSON LTeXConfigurationTargetValueWorkspaceFolder             = String "workspaceFolder"
    toJSON LTeXConfigurationTargetValueUserExternalFile            = String "userExternalFile"
    toJSON LTeXConfigurationTargetValueWorkspaceExternalFile       = String "workspaceExternalFile"
    toJSON LTeXConfigurationTargetValueWorkspaceFolderExternalFile = String "workspaceFolderExternalFile"

instance FromJSON LTeXConfigurationTargetValue where
    parseJSON (String "user")                        = pure LTeXConfigurationTargetValueUser
    parseJSON (String "workspace")                   = pure LTeXConfigurationTargetValueWorkspace
    parseJSON (String "workspaceFolder")             = pure LTeXConfigurationTargetValueWorkspaceFolder
    parseJSON (String "userExternalFile")            = pure LTeXConfigurationTargetValueUserExternalFile
    parseJSON (String "workspaceExternalFile")       = pure LTeXConfigurationTargetValueWorkspaceExternalFile
    parseJSON (String "workspaceFolderExternalFile") = pure LTeXConfigurationTargetValueWorkspaceFolderExternalFile

    parseJSON invalid = prependFailure "parsing LTeXConfigurationTargetValue failed, " (unexpected invalid)

data LTeXConfig = LTeXConfig
    { enabled              :: LTeXEnabled
    , language             :: LTeXLanguage
    , dictionary           :: Map LTeXLanguage [Text]
    , disabledRules        :: Map LTeXLanguage [Text]
    , enabledRules         :: Map LTeXLanguage [Text]
    , hiddenFalsePositives :: Map LTeXLanguage [HiddenFalsePositives]
    , bibtex               :: BibTeXConfig
    , latex                :: LaTeXConfig
    , markdown             :: MarkdownConfig
    , configurationTarget  :: Map LTeXConfigurationTargetKey LTeXConfigurationTargetValue
    }
    deriving (Show, Eq)

instance ToJSON LTeXConfig where
    toJSON conf = object
        [ "ltex" .= object
            [ "enabled"              .= enabled conf
            , "language"             .= language conf
            , "dictionary"           .= dictionary conf
            , "disabledRules"        .= disabledRules conf
            , "enabledRules"         .= enabledRules conf
            , "hiddenFalsePositives" .= fmap (map hiddenFalsePositivesToText) (hiddenFalsePositives conf)
            , "bibtex"               .= bibtex conf
            , "latex"                .= latex conf
            , "markdown"             .= markdown conf
            , "configurationTarget"  .= configurationTarget conf
            ]
        ]

instance FromJSON LTeXConfig where
    parseJSON = withObject "LTeXConfig" $ \obj ->
        obj .: "ltex" >>= \ltex ->
            withObject "ltex" parseLTeXConfig ltex
        where
            parseLTeXConfig = \ltexObj -> LTeXConfig
                <$> ltexObj  .: "enabled"
                <*> ltexObj  .: "language"
                <*> ltexObj  .: "dictionary"
                <*> ltexObj  .: "disabledRules"
                <*> ltexObj  .: "enabledRules"
                <*> (ltexObj .: "hiddenFalsePositives" >>= traverse (traverse hiddenFalsePositivesFromText))
                <*> ltexObj  .: "bibtex"
                <*> ltexObj  .: "latex"
                <*> ltexObj  .: "markdown"
                <*> ltexObj  .: "configurationTarget"

