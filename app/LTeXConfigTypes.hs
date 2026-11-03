{-# LANGUAGE TemplateHaskell #-}

module LTeXConfigTypes
    ( LTeXEnabled (..)
    , LTeXMarkupOrProgrammingLanguage
    , LTeXNaturalOrAutoDetectLanguage (..)
    , LTeXNaturalLanguage
    , HiddenFalsePositivesValue
    , BibTeXConfig
    , LaTeXConfig
    , LTeXAction
    , LaTeXEnvironmentAction
    , MarkdownConfig
    , LTeXConfigurationTargetKey
    , LTeXConfigurationTargetValue
    , AdditionalRules
    , LTeXNaturalOrNoneLanguage
    , LanguageToolOrg
    , LTeXLs
    , LTeXLsLogLevel
    , JavaConfig
    , LTeXDiagnosticSeverity
    , LTeXCheckFrequency
    , Trace
    , TraceServerMode
    , LTeXConfig
    , defaultLTeXConfig
    , enabled
    , language
    , dictionary
    , disabledRules
    , enabledRules
    , hiddenFalsePositives
    , bibtex
    , fields
    , latex
    , commands
    , environments
    , markdown
    , nodes
    , configurationTarget
    , additionalRules
    , enablePickyRules
    , motherTongue
    , languageModel
    , languageToolHttpServerUri
    , languageToolOrg
    , username
    , apiKey
    , ltexLs
    , ltexLsPath
    , logLevel
    , java
    , javaPath
    , initialHeapSize
    , maximumHeapSize
    , sentenceCacheSize
    , maxRequestSize
    , paragraphCacheTtlMinutes
    , paragraphCacheEnabled
    , completionEnabled
    , diagnosticSeverity
    , checkFrequency
    , clearDiagnosticsWhenClosingFile
    , statusBarItem
    , trace
    , server
    ) where

import Control.Applicative ((<|>))
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy (toStrict)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Vector (fromList, toList)

data LTeXNaturalLanguage = Arabic | Asturian | Belarusian | Breton | Catalan | CatalanValencian
                         | Chinese | Danish | Dutch | DutchBelgium | English | EnglishAustralian
                         | EnglishCanadian | EnglishGB | EnglishNewZealand | EnglishSouthAfrican
                         | EnglishUS | Esperanto | French | Galician | German | GermanAustria
                         | GermanGermany | GermanSwiss | Greek | Irish | Italian | Japanese
                         | Khmer | Persian | Polish | Portuguese | PortugueseAngolaPreAO
                         | PortugueseBrazil | PortugueseMozambiquePreAO | PortuguesePortugal
                         | Romanian | Russian | SimpleGerman | Slovak | Slovenian | Spanish 
                         | SpanishVoseo | Swedish | Tagalog | Tamil | Ukrainian
                         deriving (Show, Eq, Ord)

instance ToJSON LTeXNaturalLanguage where
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

instance ToJSONKey LTeXNaturalLanguage where
    toJSONKey = toJSONKeyText $ \x ->
        case toJSON x of
            String t -> t
            _        -> undefined

instance FromJSON LTeXNaturalLanguage where
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

    parseJSON invalid = prependFailure "parsing LTeXNaturalLanguage failed, " (unexpected invalid)

instance FromJSONKey LTeXNaturalLanguage where
    fromJSONKey = FromJSONKeyTextParser $ parseJSON . String

data LTeXMarkupLanguage = BibTeX | ConTeXt | LaTeX | Markdown | MDX | Typst | AsciiDoc | Neorg
                        | Org | Quarto | ReStructuredText | RSweave | XHTML
                        deriving (Show, Eq, Ord)

instance ToJSON LTeXMarkupLanguage where
    toJSON BibTeX           = String "bibtex"
    toJSON ConTeXt          = String "context"
    toJSON LaTeX            = String "latex"
    toJSON Markdown         = String "markdown"
    toJSON MDX              = String "mdx"
    toJSON Typst            = String "typst"
    toJSON AsciiDoc         = String "asciidoc"
    toJSON Neorg            = String "neorg"
    toJSON Org              = String "org"
    toJSON Quarto           = String "quarto"
    toJSON ReStructuredText = String "restructuredtext"
    toJSON RSweave          = String "rsweave"
    toJSON XHTML            = String "html"

instance ToJSONKey LTeXMarkupLanguage where
    toJSONKey = toJSONKeyText $ \x ->
        case toJSON x of
            String t -> t
            _        -> undefined

instance FromJSON LTeXMarkupLanguage where
    parseJSON (String "bibtex")           = pure BibTeX
    parseJSON (String "context")          = pure ConTeXt
    parseJSON (String "latex")            = pure LaTeX
    parseJSON (String "markdown")         = pure Markdown
    parseJSON (String "mdx")              = pure MDX
    parseJSON (String "typst")            = pure Typst
    parseJSON (String "asciidoc")         = pure AsciiDoc
    parseJSON (String "neorg")            = pure Neorg
    parseJSON (String "org")              = pure Org
    parseJSON (String "quatro")           = pure Quarto
    parseJSON (String "restructuredtext") = pure ReStructuredText
    parseJSON (String "rsweave")          = pure RSweave
    parseJSON (String "html")             = pure XHTML

    parseJSON invalid = prependFailure "parsing LTeXMarkupLanguage failed, " (unexpected invalid)

instance FromJSONKey LTeXMarkupLanguage where
    fromJSONKey = FromJSONKeyTextParser $ parseJSON . String

data LTeXProgrammingLanguage = ShellScript | C | CSharp | CPP | Clojure | CoffeeScript | Dart
                             | Elixir | Elm | EmacsLisp | Erlang | FSharp | FORTRAN | Go | Groovy
                             | Haskell | Java | JavaScript | JavaScriptReact | Julia | Kotlin
                             | Lisp | Lua | MATLAB | Perl | Perl6 | PHP | PowerShell | Puppet
                             | Python | R | Ruby | Rust | Scala | SQL | Swift | TypeScript
                             | TypeScriptReact | Verilog | VisualBasic
                             deriving (Show, Eq, Ord)

instance ToJSON LTeXProgrammingLanguage where
    toJSON ShellScript     = String "shellscript"
    toJSON C               = String "c"
    toJSON CSharp          = String "csharp"
    toJSON CPP             = String "cpp"
    toJSON Clojure         = String "clojure"
    toJSON CoffeeScript    = String "coffeescript"
    toJSON Dart            = String "dart"
    toJSON Elixir          = String "elixir"
    toJSON Elm             = String "elm"
    toJSON EmacsLisp       = String "elisp"
    toJSON Erlang          = String "erlang"
    toJSON FSharp          = String "fsharp"
    toJSON FORTRAN         = String "fortran-modern"
    toJSON Go              = String "go"
    toJSON Groovy          = String "groovy"
    toJSON Haskell         = String "haskell"
    toJSON Java            = String "java"
    toJSON JavaScript      = String "javascript"
    toJSON JavaScriptReact = String "javascriptreact"
    toJSON Julia           = String "julia"
    toJSON Kotlin          = String "kotlin"
    toJSON Lisp            = String "lisp"
    toJSON Lua             = String "lua"
    toJSON MATLAB          = String "matlab"
    toJSON Perl            = String "perl"
    toJSON Perl6           = String "perl6"
    toJSON PHP             = String "php"
    toJSON PowerShell      = String "powershell"
    toJSON Puppet          = String "puppet"
    toJSON Python          = String "python"
    toJSON R               = String "r"
    toJSON Ruby            = String "ruby"
    toJSON Rust            = String "rust"
    toJSON Scala           = String "scala"
    toJSON SQL             = String "sql"
    toJSON Swift           = String "swift"
    toJSON TypeScript      = String "typescript"
    toJSON TypeScriptReact = String "typescriptreact"
    toJSON Verilog         = String "verilog"
    toJSON VisualBasic     = String "vb"

instance ToJSONKey LTeXProgrammingLanguage where
    toJSONKey = toJSONKeyText $ \x ->
        case toJSON x of
            String t -> t
            _        -> undefined

instance FromJSON LTeXProgrammingLanguage where
    parseJSON (String "shellscript")     = pure ShellScript
    parseJSON (String "c")               = pure C
    parseJSON (String "csharp")          = pure CSharp
    parseJSON (String "cpp")             = pure CPP
    parseJSON (String "clojure")         = pure Clojure
    parseJSON (String "coffeescript")    = pure CoffeeScript
    parseJSON (String "dart")            = pure Dart
    parseJSON (String "elixir")          = pure Elixir
    parseJSON (String "elm")             = pure Elm
    parseJSON (String "elisp")           = pure EmacsLisp
    parseJSON (String "erlang")          = pure Erlang
    parseJSON (String "fsharp")          = pure FSharp
    parseJSON (String "fortran-modern")  = pure FORTRAN
    parseJSON (String "go")              = pure Go
    parseJSON (String "groovy")          = pure Groovy
    parseJSON (String "haskell")         = pure Haskell
    parseJSON (String "java")            = pure Java
    parseJSON (String "javascript")      = pure JavaScript
    parseJSON (String "javascriptreact") = pure JavaScriptReact
    parseJSON (String "julia")           = pure Julia
    parseJSON (String "kotlin")          = pure Kotlin
    parseJSON (String "lisp")            = pure Lisp
    parseJSON (String "lua")             = pure Lua
    parseJSON (String "matlab")          = pure MATLAB
    parseJSON (String "perl")            = pure Perl
    parseJSON (String "perl6")           = pure Perl6
    parseJSON (String "php")             = pure PHP
    parseJSON (String "powershell")      = pure PowerShell
    parseJSON (String "puppet")          = pure Puppet
    parseJSON (String "python")          = pure Python
    parseJSON (String "r")               = pure R
    parseJSON (String "ruby")            = pure Ruby
    parseJSON (String "rust")            = pure Rust
    parseJSON (String "scala")           = pure Scala
    parseJSON (String "sql")             = pure SQL
    parseJSON (String "swift")           = pure Swift
    parseJSON (String "typescript")      = pure TypeScript
    parseJSON (String "typescriptreact") = pure TypeScriptReact
    parseJSON (String "verilog")         = pure Verilog
    parseJSON (String "vb")              = pure VisualBasic

    parseJSON invalid = prependFailure "parsing LTeXProgrammingLanguage failed, " (unexpected invalid)

instance FromJSONKey LTeXProgrammingLanguage where
    fromJSONKey = FromJSONKeyTextParser $ parseJSON . String

data LTeXAutoDetectLanguage = LTeXAutoDetectLanguage
                            deriving (Show, Eq, Ord)

instance ToJSON LTeXAutoDetectLanguage where
    toJSON LTeXAutoDetectLanguage = String "auto"

instance ToJSONKey LTeXAutoDetectLanguage where
    toJSONKey = toJSONKeyText $ \x ->
        case toJSON x of
            String t -> t
            _        -> undefined

instance FromJSON LTeXAutoDetectLanguage where
    parseJSON (String "auto") = pure LTeXAutoDetectLanguage

    parseJSON invalid = prependFailure "parsing LTeXAutoDetectLanguage failed, " (unexpected invalid)

instance FromJSONKey LTeXAutoDetectLanguage where
    fromJSONKey = FromJSONKeyTextParser $ parseJSON . String

data LTeXNoneLanguage = LTeXNoneLanguage
                      deriving (Show, Eq, Ord)

instance ToJSON LTeXNoneLanguage where
    toJSON LTeXNoneLanguage = String ""

instance ToJSONKey LTeXNoneLanguage where
    toJSONKey = toJSONKeyText $ \x ->
        case toJSON x of
            String t -> t
            _        -> undefined

instance FromJSON LTeXNoneLanguage where
    parseJSON (String "") = pure LTeXNoneLanguage

    parseJSON invalid = prependFailure "parsing LTeXNoneLanguage failed, " (unexpected invalid)

instance FromJSONKey LTeXNoneLanguage where
    fromJSONKey = FromJSONKeyTextParser $ parseJSON . String

data LTeXMarkupOrProgrammingLanguage = LTeXMarkupOrProgrammingLanguageMarkup      LTeXMarkupLanguage
                                     | LTeXMarkupOrProgrammingLanguageProgramming LTeXProgrammingLanguage
                                     deriving (Show, Eq, Ord)

instance ToJSON LTeXMarkupOrProgrammingLanguage where
    toJSON (LTeXMarkupOrProgrammingLanguageMarkup x)      = toJSON x
    toJSON (LTeXMarkupOrProgrammingLanguageProgramming x) = toJSON x

instance ToJSONKey LTeXMarkupOrProgrammingLanguage where
    toJSONKey = toJSONKeyText $ \x ->
        case toJSON x of
            String t -> t
            _        -> undefined

instance FromJSON LTeXMarkupOrProgrammingLanguage where
    parseJSON value =
            (LTeXMarkupOrProgrammingLanguageMarkup <$> parseJSON value)
        <|> (LTeXMarkupOrProgrammingLanguageProgramming <$> parseJSON value)

instance FromJSONKey LTeXMarkupOrProgrammingLanguage where
    fromJSONKey = FromJSONKeyTextParser $ parseJSON . String

data LTeXNaturalOrAutoDetectLanguage = LTeXNaturalOrAutoDetectLanguageNatural LTeXNaturalLanguage
                                     | LTeXNaturalOrAutoDetectLanguageAuto    LTeXAutoDetectLanguage
                                     deriving (Show, Eq, Ord)

instance ToJSON LTeXNaturalOrAutoDetectLanguage where
    toJSON (LTeXNaturalOrAutoDetectLanguageNatural x) = toJSON x
    toJSON (LTeXNaturalOrAutoDetectLanguageAuto x)    = toJSON x

instance ToJSONKey LTeXNaturalOrAutoDetectLanguage where
    toJSONKey = toJSONKeyText $ \x ->
        case toJSON x of
            String t -> t
            _        -> undefined

instance FromJSON LTeXNaturalOrAutoDetectLanguage where
    parseJSON value =
            (LTeXNaturalOrAutoDetectLanguageAuto <$> parseJSON value)
        <|> (LTeXNaturalOrAutoDetectLanguageNatural <$> parseJSON value)

instance FromJSONKey LTeXNaturalOrAutoDetectLanguage where
    fromJSONKey = FromJSONKeyTextParser $ parseJSON . String

data LTeXNaturalOrNoneLanguage = LTeXNaturalOrNoneLanguageNatural LTeXNaturalLanguage
                               | LTeXNaturalOrNoneLanguageNone    LTeXNoneLanguage
                               deriving (Show, Eq, Ord)

instance ToJSON LTeXNaturalOrNoneLanguage where
    toJSON (LTeXNaturalOrNoneLanguageNatural x) = toJSON x
    toJSON (LTeXNaturalOrNoneLanguageNone x)    = toJSON x

instance ToJSONKey LTeXNaturalOrNoneLanguage where
    toJSONKey = toJSONKeyText $ \x ->
        case toJSON x of
            String t -> t
            _        -> undefined

instance FromJSON LTeXNaturalOrNoneLanguage where
    parseJSON value =
            (LTeXNaturalOrNoneLanguageNone <$> parseJSON value)
        <|> (LTeXNaturalOrNoneLanguageNatural <$> parseJSON value)

instance FromJSONKey LTeXNaturalOrNoneLanguage where
    fromJSONKey = FromJSONKeyTextParser $ parseJSON . String

data LTeXEnabled = LTeXEnabledBool Bool
                 | LTeXEnabledArray [LTeXMarkupOrProgrammingLanguage]
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

data HiddenFalsePositivesValue = HiddenFalsePositivesValue
    { rule     :: Text
    , sentence :: Text
    }
    deriving (Show, Eq)

instance ToJSON HiddenFalsePositivesValue where
    toJSON x = object
        [ "rule"     .= rule x
        , "sentence" .= sentence x
        ]

instance FromJSON HiddenFalsePositivesValue where
    parseJSON = withObject "HiddenFalsePositivesValue" $ \obj -> HiddenFalsePositivesValue
        <$> obj .: "rule"
        <*> obj .: "sentence"

hiddenFalsePositivesToText :: HiddenFalsePositivesValue -> Text
hiddenFalsePositivesToText = decodeUtf8 . toStrict . encode

hiddenFalsePositivesFromText :: Text -> Parser HiddenFalsePositivesValue
hiddenFalsePositivesFromText text =
    case eitherDecodeStrict' (encodeUtf8 text) of
        Right value -> pure value
        Left  err   -> fail err

data BibTeXConfig = BibTeXConfig
    { _fields :: Map Text Bool
    }
    deriving (Show, Eq)

makeLenses ''BibTeXConfig

instance ToJSON BibTeXConfig where
    toJSON conf = object
        [ "fields" .= _fields conf
        ]

instance FromJSON BibTeXConfig where
    parseJSON = withObject "BibTeXConfig" $ \obj -> BibTeXConfig
        <$> obj .:? "fields" .!= _fields defaultBibTeXConfig

defaultBibTeXConfig :: BibTeXConfig
defaultBibTeXConfig = BibTeXConfig
    { _fields = mempty
    }

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
    { _commands     :: Map Text LTeXAction
    , _environments :: Map Text LaTeXEnvironmentAction
    }
    deriving (Show, Eq)

makeLenses ''LaTeXConfig

instance ToJSON LaTeXConfig where
    toJSON conf = object
        [ "commands"     .= _commands conf
        , "environments" .= _environments conf
        ]

instance FromJSON LaTeXConfig where
    parseJSON = withObject "LaTeXConfig" $ \obj -> LaTeXConfig
        <$> obj .:? "commands"     .!= _commands     defaultLaTeXConfig
        <*> obj .:? "environments" .!= _environments defaultLaTeXConfig

defaultLaTeXConfig :: LaTeXConfig
defaultLaTeXConfig = LaTeXConfig
    { _commands     = mempty
    , _environments = mempty
    }

data MarkdownConfig = MarkdownConfig
    { _nodes :: Map Text LTeXAction
    }
    deriving (Show, Eq)

makeLenses ''MarkdownConfig

instance ToJSON MarkdownConfig where
    toJSON conf = object
        [ "nodes" .= _nodes conf
        ]

instance FromJSON MarkdownConfig where
    parseJSON = withObject "MarkdownConfig" $ \obj -> MarkdownConfig
        <$> obj .:? "nodes" .!= _nodes defaultMarkdownConfig

defaultMarkdownConfig :: MarkdownConfig
defaultMarkdownConfig = MarkdownConfig
    { _nodes = mempty
    }

data LTeXConfigurationTargetKey = LTeXConfigurationTargetKeyDictionary
                                | LTeXConfigurationTargetKeyDisabledRules
                                | LTeXConfigurationTargetKeyHiddenFalsePositives
                                deriving (Show, Eq, Ord)

instance ToJSON LTeXConfigurationTargetKey where
    toJSON LTeXConfigurationTargetKeyDictionary           = String "dictionary"
    toJSON LTeXConfigurationTargetKeyDisabledRules        = String "disabledRules"
    toJSON LTeXConfigurationTargetKeyHiddenFalsePositives = String "hiddenFalsePositives"

instance ToJSONKey LTeXConfigurationTargetKey where
    toJSONKey = toJSONKeyText $ \x ->
        case toJSON x of
            String t -> t
            _        -> undefined

instance FromJSON LTeXConfigurationTargetKey where
    parseJSON (String "dictionary")           = pure LTeXConfigurationTargetKeyDictionary
    parseJSON (String "disabledRules")        = pure LTeXConfigurationTargetKeyDisabledRules
    parseJSON (String "hiddenFalsePositives") = pure LTeXConfigurationTargetKeyHiddenFalsePositives

    parseJSON invalid = prependFailure "parsing LTeXConfigurationTargetKey failed, " (unexpected invalid)

instance FromJSONKey LTeXConfigurationTargetKey where
    fromJSONKey = FromJSONKeyTextParser $ parseJSON . String

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

data AdditionalRules = AdditionalRules
    { _enablePickyRules :: Bool
    , _motherTongue     :: LTeXNaturalOrNoneLanguage
    , _languageModel    :: Text
    }
    deriving (Show, Eq)

makeLenses ''AdditionalRules

instance ToJSON AdditionalRules where
    toJSON conf = object
        [ "enablePickyRules" .= _enablePickyRules conf
        , "motherTongue"     .= _motherTongue conf
        , "languageModel"    .= _languageModel conf
        ]

instance FromJSON AdditionalRules where
    parseJSON = withObject "AdditionalRules" $ \obj -> AdditionalRules
        <$> obj .:? "enablePickyRules" .!= _enablePickyRules defaultAdditionalRules
        <*> obj .:? "motherTongue"     .!= _motherTongue     defaultAdditionalRules
        <*> obj .:? "languageModel"    .!= _languageModel    defaultAdditionalRules

defaultAdditionalRules :: AdditionalRules
defaultAdditionalRules = AdditionalRules
    { _enablePickyRules = False
    , _motherTongue     = LTeXNaturalOrNoneLanguageNone LTeXNoneLanguage
    , _languageModel    = ""
    }

data LanguageToolOrg = LanguageToolOrg
    { _username :: Text
    , _apiKey   :: Text
    }
    deriving (Show, Eq)

makeLenses ''LanguageToolOrg

instance ToJSON LanguageToolOrg where
    toJSON conf = object
        [ "username" .= _username conf
        , "apiKey"   .= _apiKey conf
        ]

instance FromJSON LanguageToolOrg where
    parseJSON = withObject "LanguageToolOrg" $ \obj -> LanguageToolOrg
        <$> obj .:? "username" .!= _username defaultLanguageToolOrg
        <*> obj .:? "apiKey"   .!= _apiKey   defaultLanguageToolOrg

defaultLanguageToolOrg :: LanguageToolOrg
defaultLanguageToolOrg = LanguageToolOrg
    { _username = ""
    , _apiKey   = ""
    }

data LTeXLsLogLevel = LTeXLsLogLevelSevere
                    | LTeXLsLogLevelWarning
                    | LTeXLsLogLevelInfo
                    | LTeXLsLogLevelConfig
                    | LTeXLsLogLevelFine
                    | LTeXLsLogLevelFiner
                    | LTeXLsLogLevelFinest
                    deriving (Show, Eq)

instance ToJSON LTeXLsLogLevel where
    toJSON LTeXLsLogLevelSevere  = String "severe"
    toJSON LTeXLsLogLevelWarning = String "warning"
    toJSON LTeXLsLogLevelInfo    = String "info"
    toJSON LTeXLsLogLevelConfig  = String "config"
    toJSON LTeXLsLogLevelFine    = String "fine"
    toJSON LTeXLsLogLevelFiner   = String "finer"
    toJSON LTeXLsLogLevelFinest  = String "finest"

instance FromJSON LTeXLsLogLevel where
    parseJSON (String "severe")  = pure LTeXLsLogLevelSevere
    parseJSON (String "warning") = pure LTeXLsLogLevelWarning
    parseJSON (String "info")    = pure LTeXLsLogLevelInfo
    parseJSON (String "config")  = pure LTeXLsLogLevelConfig
    parseJSON (String "fine")    = pure LTeXLsLogLevelFine
    parseJSON (String "finer")   = pure LTeXLsLogLevelFiner
    parseJSON (String "finest")  = pure LTeXLsLogLevelFinest

    parseJSON invalid = prependFailure "parsing LTeXLsLogLevel failed, " (unexpected invalid)

data LTeXLs = LTeXLs
    { _ltexLsPath  :: Text
    , _logLevel    :: LTeXLsLogLevel
    }
    deriving (Show, Eq)

makeLenses ''LTeXLs

instance ToJSON LTeXLs where
    toJSON conf = object
        [ "path"     .= _ltexLsPath conf
        , "logLevel" .= _logLevel conf
        ]

instance FromJSON LTeXLs where
    parseJSON = withObject "LTeXLs" $ \obj -> LTeXLs
        <$> obj .:? "path"     .!= _ltexLsPath defaultLTeXLs
        <*> obj .:? "logLevel" .!= _logLevel   defaultLTeXLs

defaultLTeXLs :: LTeXLs
defaultLTeXLs = LTeXLs
    { _ltexLsPath = ""
    , _logLevel   = LTeXLsLogLevelFine
    }

data JavaConfig = JavaConfig
    { _javaPath        :: Text
    , _initialHeapSize :: Int
    , _maximumHeapSize :: Int
    }
    deriving (Show, Eq)

makeLenses ''JavaConfig

instance ToJSON JavaConfig where
    toJSON conf = object
        [ "path"            .= _javaPath conf
        , "initialHeapSize" .= _initialHeapSize conf
        , "maximumHeapSize" .= _maximumHeapSize conf
        ]

instance FromJSON JavaConfig where
    parseJSON = withObject "Java" $ \obj -> JavaConfig
        <$> obj .:? "path"            .!= _javaPath        defaultJavaConfig
        <*> obj .:? "initialHeapSize" .!= _initialHeapSize defaultJavaConfig
        <*> obj .:? "maximumHeapSize" .!= _maximumHeapSize defaultJavaConfig

defaultJavaConfig :: JavaConfig
defaultJavaConfig = JavaConfig
    { _javaPath        = ""
    , _initialHeapSize = 64
    , _maximumHeapSize = 2048
    }

data LTeXDiagnosticSeverityType = LTeXDiagnosticSeverityTypeError
                                | LTeXDiagnosticSeverityTypeWarning
                                | LTeXDiagnosticSeverityTypeInformation
                                | LTeXDiagnosticSeverityTypeHint
                                deriving (Show, Eq)

instance ToJSON LTeXDiagnosticSeverityType where
    toJSON LTeXDiagnosticSeverityTypeError       = String "error"
    toJSON LTeXDiagnosticSeverityTypeWarning     = String "warning"
    toJSON LTeXDiagnosticSeverityTypeInformation = String "information"
    toJSON LTeXDiagnosticSeverityTypeHint        = String "hint"

instance FromJSON LTeXDiagnosticSeverityType where
    parseJSON (String "error")       = pure LTeXDiagnosticSeverityTypeError
    parseJSON (String "warning")     = pure LTeXDiagnosticSeverityTypeWarning
    parseJSON (String "information") = pure LTeXDiagnosticSeverityTypeInformation
    parseJSON (String "hint")        = pure LTeXDiagnosticSeverityTypeHint
    
    parseJSON invalid = prependFailure "parsing LTeXDiagnosticSeverityType failed, " (unexpected invalid)

data LTeXDiagnosticSeverity = LTeXDiagnosticSeverityString LTeXDiagnosticSeverityType
                            | LTeXDiagnosticSeverityObject (Map Text LTeXDiagnosticSeverityType)
                            deriving (Show, Eq)

instance ToJSON LTeXDiagnosticSeverity where
    toJSON (LTeXDiagnosticSeverityString x) = toJSON x
    toJSON (LTeXDiagnosticSeverityObject x) = toJSON x

instance FromJSON LTeXDiagnosticSeverity where
    parseJSON (String x) = LTeXDiagnosticSeverityString <$> parseJSON (String x)
    parseJSON (Object x) = LTeXDiagnosticSeverityObject <$> parseJSON (Object x)

    parseJSON invalid =
        prependFailure "parsing LTeXDiagnosticSeverity failed, "
            (typeMismatch "String or Object" invalid)

data LTeXCheckFrequency = LTeXCheckFrequencyEdit
                        | LTeXCheckFrequencySave
                        | LTeXCheckFrequencyManual
                        deriving (Show, Eq)

instance ToJSON LTeXCheckFrequency where
    toJSON LTeXCheckFrequencyEdit   = String "edit"
    toJSON LTeXCheckFrequencySave   = String "save"
    toJSON LTeXCheckFrequencyManual = String "manual"

instance FromJSON LTeXCheckFrequency where
    parseJSON (String "edit")   = pure LTeXCheckFrequencyEdit
    parseJSON (String "save")   = pure LTeXCheckFrequencySave
    parseJSON (String "manual") = pure LTeXCheckFrequencyManual
    
    parseJSON invalid = prependFailure "parsing LTeXCheckFrequency failed, " (unexpected invalid)

data TraceServerMode = TraceServerModeOff
                     | TraceServerModeMessages
                     | TraceServerModeVerbose
                     deriving (Show, Eq)

instance ToJSON TraceServerMode where
    toJSON TraceServerModeOff      = String "off"
    toJSON TraceServerModeMessages = String "messages"
    toJSON TraceServerModeVerbose  = String "verbose"

instance FromJSON TraceServerMode where
    parseJSON (String "off")      = pure TraceServerModeOff
    parseJSON (String "messages") = pure TraceServerModeMessages
    parseJSON (String "verbose")  = pure TraceServerModeVerbose
    
    parseJSON invalid = prependFailure "parsing TraceServerMode failed, " (unexpected invalid)

data Trace = Trace
    { _server :: TraceServerMode 
    }
    deriving (Show, Eq)

makeLenses ''Trace

instance ToJSON Trace where
    toJSON conf = object
        [ "server" .= _server conf
        ]

instance FromJSON Trace where
    parseJSON = withObject "Trace" $ \obj -> Trace
        <$> obj .:? "server" .!= _server defaultTrace

defaultTrace :: Trace
defaultTrace = Trace
    { _server = TraceServerModeOff
    }

data LTeXConfig = LTeXConfig
    { _enabled                         :: LTeXEnabled
    , _language                        :: LTeXNaturalOrAutoDetectLanguage
    , _dictionary                      :: Map LTeXNaturalLanguage [Text]
    , _disabledRules                   :: Map LTeXNaturalLanguage [Text]
    , _enabledRules                    :: Map LTeXNaturalLanguage [Text]
    , _hiddenFalsePositives            :: Map LTeXNaturalLanguage [HiddenFalsePositivesValue]
    , _bibtex                          :: BibTeXConfig
    , _latex                           :: LaTeXConfig
    , _markdown                        :: MarkdownConfig
    , _configurationTarget             :: Map LTeXConfigurationTargetKey LTeXConfigurationTargetValue
    , _additionalRules                 :: AdditionalRules
    , _languageToolHttpServerUri       :: Text
    , _languageToolOrg                 :: LanguageToolOrg
    , _ltexLs                          :: LTeXLs
    , _java                            :: JavaConfig
    , _sentenceCacheSize               :: Int
    , _maxRequestSize                  :: Int
    , _paragraphCacheTtlMinutes        :: Int
    , _paragraphCacheEnabled           :: Bool
    , _completionEnabled               :: Bool
    , _diagnosticSeverity              :: LTeXDiagnosticSeverity
    , _checkFrequency                  :: LTeXCheckFrequency
    , _clearDiagnosticsWhenClosingFile :: Bool
    , _statusBarItem                   :: Bool
    , _trace                           :: Trace
    }
    deriving (Show, Eq)

makeLenses ''LTeXConfig

instance ToJSON LTeXConfig where
    toJSON conf = object
        [ "ltex" .= object
            [ "enabled"                         .= _enabled conf
            , "language"                        .= _language conf
            , "dictionary"                      .= _dictionary conf
            , "disabledRules"                   .= _disabledRules conf
            , "enabledRules"                    .= _enabledRules conf
            , "hiddenFalsePositives"            .= fmap (map hiddenFalsePositivesToText) (_hiddenFalsePositives conf)
            , "bibtex"                          .= _bibtex conf
            , "latex"                           .= _latex conf
            , "markdown"                        .= _markdown conf
            , "configurationTarget"             .= _configurationTarget conf
            , "additionalRules"                 .= _additionalRules conf
            , "languageToolHttpServerUri"       .= _languageToolHttpServerUri conf
            , "languageToolOrg"                 .= _languageToolOrg conf
            , "ltex-ls"                         .= _ltexLs conf
            , "java"                            .= _java conf
            , "sentenceCacheSize"               .= _sentenceCacheSize conf
            , "maxRequestSize"                  .= _maxRequestSize conf
            , "paragraphCacheTtlMinutes"        .= _paragraphCacheTtlMinutes conf
            , "paragraphCacheEnabled"           .= _paragraphCacheEnabled conf
            , "completionEnabled"               .= _completionEnabled conf
            , "diagnosticSeverity"              .= _diagnosticSeverity conf
            , "checkFrequency"                  .= _checkFrequency conf
            , "clearDiagnosticsWhenClosingFile" .= _clearDiagnosticsWhenClosingFile conf
            , "statusBarItem"                   .= _statusBarItem conf
            , "trace"                           .= _trace conf
            ]
        ]

instance FromJSON LTeXConfig where
    parseJSON = withObject "LTeXConfig" $ \obj ->
        obj .: "ltex" >>= \ltex ->
            withObject "ltex" parseLTeXConfig ltex
        where
            parseLTeXConfig = \ltexObj -> LTeXConfig
                <$> ltexObj   .:? "enabled"                         .!= _enabled                         defaultLTeXConfig
                <*> ltexObj   .:? "language"                        .!= _language                        defaultLTeXConfig 
                <*> ltexObj   .:? "dictionary"                      .!= _dictionary                      defaultLTeXConfig
                <*> ltexObj   .:? "disabledRules"                   .!= _disabledRules                   defaultLTeXConfig
                <*> ltexObj   .:? "enabledRules"                    .!= _enabledRules                    defaultLTeXConfig
                <*> ((ltexObj .:? "hiddenFalsePositives"            .!= mempty) >>= traverse (traverse hiddenFalsePositivesFromText))
                <*> ltexObj   .:? "bibtex"                          .!= _bibtex                          defaultLTeXConfig
                <*> ltexObj   .:? "latex"                           .!= _latex                           defaultLTeXConfig
                <*> ltexObj   .:? "markdown"                        .!= _markdown                        defaultLTeXConfig
                <*> ltexObj   .:? "configurationTarget"             .!= _configurationTarget             defaultLTeXConfig
                <*> ltexObj   .:? "additionalRules"                 .!= _additionalRules                 defaultLTeXConfig
                <*> ltexObj   .:? "languageToolHttpServerUri"       .!= _languageToolHttpServerUri       defaultLTeXConfig
                <*> ltexObj   .:? "languageToolOrg"                 .!= _languageToolOrg                 defaultLTeXConfig
                <*> ltexObj   .:? "ltex-ls"                         .!= _ltexLs                          defaultLTeXConfig
                <*> ltexObj   .:? "java"                            .!= _java                            defaultLTeXConfig
                <*> ltexObj   .:? "sentenceCacheSize"               .!= _sentenceCacheSize               defaultLTeXConfig
                <*> ltexObj   .:? "maxRequestSize"                  .!= _maxRequestSize                  defaultLTeXConfig
                <*> ltexObj   .:? "paragraphCacheTtlMinutes"        .!= _paragraphCacheTtlMinutes        defaultLTeXConfig
                <*> ltexObj   .:? "paragraphCacheEnabled"           .!= _paragraphCacheEnabled           defaultLTeXConfig
                <*> ltexObj   .:? "completionEnabled"               .!= _completionEnabled               defaultLTeXConfig
                <*> ltexObj   .:? "diagnosticSeverity"              .!= _diagnosticSeverity              defaultLTeXConfig 
                <*> ltexObj   .:? "checkFrequency"                  .!= _checkFrequency                  defaultLTeXConfig 
                <*> ltexObj   .:? "clearDiagnosticsWhenClosingFile" .!= _clearDiagnosticsWhenClosingFile defaultLTeXConfig
                <*> ltexObj   .:? "statusBarItem"                   .!= _statusBarItem                   defaultLTeXConfig
                <*> ltexObj   .:? "trace"                           .!= _trace                           defaultLTeXConfig
            
defaultLTeXConfig :: LTeXConfig
defaultLTeXConfig = LTeXConfig
    { _enabled                         = LTeXEnabledArray $ map LTeXMarkupOrProgrammingLanguageMarkup
        [ BibTeX, ConTeXt, XHTML, LaTeX, Markdown, MDX, Typst, AsciiDoc, Neorg, Org
        , Quarto, ReStructuredText, RSweave]
    , _language                        = LTeXNaturalOrAutoDetectLanguageNatural EnglishUS
    , _dictionary                      = mempty
    , _disabledRules                   = mempty
    , _enabledRules                    = mempty
    , _hiddenFalsePositives            = mempty
    , _bibtex                          = defaultBibTeXConfig
    , _latex                           = defaultLaTeXConfig
    , _markdown                        = defaultMarkdownConfig
    , _configurationTarget             = Map.fromList 
        [ (LTeXConfigurationTargetKeyDictionary          , LTeXConfigurationTargetValueWorkspaceFolderExternalFile)
        , (LTeXConfigurationTargetKeyDisabledRules       , LTeXConfigurationTargetValueWorkspaceFolderExternalFile)
        , (LTeXConfigurationTargetKeyHiddenFalsePositives, LTeXConfigurationTargetValueWorkspaceFolderExternalFile)
        ]
    , _additionalRules                 = defaultAdditionalRules
    , _languageToolHttpServerUri       = ""
    , _languageToolOrg                 = defaultLanguageToolOrg
    , _ltexLs                          = defaultLTeXLs
    , _java                            = defaultJavaConfig
    , _sentenceCacheSize               = 0
    , _maxRequestSize                  = 20000
    , _paragraphCacheTtlMinutes        = 30
    , _paragraphCacheEnabled           = True
    , _completionEnabled               = False
    , _diagnosticSeverity              = LTeXDiagnosticSeverityString LTeXDiagnosticSeverityTypeInformation
    , _checkFrequency                  = LTeXCheckFrequencyEdit
    , _clearDiagnosticsWhenClosingFile = True
    , _statusBarItem                   = False
    , _trace                           = defaultTrace
    }

