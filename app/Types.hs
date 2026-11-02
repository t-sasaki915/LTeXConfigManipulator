{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy (toStrict)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
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
    toJSONKey = ToJSONKeyValue toJSON toEncoding

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
    fromJSONKey = FromJSONKeyValue parseJSON

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
    toJSONKey = ToJSONKeyValue toJSON toEncoding

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
    fromJSONKey = FromJSONKeyValue parseJSON

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
    toJSONKey = ToJSONKeyValue toJSON toEncoding

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
    fromJSONKey = FromJSONKeyValue parseJSON

data LTeXAutoDetectLanguage = LTeXAutoDetectLanguage
                            deriving (Show, Eq, Ord)

instance ToJSON LTeXAutoDetectLanguage where
    toJSON LTeXAutoDetectLanguage = String "auto"

instance ToJSONKey LTeXAutoDetectLanguage where
    toJSONKey = ToJSONKeyValue toJSON toEncoding

instance FromJSON LTeXAutoDetectLanguage where
    parseJSON (String "auto") = pure LTeXAutoDetectLanguage

    parseJSON invalid = prependFailure "parsing LTeXAutoDetectLanguage failed, " (unexpected invalid)

instance FromJSONKey LTeXAutoDetectLanguage where
    fromJSONKey = FromJSONKeyValue parseJSON

data LTeXNoneLanguage = LTeXNoneLanguage
                      deriving (Show, Eq, Ord)

instance ToJSON LTeXNoneLanguage where
    toJSON LTeXNoneLanguage = String ""

instance ToJSONKey LTeXNoneLanguage where
    toJSONKey = ToJSONKeyValue toJSON toEncoding

instance FromJSON LTeXNoneLanguage where
    parseJSON (String "") = pure LTeXNoneLanguage

    parseJSON invalid = prependFailure "parsing LTeXNoneLanguage failed, " (unexpected invalid)

instance FromJSONKey LTeXNoneLanguage where
    fromJSONKey = FromJSONKeyValue parseJSON

data LTeXMarkupOrProgrammingLanguage = LTeXMarkupOrProgrammingLanguageMarkup      LTeXMarkupLanguage
                                     | LTeXMarkupOrProgrammingLanguageProgramming LTeXProgrammingLanguage
                                     deriving (Show, Eq, Ord)

instance ToJSON LTeXMarkupOrProgrammingLanguage where
    toJSON (LTeXMarkupOrProgrammingLanguageMarkup x)      = toJSON x
    toJSON (LTeXMarkupOrProgrammingLanguageProgramming x) = toJSON x

instance ToJSONKey LTeXMarkupOrProgrammingLanguage where
    toJSONKey = ToJSONKeyValue toJSON toEncoding

instance FromJSON LTeXMarkupOrProgrammingLanguage where
    parseJSON value =
            (LTeXMarkupOrProgrammingLanguageMarkup <$> parseJSON value)
        <|> (LTeXMarkupOrProgrammingLanguageProgramming <$> parseJSON value)

instance FromJSONKey LTeXMarkupOrProgrammingLanguage where
    fromJSONKey = FromJSONKeyValue parseJSON

data LTeXNaturalOrAutoDetectLanguage = LTeXNaturalOrAutoDetectLanguageNatural LTeXNaturalLanguage
                                     | LTeXNaturalOrAutoDetectLanguageAuto    LTeXAutoDetectLanguage
                                     deriving (Show, Eq, Ord)

instance ToJSON LTeXNaturalOrAutoDetectLanguage where
    toJSON (LTeXNaturalOrAutoDetectLanguageNatural x) = toJSON x
    toJSON (LTeXNaturalOrAutoDetectLanguageAuto x)    = toJSON x

instance ToJSONKey LTeXNaturalOrAutoDetectLanguage where
    toJSONKey = ToJSONKeyValue toJSON toEncoding

instance FromJSON LTeXNaturalOrAutoDetectLanguage where
    parseJSON value =
            (LTeXNaturalOrAutoDetectLanguageAuto <$> parseJSON value)
        <|> (LTeXNaturalOrAutoDetectLanguageNatural <$> parseJSON value)

instance FromJSONKey LTeXNaturalOrAutoDetectLanguage where
    fromJSONKey = FromJSONKeyValue parseJSON

data LTeXNaturalOrNoneLanguage = LTeXNaturalOrNoneLanguageNatural LTeXNaturalLanguage
                               | LTeXNaturalOrNoneLanguageNone    LTeXNoneLanguage
                               deriving (Show, Eq, Ord)

instance ToJSON LTeXNaturalOrNoneLanguage where
    toJSON (LTeXNaturalOrNoneLanguageNatural x) = toJSON x
    toJSON (LTeXNaturalOrNoneLanguageNone x)    = toJSON x

instance ToJSONKey LTeXNaturalOrNoneLanguage where
    toJSONKey = ToJSONKeyValue toJSON toEncoding

instance FromJSON LTeXNaturalOrNoneLanguage where
    parseJSON value =
            (LTeXNaturalOrNoneLanguageNone <$> parseJSON value)
        <|> (LTeXNaturalOrNoneLanguageNatural <$> parseJSON value)

instance FromJSONKey LTeXNaturalOrNoneLanguage where
    fromJSONKey = FromJSONKeyValue parseJSON

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
    { fields :: Map Text Bool
    }
    deriving (Show, Eq)

instance ToJSON BibTeXConfig where
    toJSON conf = object
        [ "fields" .= fields conf
        ]

instance FromJSON BibTeXConfig where
    parseJSON = withObject "BibTeXConfig" $ \obj -> BibTeXConfig
        <$> obj .:? "fields" .!= fields defaultBibTeXConfig

defaultBibTeXConfig :: BibTeXConfig
defaultBibTeXConfig = BibTeXConfig
    { fields = mempty
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
        <$> obj .:? "commands"     .!= commands     defaultLaTeXConfig
        <*> obj .:? "environments" .!= environments defaultLaTeXConfig

defaultLaTeXConfig :: LaTeXConfig
defaultLaTeXConfig = LaTeXConfig
    { commands     = mempty
    , environments = mempty
    }

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
        <$> obj .:? "nodes" .!= nodes defaultMarkdownConfig

defaultMarkdownConfig :: MarkdownConfig
defaultMarkdownConfig = MarkdownConfig
    { nodes = mempty
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

data AdditionalRules = AdditionalRules
    { enablePickyRules :: Bool
    , motherTongue     :: LTeXNaturalOrNoneLanguage
    , languageModel    :: Text
    }
    deriving (Show, Eq)

instance ToJSON AdditionalRules where
    toJSON conf = object
        [ "enablePickyRules" .= enablePickyRules conf
        ]

instance FromJSON AdditionalRules where
    parseJSON = withObject "AdditionalRules" $ \obj -> AdditionalRules
        <$> obj .:? "enablePickyRules" .!= enablePickyRules defaultAdditionalRules
        <*> obj .:? "motherTongue"     .!= motherTongue     defaultAdditionalRules
        <*> obj .:? "languageModel"    .!= languageModel    defaultAdditionalRules

defaultAdditionalRules :: AdditionalRules
defaultAdditionalRules = AdditionalRules
    { enablePickyRules = False
    , motherTongue     = LTeXNaturalOrNoneLanguageNone LTeXNoneLanguage
    , languageModel    = ""
    }

data LanguageToolOrg = LanguageToolOrg
    { username :: Text
    , apiKey   :: Text
    }
    deriving (Show, Eq)

instance ToJSON LanguageToolOrg where
    toJSON conf = object
        [ "username" .= username conf
        , "apiKey"   .= apiKey conf
        ]

instance FromJSON LanguageToolOrg where
    parseJSON = withObject "LanguageToolOrg" $ \obj -> LanguageToolOrg
        <$> obj .:? "username" .!= username defaultLanguageToolOrg
        <*> obj .:? "apiKey"   .!= apiKey   defaultLanguageToolOrg

defaultLanguageToolOrg :: LanguageToolOrg
defaultLanguageToolOrg = LanguageToolOrg
    { username = ""
    , apiKey   = ""
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
    { ltexLsPath  :: Text
    , logLevel    :: LTeXLsLogLevel
    }
    deriving (Show, Eq)

instance ToJSON LTeXLs where
    toJSON conf = object
        [ "path"     .= ltexLsPath conf
        , "logLevel" .= logLevel conf
        ]

instance FromJSON LTeXLs where
    parseJSON = withObject "LTeXLs" $ \obj -> LTeXLs
        <$> obj .:? "path"     .!= ltexLsPath defaultLTeXLs
        <*> obj .:? "logLevel" .!= logLevel   defaultLTeXLs

defaultLTeXLs :: LTeXLs
defaultLTeXLs = LTeXLs
    { ltexLsPath = ""
    , logLevel   = LTeXLsLogLevelFine
    }

data JavaConfig = JavaConfig
    { javaPath        :: Text
    , initialHeapSize :: Int
    , maximumHeapSize :: Int
    }
    deriving (Show, Eq)

instance ToJSON JavaConfig where
    toJSON conf = object
        [ "path"            .= javaPath conf
        , "initialHeapSize" .= initialHeapSize conf
        , "maximumHeapSize" .= maximumHeapSize conf
        ]

instance FromJSON JavaConfig where
    parseJSON = withObject "Java" $ \obj -> JavaConfig
        <$> obj .:? "path"            .!= javaPath        defaultJavaConfig
        <*> obj .:? "initialHeapSize" .!= initialHeapSize defaultJavaConfig
        <*> obj .:? "maximumHeapSize" .!= maximumHeapSize defaultJavaConfig

defaultJavaConfig :: JavaConfig
defaultJavaConfig = JavaConfig
    { javaPath        = ""
    , initialHeapSize = 64
    , maximumHeapSize = 2048
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
    { server :: TraceServerMode 
    }
    deriving (Show, Eq)

instance ToJSON Trace where
    toJSON conf = object
        [ "server" .= server conf
        ]

instance FromJSON Trace where
    parseJSON = withObject "Trace" $ \obj -> Trace
        <$> obj .:? "server" .!= server defaultTrace

defaultTrace :: Trace
defaultTrace = Trace
    { server = TraceServerModeOff
    }

data LTeXConfig = LTeXConfig
    { enabled                         :: LTeXEnabled
    , language                        :: LTeXNaturalOrAutoDetectLanguage
    , dictionary                      :: Map LTeXNaturalLanguage [Text]
    , disabledRules                   :: Map LTeXNaturalLanguage [Text]
    , enabledRules                    :: Map LTeXNaturalLanguage [Text]
    , hiddenFalsePositives            :: Map LTeXNaturalLanguage [HiddenFalsePositivesValue]
    , bibtex                          :: BibTeXConfig
    , latex                           :: LaTeXConfig
    , markdown                        :: MarkdownConfig
    , configurationTarget             :: Map LTeXConfigurationTargetKey LTeXConfigurationTargetValue
    , additionalRules                 :: AdditionalRules
    , languageToolHttpServerUri       :: Text
    , languageToolOrg                 :: LanguageToolOrg
    , ltexLs                          :: LTeXLs
    , java                            :: JavaConfig
    , sentenceCacheSize               :: Int
    , maxRequestSize                  :: Int
    , paragraphCacheTtlMinutes        :: Int
    , paragraphCacheEnabled           :: Bool
    , completionEnabled               :: Bool
    , diagnosticSeverity              :: LTeXDiagnosticSeverity
    , checkFrequency                  :: LTeXCheckFrequency
    , clearDiagnosticsWhenClosingFile :: Bool
    , statusBarItem                   :: Bool
    , trace                           :: Trace
    }
    deriving (Show, Eq)

instance ToJSON LTeXConfig where
    toJSON conf = object
        [ "ltex" .= object
            [ "enabled"                         .= enabled conf
            , "language"                        .= language conf
            , "dictionary"                      .= dictionary conf
            , "disabledRules"                   .= disabledRules conf
            , "enabledRules"                    .= enabledRules conf
            , "hiddenFalsePositives"            .= fmap (map hiddenFalsePositivesToText) (hiddenFalsePositives conf)
            , "bibtex"                          .= bibtex conf
            , "latex"                           .= latex conf
            , "markdown"                        .= markdown conf
            , "configurationTarget"             .= configurationTarget conf
            , "additionalRules"                 .= additionalRules conf
            , "languageToolHttpServerUri"       .= languageToolHttpServerUri conf
            , "languageToolOrg"                 .= languageToolOrg conf
            , "ltex-ls"                         .= ltexLs conf
            , "java"                            .= java conf
            , "sentenceCacheSize"               .= sentenceCacheSize conf
            , "maxRequestSize"                  .= maxRequestSize conf
            , "paragraphCacheTtlMinutes"        .= paragraphCacheTtlMinutes conf
            , "paragraphCacheEnabled"           .= paragraphCacheEnabled conf
            , "completionEnabled"               .= completionEnabled conf
            , "diagnosticSeverity"              .= diagnosticSeverity conf
            , "checkFrequency"                  .= checkFrequency conf
            , "clearDiagnosticsWhenClosingFile" .= clearDiagnosticsWhenClosingFile conf
            , "statusBarItem"                   .= statusBarItem conf
            , "trace"                           .= trace conf
            ]
        ]

instance FromJSON LTeXConfig where
    parseJSON = withObject "LTeXConfig" $ \obj ->
        obj .: "ltex" >>= \ltex ->
            withObject "ltex" parseLTeXConfig ltex
        where
            parseLTeXConfig = \ltexObj -> LTeXConfig
                <$> ltexObj   .:? "enabled"                         .!= enabled                         defaultLTeXConfig
                <*> ltexObj   .:? "language"                        .!= language                        defaultLTeXConfig 
                <*> ltexObj   .:? "dictionary"                      .!= dictionary                      defaultLTeXConfig
                <*> ltexObj   .:? "disabledRules"                   .!= disabledRules                   defaultLTeXConfig
                <*> ltexObj   .:? "enabledRules"                    .!= enabledRules                    defaultLTeXConfig
                <*> ((ltexObj .:? "hiddenFalsePositives"            .!= mempty) >>= traverse (traverse hiddenFalsePositivesFromText))
                <*> ltexObj   .:? "bibtex"                          .!= bibtex                          defaultLTeXConfig
                <*> ltexObj   .:? "latex"                           .!= latex                           defaultLTeXConfig
                <*> ltexObj   .:? "markdown"                        .!= markdown                        defaultLTeXConfig
                <*> ltexObj   .:? "configurationTarget"             .!= configurationTarget             defaultLTeXConfig
                <*> ltexObj   .:? "additionalRules"                 .!= additionalRules                 defaultLTeXConfig
                <*> ltexObj   .:? "languageToolHttpServerUri"       .!= languageToolHttpServerUri       defaultLTeXConfig
                <*> ltexObj   .:? "languageToolOrg"                 .!= languageToolOrg                 defaultLTeXConfig
                <*> ltexObj   .:? "ltex-ls"                         .!= ltexLs                          defaultLTeXConfig
                <*> ltexObj   .:? "java"                            .!= java                            defaultLTeXConfig
                <*> ltexObj   .:? "sentenceCacheSize"               .!= sentenceCacheSize               defaultLTeXConfig
                <*> ltexObj   .:? "maxRequestSize"                  .!= maxRequestSize                  defaultLTeXConfig
                <*> ltexObj   .:? "paragraphCacheTtlMinutes"        .!= paragraphCacheTtlMinutes        defaultLTeXConfig
                <*> ltexObj   .:? "paragraphCacheEnabled"           .!= paragraphCacheEnabled           defaultLTeXConfig
                <*> ltexObj   .:? "completionEnabled"               .!= completionEnabled               defaultLTeXConfig
                <*> ltexObj   .:? "diagnosticSeverity"              .!= diagnosticSeverity              defaultLTeXConfig 
                <*> ltexObj   .:? "checkFrequency"                  .!= checkFrequency                  defaultLTeXConfig 
                <*> ltexObj   .:? "clearDiagnosticsWhenClosingFile" .!= clearDiagnosticsWhenClosingFile defaultLTeXConfig
                <*> ltexObj   .:? "statusBarItem"                   .!= statusBarItem                   defaultLTeXConfig
                <*> ltexObj   .:? "trace"                           .!= trace                           defaultLTeXConfig
            
defaultLTeXConfig :: LTeXConfig
defaultLTeXConfig = LTeXConfig
    { enabled                         = LTeXEnabledArray $ map LTeXMarkupOrProgrammingLanguageMarkup
        [ BibTeX, ConTeXt, XHTML, LaTeX, Markdown, MDX, Typst, AsciiDoc, Neorg, Org
        , Quarto, ReStructuredText, RSweave]
    , language                        = LTeXNaturalOrAutoDetectLanguageNatural EnglishUS
    , dictionary                      = mempty
    , disabledRules                   = mempty
    , enabledRules                    = mempty
    , hiddenFalsePositives            = mempty
    , bibtex                          = defaultBibTeXConfig
    , latex                           = defaultLaTeXConfig
    , markdown                        = defaultMarkdownConfig
    , configurationTarget             = Map.fromList 
        [ (LTeXConfigurationTargetKeyDictionary          , LTeXConfigurationTargetValueWorkspaceFolderExternalFile)
        , (LTeXConfigurationTargetKeyDisabledRules       , LTeXConfigurationTargetValueWorkspaceFolderExternalFile)
        , (LTeXConfigurationTargetKeyHiddenFalsePositives, LTeXConfigurationTargetValueWorkspaceFolderExternalFile)
        ]
    , additionalRules                 = defaultAdditionalRules
    , languageToolHttpServerUri       = ""
    , languageToolOrg                 = defaultLanguageToolOrg
    , ltexLs                          = defaultLTeXLs
    , java                            = defaultJavaConfig
    , sentenceCacheSize               = 0
    , maxRequestSize                  = 20000
    , paragraphCacheTtlMinutes        = 30
    , paragraphCacheEnabled           = True
    , completionEnabled               = False
    , diagnosticSeverity              = LTeXDiagnosticSeverityString LTeXDiagnosticSeverityTypeInformation
    , checkFrequency                  = LTeXCheckFrequencyEdit
    , clearDiagnosticsWhenClosingFile = True
    , statusBarItem                   = False
    , trace                           = defaultTrace
    }

data ConfigPath a where
    Enabled                         :: ConfigPath LTeXEnabled
    Language                        :: ConfigPath LTeXNaturalOrAutoDetectLanguage
    Dictionary                      :: LTeXNaturalLanguage -> ConfigPath [Text]
    DisabledRules                   :: LTeXNaturalLanguage -> ConfigPath [Text]
    EnabledRules                    :: LTeXNaturalLanguage -> ConfigPath [Text]
    HiddenFalsePositives            :: LTeXNaturalLanguage -> ConfigPath [HiddenFalsePositivesValue]
    BibTeXFields                    :: Text -> ConfigPath Bool
    LaTeXCommands                   :: Text -> ConfigPath LTeXAction
    LaTeXEnvironments               :: Text -> ConfigPath LaTeXEnvironmentAction
    MarkdownNodes                   :: Text -> ConfigPath LTeXAction
    ConfigurationTarget             :: LTeXConfigurationTargetKey -> ConfigPath LTeXConfigurationTargetValue
    AdditionalRulesEnablePickyRules :: ConfigPath Bool
    AdditionalRulesMotherTongue     :: ConfigPath LTeXNaturalOrNoneLanguage
    AdditionalRulesLanguageModel    :: ConfigPath Text
    LanguageToolHttpServerUri       :: ConfigPath Text
    LanguageToolOrgUsername         :: ConfigPath Text
    LanguageToolOrgApiKey           :: ConfigPath Text
    LTeXLsPath                      :: ConfigPath Text
    LTeXLsLogLevel                  :: ConfigPath LTeXLsLogLevel
    JavaPath                        :: ConfigPath Text
    JavaInitialHeapSize             :: ConfigPath Int
    JavaMaximumHeapSize             :: ConfigPath Int
    SentenceCacheSize               :: ConfigPath Int
    MaxRequestSize                  :: ConfigPath Int
    ParagraphCacheTtlMinutes        :: ConfigPath Int
    ParagraphCacheEnabled           :: ConfigPath Bool
    CompletionEnabled               :: ConfigPath Bool
    DiagnosticSeverity              :: ConfigPath LTeXDiagnosticSeverity
    CheckFrequency                  :: ConfigPath LTeXCheckFrequency
    ClearDiagnosticsWhenClosingFile :: ConfigPath Bool
    StatusBarItem                   :: ConfigPath Bool
    TraceServer                     :: ConfigPath TraceServerMode

data SomeConfigPath where
    SomeConfigPath :: ConfigPath a -> SomeConfigPath

configPathFromText :: Text -> Maybe SomeConfigPath
configPathFromText input =
    case Text.splitOn "." input of
        ["enabled"]                             -> Just (SomeConfigPath Enabled)
        ["language"]                            -> Just (SomeConfigPath Language)
        ("dictionary" : rest)                   -> decodeRestAsNatLang rest >>= pure . SomeConfigPath . Dictionary
        ("disabledRules" : rest)                -> decodeRestAsNatLang rest >>= pure . SomeConfigPath . DisabledRules
        ("enabledRules" : rest)                 -> decodeRestAsNatLang rest >>= pure . SomeConfigPath . EnabledRules
        ("hiddenFalsePositives" : rest)         -> decodeRestAsNatLang rest >>= pure . SomeConfigPath . HiddenFalsePositives
        ("bibtex" : "fields" : rest)            -> Just (SomeConfigPath (BibTeXFields      (unsplitRest rest)))
        ("latex" : "commands" : rest)           -> Just (SomeConfigPath (LaTeXCommands     (unsplitRest rest)))
        ("latex" : "environments" : rest)       -> Just (SomeConfigPath (LaTeXEnvironments (unsplitRest rest)))
        ("markdown" : "nodes" : rest)           -> Just (SomeConfigPath (MarkdownNodes     (unsplitRest rest)))
        ("configurationTarget" : rest)          -> decodeRestAsTargetKey rest >>= pure . SomeConfigPath . ConfigurationTarget
        ["additionalRules", "enablePickyRules"] -> Just (SomeConfigPath AdditionalRulesEnablePickyRules)
        ["additionalRules", "motherTongue"]     -> Just (SomeConfigPath AdditionalRulesMotherTongue)
        ["additionalRules", "languageModel"]    -> Just (SomeConfigPath AdditionalRulesLanguageModel)
        ["languageToolHttpServerUri"]           -> Just (SomeConfigPath LanguageToolHttpServerUri)
        ["languageToolOrg", "username"]         -> Just (SomeConfigPath LanguageToolOrgUsername)
        ["languageToolOrg", "apiKey"]           -> Just (SomeConfigPath LanguageToolOrgApiKey)
        ["ltex-ls", "path"]                     -> Just (SomeConfigPath LTeXLsPath)
        ["ltex-ls", "logLevel"]                 -> Just (SomeConfigPath LTeXLsLogLevel)
        ["java", "path"]                        -> Just (SomeConfigPath JavaPath)
        ["java", "initialHeapSize"]             -> Just (SomeConfigPath JavaInitialHeapSize)
        ["java", "maximumHeapSize"]             -> Just (SomeConfigPath JavaMaximumHeapSize)
        ["sentenceCacheSize"]                   -> Just (SomeConfigPath SentenceCacheSize)
        ["maxRequestSize"]                      -> Just (SomeConfigPath MaxRequestSize)
        ["paragraphCacheTtlMinutes"]            -> Just (SomeConfigPath ParagraphCacheTtlMinutes)
        ["paragraphCacheEnabled"]               -> Just (SomeConfigPath ParagraphCacheEnabled)
        ["completionEnabled"]                   -> Just (SomeConfigPath CompletionEnabled)
        ["diagnosticSeverity"]                  -> Just (SomeConfigPath DiagnosticSeverity)
        ["checkFrequency"]                      -> Just (SomeConfigPath CheckFrequency)
        ["clearDiagnosticsWhenClosingFile"]     -> Just (SomeConfigPath ClearDiagnosticsWhenClosingFile)
        ["statusBarItem"]                       -> Just (SomeConfigPath StatusBarItem)
        ["trace", "server"]                     -> Just (SomeConfigPath TraceServer)
        _                                       -> Nothing
    where
        unsplitRest = Text.intercalate "."

        decodeRestAsNatLang   = decodeStrict . encodeUtf8 . unsplitRest
        decodeRestAsTargetKey = decodeStrict . encodeUtf8 . unsplitRest

data Operation = Get | Set | Add | Remove deriving (Show, Eq, Ord)

operationFromText :: Text -> Maybe Operation
operationFromText "get"    = Just Get
operationFromText "set"    = Just Set
operationFromText "add"    = Just Add
operationFromText "remove" = Just Remove
operationFromText _        = Nothing

class AllowedOperations a where
    allowedOperations :: ConfigPath a -> [Operation]

instance AllowedOperations LTeXEnabled where
    allowedOperations _ = [Get, Set]

instance AllowedOperations LTeXNaturalOrAutoDetectLanguage where
    allowedOperations _ = [Get, Set]

instance AllowedOperations [Text] where
    allowedOperations _ = [Get, Set, Add, Remove]

instance AllowedOperations [HiddenFalsePositivesValue] where
    allowedOperations _ = [Get, Set, Add, Remove]

instance AllowedOperations Bool where
    allowedOperations _ = [Get, Set]

instance AllowedOperations LTeXAction where
    allowedOperations _ = [Get, Set]

instance AllowedOperations LaTeXEnvironmentAction where
    allowedOperations _ = [Get, Set]

instance AllowedOperations LTeXConfigurationTargetValue where
    allowedOperations _ = [Get, Set]

instance AllowedOperations LTeXNaturalOrNoneLanguage where
    allowedOperations _ = [Get, Set]

instance AllowedOperations Text where
    allowedOperations _ = [Get, Set]

instance AllowedOperations LTeXLsLogLevel where
    allowedOperations _ = [Get, Set]

instance AllowedOperations Int where
    allowedOperations _ = [Get, Set]

instance AllowedOperations LTeXDiagnosticSeverity where
    allowedOperations _ = [Get, Set]

instance AllowedOperations LTeXCheckFrequency where
    allowedOperations _ = [Get, Set]

instance AllowedOperations TraceServerMode where
    allowedOperations _ = [Get, Set]

