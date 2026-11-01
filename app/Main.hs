module Main (main) where

import Lib
import Data.Aeson (Value(..), ToJSON(..), FromJSON(..))
import Data.Aeson.Types (prependFailure, unexpected)

data LTeXLanguage = Arabic | Asturian | Belarusian | Breton | Catalan | CatalanValencian | Chinese
                  | Danish | Dutch | DutchBelgium | English | EnglishAustralian | EnglishCanadian
                  | EnglishGB | EnglishNewZealand | EnglishSouthAfrican | EnglishUS | Esperanto
                  | French | Galician | German | GermanAustria | GermanGermany | GermanSwiss
                  | Greek | Irish | Italian | Japanese | Khmer | Persian | Polish | Portuguese
                  | PortugueseAngolaPreAO | PortugueseBrazil | PortugueseMozambiquePreAO
                  | PortuguesePortugal | Romanian | Russian | SimpleGerman | Slovak | Slovenian
                  | Spanish | SpanishVoseo | Swedish | Tagalog | Tamil | Ukrainian

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

data LTeXConfig = LTeXConfig
    { language :: LTeXLanguage
    }

main :: IO ()
main = someFunc
