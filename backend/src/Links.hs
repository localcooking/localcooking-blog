{-# LANGUAGE
    MultiParamTypeClasses
  , RecordWildCards
  , OverloadedStrings
  , OverloadedLists
  , DeriveGeneric
  , QuasiQuotes
  , NamedFieldPuns
  #-}

module Links where

import LocalCooking.Links.Class (LocalCookingSiteLinks (..))

import Data.Monoid ((<>))
import Data.Attoparsec.Text (Parser, parseOnly, char, string, endOfInput)
import Data.Text (Text)
import Path (File, Abs, Rel, absdir, absfile, relfile, toFilePath, (</>))
import Path.Extended (Location (..), ToPath (..), ToLocation (..), FromLocation (..), fromAbsFile)
import qualified Data.Text as T
import Control.Applicative ((<|>))
import Control.Monad (void)
import Unsafe.Coerce (unsafeCoerce)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..), oneof)




data UserDetailsLinks
  = UserDetailsGeneral
  | UserDetailsSecurity
  deriving (Eq, Show, Generic)

instance Arbitrary UserDetailsLinks where
  arbitrary = oneof
    [ pure UserDetailsGeneral
    , pure UserDetailsSecurity
    ]

instance ToPath UserDetailsLinks Rel File where
  toPath x = case x of
    UserDetailsGeneral   -> [relfile|general|]
    UserDetailsSecurity  -> [relfile|security|]

userDetailsLinksParser :: Parser UserDetailsLinks
userDetailsLinksParser = do
  let general = UserDetailsGeneral <$ string "general"
      security = UserDetailsSecurity <$ string "security"
  general <|> security

userDetailsToDocumentTitle :: UserDetailsLinks -> Text
userDetailsToDocumentTitle x = case x of
  UserDetailsGeneral   -> "General - "
  UserDetailsSecurity  -> "Security - "



data SiteLinks
  = RootLink
  | AboutLink
  | RegisterLink
  | UserDetailsLink (Maybe UserDetailsLinks)
  deriving (Eq, Show, Generic)

instance Arbitrary SiteLinks where
  arbitrary = oneof
    [ pure RootLink
    , pure AboutLink
    , pure RegisterLink
    , UserDetailsLink <$> arbitrary
    ]

-- TODO URI / Location parser

instance ToPath SiteLinks Abs File where
  toPath x = case x of
    RootLink -> unsafeCoerce [absdir|/|]
    AboutLink -> [absfile|/about|]
    RegisterLink -> [absfile|/register|]
    UserDetailsLink mDetails -> case mDetails of
      Nothing -> [absfile|/userDetails|]
      Just d -> [absdir|/userDetails/|] </> toPath d


instance ToLocation SiteLinks where
  toLocation = fromAbsFile . toPath

instance FromLocation SiteLinks where
  parseLocation (Location {locPath}) =
    case locPath of
      Left abs | abs == [absdir|/|] -> pure RootLink
               | otherwise -> fail $ "Unknown abs dir: " ++ toFilePath abs
      Right x -> case parseOnly pathParser $ T.pack $ toFilePath x of
        Left e -> fail (show e)
        Right y -> pure y
    where
      pathParser :: Parser SiteLinks
      pathParser = do
        divider
        let root  = RootLink <$ endOfInput
            about = AboutLink <$ string "about"
            register = RegisterLink <$ string "register"
            userDetails = do
              void (string "userDetails")
              let nil = Nothing <$ endOfInput
                  detail = do
                    divider
                    Just <$> userDetailsLinksParser
              UserDetailsLink <$> (detail <|> nil)
        register <|> about <|> userDetails <|> root
      divider = void (char '/')

instance LocalCookingSiteLinks SiteLinks where
  rootLink = RootLink
  registerLink = RegisterLink
  toDocumentTitle x =
    ( case x of
        RootLink -> ""
        AboutLink -> "About - "
        RegisterLink -> "Register - "
        UserDetailsLink mDetails -> case mDetails of
          Nothing -> "User Details - "
          Just d -> userDetailsToDocumentTitle d <> "User Details - "
      ) <> "Local Cooking Blog"


data LogoLinks
  = LogoPng
  | LogoWhitePng
  | IconPng
  | IconSvg


instance ToPath LogoLinks Abs File where
  toPath x = case x of
    LogoPng      -> [absfile|/static/images/logo.png|]
    LogoWhitePng -> [absfile|/static/images/logo-white.png|]
    IconPng      -> [absfile|/static/images/icon.png|]
    IconSvg      -> [absfile|/static/images/icon.svg|]


instance ToLocation LogoLinks where
  toLocation = fromAbsFile . toPath
