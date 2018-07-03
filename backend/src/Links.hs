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
import Data.Text.Permalink (Permalink, permalinkParser, printPermalink)
import Path (File, Abs, Rel, absdir, absfile, relfile, toFilePath, (</>), parseRelFile)
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
  = RootLink (Maybe Permalink)
  | NewBlogPostLink
  | RegisterLink
  | UserDetailsLink (Maybe UserDetailsLinks)
  deriving (Eq, Show, Generic)

instance Arbitrary SiteLinks where
  arbitrary = oneof
    [ RootLink <$> arbitrary
    , pure RegisterLink
    , pure NewBlogPostLink
    , UserDetailsLink <$> arbitrary
    ]

instance ToPath SiteLinks Abs File where
  toPath x = case x of
    RootLink mLink -> case mLink of
      Nothing -> unsafeCoerce [absdir|/|]
      Just link -> [absdir|/|] </> case parseRelFile $ T.unpack $ printPermalink link of
        Just linkFile -> linkFile
    RegisterLink -> [absfile|/register|]
    NewBlogPostLink -> [absfile|/newBlogPost|]
    UserDetailsLink mDetails -> case mDetails of
      Nothing -> [absfile|/userDetails|]
      Just d -> [absdir|/userDetails/|] </> toPath d


instance ToLocation SiteLinks where
  toLocation = fromAbsFile . toPath

instance FromLocation SiteLinks where
  parseLocation (Location {locPath}) =
    case locPath of
      Left abs | abs == [absdir|/|] -> pure (RootLink Nothing)
               | otherwise          -> fail $ "Unknown abs dir: " ++ toFilePath abs
      Right x -> case parseOnly pathParser $ T.pack $ toFilePath x of
        Left e -> fail (show e)
        Right y -> pure y
    where
      pathParser :: Parser SiteLinks
      pathParser = do
        divider
        let blogPost = do
              let nil = Nothing <$ endOfInput
                  post = do
                    divider
                    Just <$> permalinkParser
              RootLink <$> (post <|> nil)
            newBlogPost = NewBlogPostLink <$ string "newBlogPost"
            register = RegisterLink <$ string "register"
            userDetails = do
              void (string "userDetails")
              let nil = Nothing <$ endOfInput
                  detail = do
                    divider
                    Just <$> userDetailsLinksParser
              UserDetailsLink <$> (detail <|> nil)
        register <|> newBlogPost <|> userDetails <|> blogPost
      divider = void (char '/')

instance LocalCookingSiteLinks SiteLinks where
  rootLink = RootLink Nothing
  registerLink = RegisterLink
  toDocumentTitle x =
    ( case x of
        RootLink mPost -> case mPost of
          Nothing -> ""
          Just post -> printPermalink post <> " - "
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
