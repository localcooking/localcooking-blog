module Links where

import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks, class LocalCookingUserDetailsLinks, replaceState', defaultSiteLinksPathParser)

import Prelude
import Data.String.Permalink (Permalink, permalinkParser)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.URI (Query (..))
import Data.URI.URI as URI
import Data.URI.Path as URIPath
import Data.URI.Location (class ToLocation, toLocation, class FromLocation, fromLocation, Location (..), fromURI, printLocation)
import Data.StrMap as StrMap
import Data.Path.Pathy ((</>), dir, file, rootDir, Path, Rel, File, Sandboxed)
import Data.Generic (class Generic, gEq, gShow)
import Data.NonEmpty ((:|))
import Text.Parsing.StringParser (Parser, try, runParser)
import Text.Parsing.StringParser.String (char, string)
import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, warn)

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (location, history)
import DOM.HTML.Location (href)
import DOM.HTML.Types (HISTORY)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)




data UserDetailsLinks
  = UserDetailsGeneralLink
  | UserDetailsSecurityLink

derive instance genericUserDetailsLinks :: Generic UserDetailsLinks

instance eqUserDetailsLinks :: Eq UserDetailsLinks where
  eq = gEq

instance showUserDetailsLinks :: Show UserDetailsLinks where
  show = gShow

instance arbitraryUserDetailsLinks :: Arbitrary UserDetailsLinks where
  arbitrary = oneOf $
    ( pure UserDetailsGeneralLink
    ) :|
    [ pure UserDetailsSecurityLink
    ]

instance localCookingUserDetailsLinksUserDetailsLinks :: LocalCookingUserDetailsLinks UserDetailsLinks where
  userDetailsGeneralLink = UserDetailsGeneralLink
  userDetailsSecurityLink = UserDetailsSecurityLink
  toUserDetailsDocumentTitle x = case x of
    UserDetailsGeneralLink   -> "General - "
    UserDetailsSecurityLink  -> "Security - "

userDetailsLinksToPath :: UserDetailsLinks -> Path Rel File Sandboxed
userDetailsLinksToPath x = case x of
  UserDetailsGeneralLink -> file "general"
  UserDetailsSecurityLink -> file "security"

userDetailsLinksParser :: Parser UserDetailsLinks
userDetailsLinksParser = do
  void divider
  let general = do
        void (string "general")
        pure UserDetailsGeneralLink
      security = do
        void (string "security")
        pure UserDetailsSecurityLink
  try general
    <|> security
  where
    divider = char '/'


data SiteLinks
  = RootLink (Maybe Permalink)
  | NewBlogPostLink
  | RegisterLink
  | UserDetailsLink (Maybe UserDetailsLinks)
  | EmailConfirmLink

instance arbitrarySiteLinks :: Arbitrary SiteLinks where
  arbitrary = oneOf $
        (RootLink <$> arbitrary)
    :|  [ pure RegisterLink
        , pure NewBlogPostLink
        , UserDetailsLink <$> arbitrary
        , pure EmailConfirmLink
        ]

derive instance genericSiteLinks :: Generic SiteLinks

instance showSiteLinks :: Show SiteLinks where
  show = printLocation <<< toLocation

instance eqSiteLinks :: Eq SiteLinks where
  eq = gEq


instance toLocationSiteLinks :: ToLocation SiteLinks where
  toLocation x = case x of
    RootLink mPost -> case mPost of
      Nothing -> Location (Left rootDir) Nothing Nothing
      Just post -> Location (Right $ rootDir </> file (show post)) Nothing Nothing
    NewBlogPostLink -> Location (Right $ rootDir </> file "newBlogPost") Nothing Nothing
    RegisterLink -> Location (Right $ rootDir </> file "register") Nothing Nothing
    EmailConfirmLink -> Location (Right $ rootDir </> file "emailConfirm") Nothing Nothing
    UserDetailsLink mUserDetails ->
      Location
        ( Right $ case mUserDetails of
             Nothing -> rootDir </> file "userDetails"
             Just d -> rootDir </> dir "userDetails" </> userDetailsLinksToPath d
        ) Nothing Nothing


instance localCookingSiteLinksSiteLinks :: LocalCookingSiteLinks SiteLinks UserDetailsLinks where
  rootLink = RootLink Nothing
  registerLink = RegisterLink
  userDetailsLink = UserDetailsLink
  emailConfirmLink = EmailConfirmLink
  getUserDetailsLink link = case link of
    UserDetailsLink mDetails -> Just mDetails
    _ -> Nothing
  toDocumentTitle _ = ""
  subsidiaryTitle _ = " Blog"


-- Policy: don't fail on bad query params / fragment unless you have to
instance fromLocationSiteLinks :: FromLocation SiteLinks where
  fromLocation (Location path mQuery mFrag) = do
    case runParser siteLinksPathParser (URIPath.printPath path) of
      Left e -> Left (show e)
      Right link -> pure link
    where
      siteLinksPathParser :: Parser SiteLinks
      siteLinksPathParser = do
        divider
        let def = defaultSiteLinksPathParser userDetailsLinksParser
            blogPost = RootLink <<< Just <$> permalinkParser
            newBlogPost = NewBlogPostLink <$ string "newBlogPost"
            emailConfirm = EmailConfirmLink <$ string "emailConfirm"
        try emailConfirm
          <|> try newBlogPost
          <|> try blogPost
          <|> def
        where
          divider = void (char '/')
        -- TODO put nonstandard parsers here

