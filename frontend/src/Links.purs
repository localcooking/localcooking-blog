module Links where

import LocalCooking.Dependencies.Blog
  ( GetBlogPostSparrowClientQueues, GetBlogPostCategorySparrowClientQueues)
import LocalCooking.Common.Blog
  (BlogPostVariant, blogPostVariantParser, printBlogPostVariant)
import LocalCooking.Semantics.Blog (GetBlogPost (..), GetBlogPostCategory (..))
import LocalCooking.Database.Schema (StoredBlogPostCategoryId (..))
import LocalCooking.Global.Links.Class
  (class LocalCookingSiteLinks, class LocalCookingUserDetailsLinks, defaultSiteLinksPathParser)

import Prelude
import Data.String.Permalink (Permalink, permalinkParser)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Tuple (Tuple (..))
import Data.URI.Path as URIPath
import Data.URI.Location (class ToLocation, class FromLocation, Location (..))
import Data.Path.Pathy ((</>), dir, file, rootDir, Path, Rel, File, Sandboxed)
import Data.Generic (class Generic, gEq, gShow)
import Data.NonEmpty ((:|))
import Data.Argonaut.JSONTuple (JSONTuple (..))
import Text.Parsing.StringParser (Parser, try, runParser)
import Text.Parsing.StringParser.String (char, string)
import Text.Parsing.StringParser.Int (int)
import Control.Parallel (parallel, sequential)
import Control.Alternative ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE, warn)

import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)
import Queue.One.Aff as OneIO



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
  = RootLink
  | BlogPostCategoryLink BlogPostVariant Permalink
  | BlogPostLink BlogPostVariant Permalink Permalink
  | NewBlogPostCategoryLink BlogPostVariant
  | NewBlogPostLink StoredBlogPostCategoryId
  | RegisterLink
  | UserDetailsLink (Maybe UserDetailsLinks)
  | EmailConfirmLink

instance arbitrarySiteLinks :: Arbitrary SiteLinks where
  arbitrary = oneOf $
        (pure RootLink)
    :|  [ BlogPostCategoryLink <$> arbitrary <*> arbitrary
        , BlogPostLink <$> arbitrary <*> arbitrary <*> arbitrary
        , pure RegisterLink
        , NewBlogPostCategoryLink <$> arbitrary
        , NewBlogPostLink <$> arbitrary
        , UserDetailsLink <$> arbitrary
        , pure EmailConfirmLink
        ]

derive instance genericSiteLinks :: Generic SiteLinks

instance showSiteLinks :: Show SiteLinks where
  show = gShow -- printLocation <<< toLocation

instance eqSiteLinks :: Eq SiteLinks where
  eq = gEq


instance toLocationSiteLinks :: ToLocation SiteLinks where
  toLocation x = case x of
    RootLink -> Location (Left rootDir) Nothing Nothing
    BlogPostCategoryLink variant cat ->
      Location (Right $ rootDir </> dir (printBlogPostVariant variant)
                                </> file (show cat)) Nothing Nothing
    BlogPostLink variant cat post ->
      Location (Right $ rootDir </> dir (printBlogPostVariant variant)
                                </> dir (show cat)
                                </> file (show post)) Nothing Nothing
    NewBlogPostCategoryLink variant ->
      Location (Right $ rootDir </> dir "newBlogPostCategory"
                                </> file (show variant)) Nothing Nothing
    NewBlogPostLink cat ->
      Location (Right $ rootDir </> dir "newBlogPost"
                                </> file (show $ (\(StoredBlogPostCategoryId x) -> x) cat)) Nothing Nothing
    RegisterLink ->
      Location (Right $ rootDir </> file "register") Nothing Nothing
    EmailConfirmLink ->
      Location (Right $ rootDir </> file "emailConfirm") Nothing Nothing
    UserDetailsLink mUserDetails ->
      Location
        ( Right $ case mUserDetails of
             Nothing -> rootDir </> file "userDetails"
             Just d -> rootDir </> dir "userDetails" </> userDetailsLinksToPath d
        ) Nothing Nothing


instance localCookingSiteLinksSiteLinks :: LocalCookingSiteLinks SiteLinks UserDetailsLinks where
  rootLink = RootLink
  registerLink = RegisterLink
  userDetailsLink = UserDetailsLink
  emailConfirmLink = EmailConfirmLink
  getUserDetailsLink link = case link of
    UserDetailsLink mDetails -> Just mDetails
    _ -> Nothing
  subsidiaryTitle _ = " Blog"


initToDocumentTitle :: SiteLinks -> String
initToDocumentTitle link = case link of
  NewBlogPostCategoryLink variant -> "New Blog Post Category - " <> show variant <> " - "
  NewBlogPostLink cat -> "New Blog Post - " <> show ((\(StoredBlogPostCategoryId x) -> x) cat) <> " - "
  RootLink -> ""
  BlogPostCategoryLink variant cat ->
    show cat <> " - " <> printBlogPostVariant variant <> " - "
  BlogPostLink variant cat post ->
    show post <> "- " <> show cat <> " - " <> printBlogPostVariant variant <> " - "
  _ -> ""


asyncToDocumentTitle :: forall eff
                      . { getBlogPostCategoryQueues :: GetBlogPostCategorySparrowClientQueues (ref :: REF, console :: CONSOLE | eff)
                        , getBlogPostQueues :: GetBlogPostSparrowClientQueues (ref :: REF, console :: CONSOLE | eff)
                        }
                     -> SiteLinks
                     -> Aff (ref :: REF, console :: CONSOLE | eff) String
asyncToDocumentTitle {getBlogPostCategoryQueues,getBlogPostQueues} link = case link of
  BlogPostCategoryLink variant cat -> do
    mCat <- OneIO.callAsync getBlogPostCategoryQueues (JSONTuple variant cat)
    case mCat of
      Nothing -> do
        liftEff $ warn $ "Error - couldn't find blog post category permalink - " <> show cat
        pure $ show cat <> " - " <> printBlogPostVariant variant <> " - "
      Just (GetBlogPostCategory {name}) -> pure $ show name <> " - " <> show variant <> " - "
  BlogPostLink variant cat post -> do
    Tuple mCat mPost <- sequential $
      Tuple <$> parallel (OneIO.callAsync getBlogPostCategoryQueues (JSONTuple variant cat))
            <*> parallel (OneIO.callAsync getBlogPostQueues (JSONTuple variant (JSONTuple cat post)))
    case mCat of
      Nothing -> do
        liftEff $ warn $ "Error - couldn't find blog post category permalink - " <> show cat
        pure $ show post <> " - " <> show cat <> " - " <> printBlogPostVariant variant <> " - "
      Just (GetBlogPostCategory {name}) ->
        case mPost of
          Nothing -> do
            liftEff $ warn $ "Error - couldn't find blog post permalink - " <> show post
            pure $ show post <> " - " <> show cat <> " - " <> printBlogPostVariant variant <> " - "
          Just (GetBlogPost {headline}) ->
            pure $ headline <> " - " <> show name <> " - " <> show variant <> " - "
  _ -> pure (initToDocumentTitle link)



-- Policy: don't fail on bad query params / fragment unless you have to
instance fromLocationSiteLinks :: FromLocation SiteLinks where
  fromLocation (Location path mQuery mFrag) = do
    case runParser siteLinksPathParser (URIPath.printPath path) of
      Left e -> Left (show e)
      Right link -> pure link

-- TODO verify correctness
siteLinksPathParser :: Parser SiteLinks
siteLinksPathParser = do
  divider
  let blogPostCategory = do
        variant <- blogPostVariantParser
        void divider
        cat <- permalinkParser
        pure (BlogPostCategoryLink variant cat)
      blogPost = do
        variant <- blogPostVariantParser
        void divider
        cat <- permalinkParser
        void divider
        post <- permalinkParser
        pure (BlogPostLink variant cat post)
      def = defaultSiteLinksPathParser userDetailsLinksParser Nothing
      newBlogPostCategory = NewBlogPostCategoryLink <$>
        (string "newBlogPostCategory" *> blogPostVariantParser)
      newBlogPost = NewBlogPostLink <<< StoredBlogPostCategoryId <$>
        (string "newBlogPost" *> divider *> int)
  try newBlogPostCategory
    <|> try newBlogPost
    <|> try blogPostCategory
    <|> try blogPost
    <|> def
  where
    divider = void (char '/')
  -- TODO put nonstandard parsers here

