module Spec.Dialogs.NewBlogPost where

import Links (SiteLinks (RootLink, NewBlogPostLink))
import User (UserDetails)
import LocalCooking.Main (ExtraProcessingParams)
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Global.Links.Internal (PolicyLinks (..))
import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks)
import LocalCooking.Semantics.Blog (NewBlogPost (..))
import LocalCooking.Database.Schema (StoredBlogPostCategoryId)
import LocalCooking.Dependencies.Blog (BlogQueues)
import LocalCooking.Common.Blog (BlogPostPriority (..))
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Spec.Dialogs.Generic (genericDialog)
import LocalCooking.Spec.Common.Form.Text as Text
import LocalCooking.Spec.Common.Form.Permalink as Permalink
import LocalCooking.Spec.Common.Form.Markdown as Markdown
import LocalCooking.Spec.Common.Form.BlogPostPriority as BlogPostPriority
import LocalCooking.Spec.Common.Form.Checkbox as Checkbox

import Prelude
import Data.URI.URI (print) as URI
import Data.URI.Location (class ToLocation, toLocation)
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
import Data.String.Permalink (Permalink)
import Data.String.Markdown (MarkdownText (..))
import Data.Array as Array
import Data.Generic (class Generic, gEq, gCompare)
import Data.Argonaut.JSONTuple (JSONTuple (..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (warn, log)

import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Grid (grid)
import MaterialUI.Grid as Grid

import React (ReactElement) as R
import React.DOM (text) as R
import React.DOM.Props as RP
import DOM (DOM)

import Queue.Types (readOnly, writeOnly, WRITE)
import Queue.One.Aff as OneIO
import Queue.One as One
import IxQueue as IxQueue
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import IxSignal.Extra (onAvailableIx)


type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , dom       :: DOM
  | eff)


type NewBlogPostDialog eff =
  { dialogQueues :: OneIO.IOQueues eff StoredBlogPostCategoryId (Maybe NewBlogPost)
  , closeQueue   :: One.Queue (write :: WRITE) eff Unit
  , dialogSignal :: IxSignal eff (Maybe StoredBlogPostCategoryId)
  }

mkNewBlogPostDialogQueues :: forall eff
                           . Eff (ref :: REF | eff) (NewBlogPostDialog (ref :: REF | eff))
mkNewBlogPostDialogQueues = do
  dialogQueues <- OneIO.newIOQueues
  closeQueue <- writeOnly <$> One.newQueue
  dialogSignal <- IxSignal.make Nothing
  pure
    { dialogQueues
    , closeQueue
    , dialogSignal
    }


extraProcessingNewBlogPost :: forall eff
                            . NewBlogPostDialog (Effects eff)
                           -> BlogQueues (Effects eff)
                           -> { authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
                              }
                           -> SiteLinks
                           -> ExtraProcessingParams SiteLinks UserDetails (Effects eff)
                           -> Eff (Effects eff) Unit
extraProcessingNewBlogPost
  {dialogQueues,closeQueue,dialogSignal}
  blogQueues
  {authTokenSignal}
  siteLinks
  params
  = case siteLinks of
  NewBlogPostLink cat -> do
    let handleNewBlogPostDialog mNewBlogPost = case mNewBlogPost of
          Nothing -> pure unit -- closed dialog
          Just newBlogPost -> do
            let withAuthToken authToken = do
                  let newBlogPosted mPostCatId = do
                        unsafeCoerceEff $ case mPostCatId of
                          Nothing -> do
                            warn "Error: couldn't new blog post ?"
                          Just newPostCatId -> do
                            log $ "Blog  created! " <> show newPostCatId
                            -- FIXME do something with cat ID?
                        One.putQueue closeQueue unit
                  OneIO.callAsyncEff
                    blogQueues.newBlogPostQueues
                    newBlogPosted
                    (JSONTuple authToken newBlogPost)
            onAvailableIx withAuthToken "newBlogPost" authTokenSignal
    OneIO.callAsyncEff
      dialogQueues
      handleNewBlogPostDialog
      cat
  _ -> pure unit


-- | Only used for checkboxes
data BlogPostPrimary = BlogPostPrimary

derive instance genericBlogPostPrimary :: Generic BlogPostPrimary

instance eqBlogPostPrimary :: Eq BlogPostPrimary where
  eq = gEq

instance ordBlogPostPrimary :: Ord BlogPostPrimary where
  compare = gCompare

instance showBlogPostPrimary :: Show BlogPostPrimary where
  show BlogPostPrimary = "Primary"



newBlogPostDialog :: forall eff
                   . LocalCookingParams SiteLinks UserDetails (Effects eff)
                  -> NewBlogPostDialog (Effects eff)
                  -> { back :: Eff (Effects eff) Unit
                     } -- FIXME Just take GetBlogPost as input? Leave that up to caller
                  -> R.ReactElement
newBlogPostDialog
  params@{toURI}
  { dialogQueues
  , closeQueue
  , dialogSignal
  }
  { back
  } =
  genericDialog
  params
  { dialogQueue: dialogQueues
  , closeQueue: Just closeQueue
  , dialogSignal: Just dialogSignal
  , extraOnClose: back
  , buttons: \_ -> []
  , title: \_ -> "New Blog Post"
  , submitValue: Just "Submit"
  , pends: false
  , content:
    { component: \_ ->
      [ Text.text
        { label: R.text "Headline"
        , fullWidth: true
        , id: "headline"
        , updatedQueue: headlineUpdatedQueue
        , textSignal: headlineSignal
        , setQueue: headlineSetQueue
        }
      , Permalink.permalink
        { label: R.text "Permalink"
        , fullWidth: true
        , id: "permalink"
        , updatedQueue: permalinkUpdatedQueue
        , permalinkSignal: permalinkSignal
        , setQueue: permalinkSetQueue
        }
      , Markdown.markdown
        { label: R.text "Content"
        , fullWidth: true
        , id: "content"
        , updatedQueue: contentUpdatedQueue
        , markdownSignal: contentSignal
        , setQueue: contentSetQueue
        }
      , BlogPostPriority.blogPostPriority
        { label: R.text "Priority"
        , fullWidth: true
        , id: "blogPostPriority"
        , updatedQueue: blogPostPriorityUpdatedQueue
        , blogPostPrioritySignal: blogPostPrioritySignal
        , setQueue: blogPostPrioritySetQueue
        }
      , Checkbox.checkboxes
        { entriesSignal: blogPostPrimarySignal
        , label: "Primary"
        , entries: [BlogPostPrimary]
        }
      -- TODO edit button for those who deserve it :|
      ]
    , obtain: \category -> do
      mPermalink <- liftEff $ IxSignal.get permalinkSignal
      case mPermalink of
        Permalink.PermalinkPartial _ -> pure Nothing
        Permalink.PermalinkBad _ -> pure Nothing
        Permalink.PermalinkGood permalink -> do
          headline <- liftEff $ IxSignal.get headlineSignal
          content <- liftEff $ IxSignal.get contentSignal
          priority <- liftEff $ IxSignal.get blogPostPrioritySignal
          primary <- liftEff $ (not <<< Array.null) <$> IxSignal.get blogPostPrimarySignal
          pure $ Just $ NewBlogPost {headline,permalink,content,priority,primary,category}
    , reset: do
        One.putQueue headlineSetQueue ""
        One.putQueue permalinkSetQueue (Permalink.PermalinkPartial "")
        One.putQueue contentSetQueue (MarkdownText "")
        One.putQueue blogPostPrioritySetQueue (BlogPostPriority 0)
    }
  }
  where
    headlineUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    headlineSignal = unsafePerformEff $ IxSignal.make ""
    headlineSetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    permalinkUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    permalinkSignal = unsafePerformEff $ IxSignal.make $ Permalink.PermalinkPartial ""
    permalinkSetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    contentUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    contentSignal = unsafePerformEff $ IxSignal.make $ MarkdownText ""
    contentSetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    blogPostPriorityUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    blogPostPrioritySignal = unsafePerformEff $ IxSignal.make $ BlogPostPriority 0
    blogPostPrioritySetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    blogPostPrimarySignal = unsafePerformEff $ IxSignal.make []
