module Spec.Dialogs.NewBlogPostCategory where

import Links (SiteLinks (NewBlogPostCategoryLink))
import User (UserDetails)
import LocalCooking.Main (ExtraProcessingParams)
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Semantics.Blog (NewBlogPostCategory (..))
import LocalCooking.Dependencies.Blog (BlogQueues)
import LocalCooking.Common.Blog (BlogPostVariant, BlogPostCategory (..), BlogPostPriority (..))
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Spec.Common.Form.BlogPostCategory as BlogPostCategory
import LocalCooking.Spec.Common.Form.BlogPostPriority as BlogPostPriority
import Components.Form.Permalink as Permalink
import Components.Dialog.Generic (genericDialog)

import Prelude
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
import Data.Argonaut.JSONTuple (JSONTuple (..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (warn, log)

import React (ReactElement) as R
import React.DOM (text) as R
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


-- TODO integrate
type NewBlogPostCategoryDialog eff =
  { dialogQueues :: OneIO.IOQueues eff BlogPostVariant (Maybe NewBlogPostCategory)
  , closeQueue   :: One.Queue (write :: WRITE) eff Unit
  , dialogSignal :: IxSignal eff (Maybe BlogPostVariant)
  }

mkNewBlogPostCategoryDialogQueues :: forall eff
                                 . Eff (ref :: REF | eff) (NewBlogPostCategoryDialog (ref :: REF | eff))
mkNewBlogPostCategoryDialogQueues = do
  dialogQueues <- OneIO.newIOQueues
  closeQueue <- writeOnly <$> One.newQueue
  dialogSignal <- IxSignal.make Nothing
  pure
    { dialogQueues
    , closeQueue
    , dialogSignal
    }


extraProcessingNewBlogPostCategory :: forall eff
                                    . NewBlogPostCategoryDialog (Effects eff)
                                   -> BlogQueues (Effects eff)
                                   -> SiteLinks
                                   -> ExtraProcessingParams SiteLinks UserDetails (Effects eff)
                                   -> Eff (Effects eff) Unit
extraProcessingNewBlogPostCategory
  {dialogQueues,closeQueue,dialogSignal}
  blogQueues
  siteLinks
  params
  = case siteLinks of
  NewBlogPostCategoryLink variant -> do
    let handleNewBlogPostCategoryDialog mNewBlogPost = case mNewBlogPost of
          Nothing -> pure unit -- closed dialog
          Just newBlogPost -> do
            let withAuthToken authToken = do
                  let newBlogPosted mPostCatId = do
                        unsafeCoerceEff $ case mPostCatId of
                          Nothing -> do
                            warn "Error: couldn't new blog post category?"
                          Just newPostCatId -> do
                            log $ "Blog category created! " <> show newPostCatId
                            -- FIXME do something with cat ID?
                        One.putQueue closeQueue unit
                  OneIO.callAsyncEff
                    blogQueues.newBlogPostCategoryQueues
                    newBlogPosted
                    (JSONTuple authToken newBlogPost)
            onAvailableIx withAuthToken "newBlogPost" params.authTokenSignal
    OneIO.callAsyncEff
      dialogQueues
      handleNewBlogPostCategoryDialog
      variant
  _ -> pure unit



newBlogPostCategoryDialog :: forall eff
                           . LocalCookingParams SiteLinks UserDetails (Effects eff)
                          -> NewBlogPostCategoryDialog (Effects eff)
                          -> { back :: Eff (Effects eff) Unit
                             } -- FIXME Just take GetBlogPost as input? Leave that up to caller
                          -> R.ReactElement
newBlogPostCategoryDialog
  params@{toURI}
  { dialogQueues
  , closeQueue
  , dialogSignal
  }
  { back
  } =
  genericDialog
  { dialogQueue: dialogQueues
  , closeQueue: Just closeQueue
  , dialogSignal: Just dialogSignal
  , extraOnClose: back
  , buttons: \_ -> []
  , title: \_ -> "New Blog Post Category"
  , submitValue: Just "Submit"
  , pends: false
  , content:
    { component: \_ ->
      [ BlogPostCategory.blogPostCategory
        { label: R.text "Category Name"
        , fullWidth: true
        , id: "blogPostCategory"
        , updatedQueue: blogPostCategoryUpdatedQueue
        , blogPostCategorySignal: blogPostCategorySignal
        , setQueue: blogPostCategorySetQueue
        }
      , Permalink.permalink
        { label: R.text "Permalink"
        , fullWidth: true
        , id: "permalink"
        , updatedQueue: permalinkUpdatedQueue
        , permalinkSignal: permalinkSignal
        , setQueue: permalinkSetQueue
        }
      , BlogPostPriority.blogPostPriority
        { label: R.text "Priority"
        , fullWidth: true
        , id: "blogPostCategoryPriority"
        , updatedQueue: blogPostPriorityUpdatedQueue
        , blogPostPrioritySignal: blogPostPrioritySignal
        , setQueue: blogPostPrioritySetQueue
        }
      -- TODO edit button for those who deserve it :|
      ]
    , obtain: \variant -> do
      mPermalink <- liftEff $ IxSignal.get permalinkSignal
      case mPermalink of
        Permalink.PermalinkPartial _ -> pure Nothing
        Permalink.PermalinkBad _ -> pure Nothing
        Permalink.PermalinkGood permalink -> do
          name <- liftEff $ IxSignal.get blogPostCategorySignal
          priority <- liftEff $ IxSignal.get blogPostPrioritySignal
          pure $ Just $ NewBlogPostCategory {name,permalink,priority,variant}
    , reset: do
        One.putQueue blogPostCategorySetQueue (BlogPostCategory "")
        One.putQueue permalinkSetQueue (Permalink.PermalinkPartial "")
        One.putQueue blogPostPrioritySetQueue (BlogPostPriority 0)
    }
  , windowSizeSignal: params.windowSizeSignal
  }
  where
    blogPostCategoryUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    blogPostCategorySignal = unsafePerformEff $ IxSignal.make $ BlogPostCategory ""
    blogPostCategorySetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    permalinkUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    permalinkSignal = unsafePerformEff $ IxSignal.make $ Permalink.PermalinkPartial ""
    permalinkSetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    blogPostPriorityUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    blogPostPrioritySignal = unsafePerformEff $ IxSignal.make $ BlogPostPriority 0
    blogPostPrioritySetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
