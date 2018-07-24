module Spec.Dialogs.NewBlogPostCategory where

import Links (SiteLinks (RootLink))
import User (UserDetails)
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Global.Links.Internal (PolicyLinks (..))
import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks)
import LocalCooking.Semantics.Blog (NewBlogPostCategory (..))
import LocalCooking.Common.Blog (BlogPostVariant, BlogPostCategory (..), BlogPostPriority (..))
import LocalCooking.Spec.Dialogs.Generic (genericDialog)
import LocalCooking.Spec.Common.Form.BlogPostCategory as BlogPostCategory
import LocalCooking.Spec.Common.Form.BlogPostPriority as BlogPostPriority
import LocalCooking.Spec.Common.Form.Permalink as Permalink

import Prelude
import Data.URI.URI (print) as URI
import Data.URI.Location (class ToLocation, toLocation)
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
import Data.String.Permalink (Permalink)
import Data.String.Markdown (MarkdownText (..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Class (liftEff)

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


type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , dom       :: DOM
  | eff)



newBlogPostCategoryDialog :: forall eff
                           . LocalCookingParams SiteLinks UserDetails (Effects eff)
                          -> { dialogQueues :: OneIO.IOQueues (Effects eff) BlogPostVariant (Maybe NewBlogPostCategory)
                             , closeQueue   :: One.Queue (write :: WRITE) (Effects eff) Unit
                             , dialogSignal :: IxSignal (Effects eff) (Maybe BlogPostVariant)
                             , back         :: Eff (Effects eff) Unit
                             } -- FIXME Just take GetBlogPost as input? Leave that up to caller
                          -> R.ReactElement
newBlogPostCategoryDialog
  params@{toURI}
  { dialogQueues
  , closeQueue
  , dialogSignal
  , back
  } =
  genericDialog
  params
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
        { label: R.text "Blog Post Category"
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
        { label: R.text "Blog Post Priority"
        , fullWidth: true
        , id: "blogPostPriority"
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
