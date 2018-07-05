module Spec.Dialogs.NewBlogPost where

import Links (SiteLinks (RootLink))
import User (UserDetails)
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Global.Links.Internal (PolicyLinks (..))
import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks)
import LocalCooking.Spec.Dialogs.Generic (genericDialog)
import LocalCooking.Semantics.Blog (NewBlogPost (..))
import LocalCooking.Spec.Common.Form.Text as Text
import LocalCooking.Spec.Common.Form.Permalink as Permalink
import LocalCooking.Spec.Common.Form.Markdown as Markdown

import Prelude
import Data.URI.URI (print) as URI
import Data.URI.Location (class ToLocation, toLocation)
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
import Data.String.Permalink (Permalink)
import Data.String.Markdown (MarkdownText (..))
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
import IxSignal.Internal as IxSignal


type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , dom       :: DOM
  | eff)



newBlogPostDialog :: forall eff
                   . LocalCookingParams SiteLinks UserDetails (Effects eff)
                  -> { newBlogPostQueues :: OneIO.IOQueues (Effects eff) Unit (Maybe NewBlogPost)
                     , closeNewBlogPostQueue :: One.Queue (write :: WRITE) (Effects eff) Unit
                     } -- FIXME Just take GetBlogPost as input? Leave that up to caller
                  -> R.ReactElement
newBlogPostDialog
  params@{toURI}
  { newBlogPostQueues
  , closeNewBlogPostQueue
  } =
  genericDialog
  params
  { dialogQueue: newBlogPostQueues
  , closeQueue: Just closeNewBlogPostQueue
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
      -- TODO edit button for those who deserve it :|
      ]
    , obtain: \_ -> do
      mPermalink <- liftEff $ IxSignal.get permalinkSignal
      case mPermalink of
        Permalink.PermalinkPartial _ -> pure Nothing
        Permalink.PermalinkBad _ -> pure Nothing
        Permalink.PermalinkGood permalink -> do
          headline <- liftEff $ IxSignal.get headlineSignal
          content <- liftEff $ IxSignal.get contentSignal
          pure $ Just $ NewBlogPost {headline,permalink,content}
    , reset: do
        One.putQueue headlineSetQueue ""
        One.putQueue permalinkSetQueue (Permalink.PermalinkPartial "")
        One.putQueue contentSetQueue (MarkdownText "")
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
