module Spec.Dialogs.NewBlogPost where

import Links (SiteLinks (RootLink))
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Global.Links.Internal (PolicyLinks (..))
import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks)
import LocalCooking.Spec.Dialogs.Generic (genericDialog)
import LocalCooking.Semantics.Blog (NewBlogPost (..))
import LocalCooking.Spec.Common.Form.Text as Text
import LocalCooking.Spec.Common.Form.Permalink as Permalink

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

import Queue.Types (readOnly, writeOnly)
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



blogPostDialog :: forall eff siteLinks userDetails userDetailsLinks
                     . LocalCookingSiteLinks siteLinks userDetailsLinks
                    => ToLocation siteLinks
                    => LocalCookingParams siteLinks userDetails (Effects eff)
                    -> { newBlogPostQueues :: OneIO.IOQueues (Effects eff) Unit (Maybe NewBlogPost)
                       } -- FIXME Just take GetBlogPost as input? Leave that up to caller
                    -> R.ReactElement
blogPostDialog
  params@{toURI}
  { newBlogPostQueues
  } =
  genericDialog
  params
  { dialogQueue: newBlogPostQueues
  , closeQueue: Nothing
  , buttons: \_ -> []
  , title: \_ -> "New Blog Post"
  , submitValue: Nothing
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
      -- , -- FIXME show markdown with markdown thingy
      -- TODO edit button for those who deserve it :|
      ]
    , obtain: \_ -> do
      headline <- liftEff $ IxSignal.get headlineSignal
      mPermalink <- liftEff $ IxSignal.get permalinkSignal
      case mPermalink of
        Permalink.PermalinkPartial _ -> pure Nothing
        Permalink.PermalinkBad _ -> pure Nothing
        Permalink.PermalinkGood permalink -> do
          let content = MarkdownText ""
          pure $ Just $ NewBlogPost {headline,permalink,content}
    , reset: pure unit
    }
  }
  where
    headlineUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    headlineSignal = unsafePerformEff $ IxSignal.make ""
    headlineSetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    permalinkUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    permalinkSignal = unsafePerformEff $ IxSignal.make $ Permalink.PermalinkPartial ""
    permalinkSetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
