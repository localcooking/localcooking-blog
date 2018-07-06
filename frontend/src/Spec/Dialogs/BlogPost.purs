module Spec.Dialogs.BlogPost where

import Links (SiteLinks (RootLink))
import User (UserDetails)
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Global.Links.Internal (PolicyLinks (..))
import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks)
import LocalCooking.Spec.Dialogs.Generic (genericDialog)
import LocalCooking.Semantics.Blog (GetBlogPost (..))

import Prelude
import Data.URI.URI (print) as URI
import Data.URI.Location (class ToLocation, toLocation)
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
import Data.String.Permalink (Permalink)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Grid (grid)
import MaterialUI.Grid as Grid

import React (ReactElement) as R
import React.DOM (text) as R
import React.DOM.Props as RP
import React.Markdown as ReactMarkdown
import DOM (DOM)

import Queue.One.Aff as OneIO


type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , dom       :: DOM
  | eff)



blogPostDialog :: forall eff
                . LocalCookingParams SiteLinks UserDetails (Effects eff)
              -> { openBlogPostQueues :: OneIO.IOQueues (Effects eff) GetBlogPost (Maybe Unit)
                  } -- FIXME Just take GetBlogPost as input? Leave that up to caller
              -> R.ReactElement
blogPostDialog
  params@{toURI}
  { openBlogPostQueues
  } =
  genericDialog
  params
  { dialogQueue: openBlogPostQueues
  , closeQueue: Nothing
  , dialogSignal: Nothing
  , buttons: \_ -> []
  , title: \(GetBlogPost {headline}) -> headline
  , submitValue: Nothing
  , pends: false
  , content:
    { component: \{input: GetBlogPost {author,timestamp,permalink,content}} ->
      [ grid {spacing: Grid.spacing8, container: true}
        [ grid {xs: 4, item: true}
          [typography {variant: Typography.body1} [R.text $ show author]]
        , grid {xs: 4, item: true}
          [typography {variant: Typography.body1} [R.text $ show permalink]]
        , grid {xs: 4, item: true}
          [typography {variant: Typography.body1} [R.text $ show timestamp]]
        ]
      , ReactMarkdown.markdown {source: content, renderers: {}}
      -- , -- FIXME show markdown with markdown thingy
      -- TODO edit button for those who deserve it :|
      ]
    , obtain: \_ -> pure (Just unit)
    , reset: pure unit
    }
  }
