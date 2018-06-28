module Spec.Dialogs.BlogPost where

import Links (SiteLinks (RootLink))
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Global.Links.Internal (PolicyLinks (..))
import LocalCooking.Global.Links.Class (class LocalCookingSiteLinks)
import LocalCooking.Spec.Dialogs.Generic (genericDialog)

import Prelude
import Data.URI.URI (print) as URI
import Data.URI.Location (class ToLocation, toLocation)
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
import Data.String.Permalink (Permalink)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import React (ReactElement) as R
import React.DOM (iframe) as R
import React.DOM.Props as RP
import DOM (DOM)

import Queue.One.Aff as OneIO


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
                    -> { blogPostDialogQueue :: OneIO.IOQueues (Effects eff) Permalink (Maybe Unit)
                       } -- FIXME Just take GetBlogPost as input? Leave that up to caller
                    -> R.ReactElement
blogPostDialog
  params@{toURI}
  { blogPostDialogQueue
  } =
  genericDialog
  params
  { dialogQueue: blogPostDialogQueue
  , closeQueue: Nothing
  , buttons: \_ -> []
  , title: "Blog Post"
  , submitValue: "Acknowledge"
  , pends: false
  , content:
    { component: \_ ->
      [ R.iframe
        [ RP.src $ URI.print $ toURI $ toLocation $ RootLink Nothing -- $ Just 
        , RP.style {width: "100%", border: "1px solid black"}
        ] []
      ]
    , obtain: \_ -> pure (Just unit)
    , reset: pure unit
    }
  }


