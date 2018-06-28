module Spec.Content where

import Spec.Content.Root (root)
import Spec.Dialogs.BlogPost (blogPostDialog)
import Links (SiteLinks (..))
import User (UserDetails)
import LocalCooking.Thermite.Params (LocalCookingParams, LocalCookingState, LocalCookingAction, performActionLocalCooking, whileMountedLocalCooking, initLocalCookingState)
import LocalCooking.Dependencies.Blog (BlogQueues)
import LocalCooking.Semantics.Blog (GetBlogPost)

import Prelude
import Data.Maybe (Maybe)
import Data.UUID (GENUUID)
import Data.URI (URI)
import Data.URI.Location (Location)
import Data.Lens (Lens', Prism', lens, prism')
import Data.String.Permalink (Permalink)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (text) as R
import React.Signal.WhileMounted as Signal
import DOM (DOM)
import DOM.HTML.Window.Extra (WindowSize)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue.One.Aff as OneIO



type State =
  { localCooking :: LocalCookingState SiteLinks UserDetails
  }

initialState :: LocalCookingState SiteLinks UserDetails -> State
initialState localCooking =
  { localCooking
  }

data Action
  = LocalCookingAction (LocalCookingAction SiteLinks UserDetails)


type Effects eff =
  ( ref       :: REF
  , exception :: EXCEPTION
  , uuid      :: GENUUID
  , console   :: CONSOLE
  , dom       :: DOM
  | eff)

getLCState :: Lens' State (LocalCookingState SiteLinks UserDetails)
getLCState = lens (_.localCooking) (_ { localCooking = _ })


spec :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> { blogQueues :: BlogQueues (Effects eff)
        }
     -> T.Spec (Effects eff) State Unit Action
spec
  params {blogQueues} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      LocalCookingAction a -> performActionLocalCooking getLCState a props state

    render :: T.Render State Unit Action
    render dispatch props state children = case state.localCooking.currentPage of
      RootLink _ ->
        [ root params {blogQueues,openBlogPostQueues}
        , blogPostDialog params {openBlogPostQueues}
        ]
      _ -> []

    openBlogPostQueues :: OneIO.IOQueues (Effects eff) GetBlogPost (Maybe Unit)
    openBlogPostQueues = unsafePerformEff OneIO.newIOQueues
    -- FIXME raise this queue up to the top level - route parsing needs to
    -- call this, with the permalink parsed.
    -- I wonder... can I encode them into an `extraSiteLinksInvocations`? Such that
    -- `siteLinks` drives their openings? Would the dialog's `close` need to
    -- drive a "return/back" (or explicitly chosen one)?



content :: forall eff
         . LocalCookingParams SiteLinks UserDetails (Effects eff)
        -> { blogQueues :: BlogQueues (Effects eff)
           }
        -> R.ReactElement
content
  params {blogQueues} =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec params {blogQueues}
          ) (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
        whileMountedLocalCooking
          params
          "Spec.Content"
          LocalCookingAction
          (\this -> unsafeCoerceEff <<< dispatcher this)
          reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
