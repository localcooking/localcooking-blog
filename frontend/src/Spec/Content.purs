module Spec.Content where

import Spec.Content.Root (root)
import Spec.Dialogs.BlogPost (blogPostDialog)
import Spec.Dialogs.NewBlogPost (newBlogPostDialog)
import Links (SiteLinks (..))
import User (UserDetails)
import LocalCooking.Thermite.Params (LocalCookingParams, LocalCookingState, LocalCookingAction, performActionLocalCooking, whileMountedLocalCooking, initLocalCookingState, showLocalCookingState)
import LocalCooking.Dependencies.Blog (BlogQueues)
import LocalCooking.Semantics.Blog (GetBlogPost, NewBlogPost)

import Prelude
import Data.Maybe (Maybe)
import Data.UUID (GENUUID)
import Data.URI (URI)
import Data.URI.Location (Location)
import Data.Lens (Lens', Prism', lens, prism')
import Data.String.Permalink (Permalink)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
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
import Queue.Types (WRITE)
import Queue.One as One
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
        , newBlogPostQueues :: OneIO.IOQueues (Effects eff) Unit (Maybe NewBlogPost)
        , closeNewBlogPostQueue :: One.Queue (write :: WRITE) (Effects eff) Unit
        }
     -> T.Spec (Effects eff) State Unit Action
spec
  params
  { blogQueues
  , newBlogPostQueues
  , closeNewBlogPostQueue
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      LocalCookingAction a -> performActionLocalCooking getLCState a props state

    render :: T.Render State Unit Action
    render dispatch props state children =
      if isBlogPostContent state.localCooking.currentPage 
        then blogPostsContent
        else []
      where
        isBlogPostContent link =
          let _ = unsafePerformEff $ log $ "calculating rendering: " <> showLocalCookingState state.localCooking
          in  case link of
                RootLink _ -> true
                NewBlogPostLink -> true
                _ -> false
        blogPostsContent =
          [ root params {blogQueues,openBlogPostQueues,newBlogPostQueues}
          , blogPostDialog params {openBlogPostQueues}
          , newBlogPostDialog params {newBlogPostQueues,closeNewBlogPostQueue}
          ]

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
           , newBlogPostQueues :: OneIO.IOQueues (Effects eff) Unit (Maybe NewBlogPost)
           , closeNewBlogPostQueue :: One.Queue (write :: WRITE) (Effects eff) Unit
           }
        -> R.ReactElement
content
  params args =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec params args
          ) (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
        whileMountedLocalCooking
          params
          "Spec.Content"
          LocalCookingAction
          (\this -> unsafeCoerceEff <<< dispatcher this)
          reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
