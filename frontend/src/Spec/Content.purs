module Spec.Content where

import Spec.Content.Root (root)
import Spec.Dialogs.BlogPost (blogPostDialog)
import Spec.Dialogs.NewBlogPost
  (newBlogPostDialog, NewBlogPostDialog)
import Spec.Dialogs.NewBlogPostCategory
  (newBlogPostCategoryDialog, NewBlogPostCategoryDialog)
import Links (SiteLinks (..))
import User (UserDetails)
import LocalCooking.Thermite.Params (LocalCookingParams, LocalCookingStateLight, LocalCookingActionLight, performActionLocalCookingLight, whileMountedLocalCookingLight, initLocalCookingStateLight, showLocalCookingStateLight)
import LocalCooking.Dependencies.Blog (BlogQueues)
import LocalCooking.Semantics.Blog (GetBlogPost)

import Prelude
import Data.Maybe (Maybe)
import Data.UUID (GENUUID)
import Data.Lens (Lens', lens)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import DOM (DOM)
import Queue.One.Aff as OneIO



type State =
  { localCooking :: LocalCookingStateLight SiteLinks
  }

initialState :: LocalCookingStateLight SiteLinks -> State
initialState localCooking =
  { localCooking
  }

data Action
  = LocalCookingAction (LocalCookingActionLight SiteLinks)


type Effects eff =
  ( ref       :: REF
  , exception :: EXCEPTION
  , uuid      :: GENUUID
  , console   :: CONSOLE
  , dom       :: DOM
  | eff)

getLCState :: Lens' State (LocalCookingStateLight SiteLinks)
getLCState = lens (_.localCooking) (_ { localCooking = _ })


spec :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> { blogQueues :: BlogQueues (Effects eff)
        , newBlogPost :: NewBlogPostDialog (Effects eff)
        , newBlogPostCategory :: NewBlogPostCategoryDialog (Effects eff)
        , back :: Eff (Effects eff) Unit
        }
     -> T.Spec (Effects eff) State Unit Action
spec
  params
  { blogQueues
  , newBlogPost
  , newBlogPostCategory
  , back
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      LocalCookingAction a -> performActionLocalCookingLight getLCState a props state

    render :: T.Render State Unit Action
    render dispatch props state children =
      if isBlogPostContent state.localCooking.currentPage 
        then blogPostsContent
        else []
      where
        isBlogPostContent link =
          let _ = unsafePerformEff $ log $ "calculating rendering: " <> showLocalCookingStateLight state.localCooking
          in  case link of
                RootLink -> true
                NewBlogPostLink _ -> true
                _ -> false
        blogPostsContent =
          [ root params
            { blogQueues
            , openBlogPostQueues
            , newBlogPost
            , newBlogPostCategory
            }
          , blogPostDialog params {openBlogPostQueues}
          , newBlogPostDialog params newBlogPost {back}
          , newBlogPostCategoryDialog params newBlogPostCategory {back}
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
           , newBlogPost :: NewBlogPostDialog (Effects eff)
           , newBlogPostCategory :: NewBlogPostCategoryDialog (Effects eff)
           , back :: Eff (Effects eff) Unit
           }
        -> R.ReactElement
content
  params args =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec params args
          ) (initialState (unsafePerformEff (initLocalCookingStateLight params)))
      reactSpec' =
        whileMountedLocalCookingLight
          params
          "Spec.Content"
          LocalCookingAction
          (\this -> unsafeCoerceEff <<< dispatcher this)
          reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
