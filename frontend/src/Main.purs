module Main where

import Colors (palette)
import User (UserDetails (..), PreUserDetails (..))
import Links (SiteLinks (RootLink, NewBlogPostLink, NewBlogPostCategoryLink), initToDocumentTitle, asyncToDocumentTitle)
import Error (SiteError (RedirectError), RedirectError (RedirectNewBlogPostNoEditor))
import Spec.Topbar.Buttons (topbarButtons)
import Spec.Drawers.Buttons (drawersButtons)
import Spec.Dialogs.NewBlogPost
  (mkNewBlogPostDialogQueues, extraProcessingNewBlogPost)
import Spec.Dialogs.NewBlogPostCategory
  (mkNewBlogPostCategoryDialogQueues, extraProcessingNewBlogPostCategory)
import Spec.Content (content)
import Spec.Content.UserDetails (userDetails)
import Spec.Content.UserDetails.Buttons (userDetailsButtons)
import Spec.Snackbar (messages)
import LocalCooking.Types.ServerToClient (env)
import LocalCooking.Main (defaultMain)
import LocalCooking.Common.User.Role (UserRole (Editor))
import LocalCooking.Spec.Misc.Network (networkButton)
import LocalCooking.Dependencies.Blog (blogDependencies, newBlogQueues)
import LocalCooking.Semantics.Common (User (..))
import LocalCooking.Global.Links.Internal (ImageLinks (Logo40Png))

import Prelude
import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Data.URI.Location (toLocation)
import Data.Array as Array
import Control.Monad.Aff (sequential)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Execution.Immediate (SET_IMMEDIATE_SHIM)

import React.DOM (text) as R
import MaterialUI.InjectTapEvent (INJECT_TAP_EVENT)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (history)
import DOM.HTML.Types (HISTORY)
import DOM.HTML.History (back)
import WebSocket (WEBSOCKET)
import Network.HTTP.Affjax (AJAX)
import Browser.WebStorage (WEB_STORAGE)
import Crypto.Scrypt (SCRYPT)
import IxSignal.Internal as IxSignal
import Queue.One as One



-- | All top-level effects
type Effects =
  ( console            :: CONSOLE
  , injectTapEvent     :: INJECT_TAP_EVENT
  , set_immediate_shim :: SET_IMMEDIATE_SHIM
  , ref                :: REF
  , dom                :: DOM
  , timer              :: TIMER
  , uuid               :: GENUUID
  , exception          :: EXCEPTION
  , history            :: HISTORY
  , now                :: NOW
  , ws                 :: WEBSOCKET
  , ajax               :: AJAX
  , webStorage         :: WEB_STORAGE
  , scrypt             :: SCRYPT
  )


main :: Eff Effects Unit
main = do
  log "Starting Local Cooking Blog frontend..."

  blogQueues <- newBlogQueues
  siteErrorQueue <- One.newQueue

  newBlogPostDialogQueues <- mkNewBlogPostDialogQueues
  newBlogPostCategoryDialogQueues <- mkNewBlogPostCategoryDialogQueues


  defaultMain
    { env
    , palette
    , deps: do
        blogDependencies blogQueues
    -- FIXME align with all redirects...?
    , extraProcessing: \link exParams@{back,authTokenSignal} -> do
        extraProcessingNewBlogPost
          newBlogPostDialogQueues
          blogQueues
          {authTokenSignal}
          link
          exParams
        extraProcessingNewBlogPostCategory
          newBlogPostCategoryDialogQueues
          blogQueues
          {authTokenSignal}
          link
          exParams
        case link of
          NewBlogPostLink _ -> pure unit
          NewBlogPostCategoryLink _ -> pure unit
          _ -> do -- call close, but also set a ref that declares if a `back` should be issued...?
                  -- FIXME race condition?
            log "Closing dialog.."
            IxSignal.setDiff Nothing newBlogPostDialogQueues.dialogSignal
            IxSignal.setDiff Nothing newBlogPostCategoryDialogQueues.dialogSignal
    -- FIXME should also include / issue siteError
    , extraRedirect: \link mDetails -> case link of
        NewBlogPostLink _ -> case mDetails of
          Nothing -> Just { siteLink: RootLink, siteError: RedirectError RedirectNewBlogPostNoEditor }
          Just (UserDetails {user: User {roles}})
            | Array.elem Editor roles -> Nothing
            | otherwise -> Just { siteLink: RootLink, siteError: RedirectError RedirectNewBlogPostNoEditor }
        _ -> Nothing
    , initToDocumentTitle 
    , asyncToDocumentTitle: asyncToDocumentTitle
        { getBlogPostQueues: blogQueues.getBlogPostQueues
        , getBlogPostCategoryQueues: blogQueues.getBlogPostCategoryQueues
        }
    , leftDrawer:
      { buttons: drawersButtons
      }
    , topbar:
      { imageSrc: toLocation Logo40Png
      , buttons: topbarButtons
      }
    , content: \params -> content params
      { blogQueues
      , newBlogPost: newBlogPostDialogQueues
      , newBlogPostCategory: newBlogPostCategoryDialogQueues
      , back: back =<< history =<< window
      }
    , userDetails:
      { buttons: userDetailsButtons
      , content: userDetails
      , obtain: \{user} -> do
        PreUserDetails mUser <- sequential $ PreUserDetails <$> user
        case mUser of
          Just user -> pure $ Just $ UserDetails {user}
          _ -> pure Nothing
      }
    , error:
      { content: messages
      , queue: siteErrorQueue
      }
    , extendedNetwork:
      [ networkButton
        { light: "#c62828"
        , dark: "#ff5f52"
        , href: "https://localcooking.com/"
        , label: "Customers"
        }
      , R.text " "
      , networkButton
        { light: "#1565c0"
        , dark: "#5e92f3"
        , href: "https://chef.localcooking.com/"
        , label: "Chefs"
        }
      , R.text " "
      , networkButton
        { light: "#1b5e20"
        , dark: "#4c8c4a"
        , href: "https://farm.localcooking.com/"
        , label: "Farms"
        }
      , R.text " "
      , networkButton
        { light: "#7b1fa2"
        , dark: "#ae52d4"
        , href: "https://restaurant.localcooking.com/"
        , label: "Restaurants"
        }
      , R.text " "
      , networkButton
        { light: "#546e7a"
        , dark: "#819ca9"
        , href: "https://admin.localcooking.com/"
        , label: "Admins"
        }
      ]
    }
