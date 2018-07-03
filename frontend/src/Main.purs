module Main where

import Colors (palette)
import User (UserDetails (..), PreUserDetails (..))
import Links (SiteLinks (RootLink, NewBlogPostLink))
import Spec.Topbar.Buttons (topbarButtons)
import Spec.Drawers.Buttons (drawersButtons)
import Spec.Content (content)
import Spec.Content.UserDetails (userDetails)
import Spec.Content.UserDetails.Buttons (userDetailsButtons)
import Spec.Snackbar (messages)
import LocalCooking.Types.ServerToClient (env)
import LocalCooking.Main (defaultMain)
import LocalCooking.Common.User.Role (UserRole (Editor))
import LocalCooking.Spec.Misc.Branding (mainBrand)
import LocalCooking.Dependencies.Blog (blogDependencies, newBlogQueues)
import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn (..))
import LocalCooking.Semantics.Blog (NewBlogPost)
import LocalCooking.Semantics.Common (User (..))
import LocalCooking.Global.Links.Internal (ImageLinks (Logo40Png))

import Prelude
import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Data.URI.Location (toLocation)
import Data.Argonaut.JSONUnit (JSONUnit (..))
import Data.Array as Array
import Control.Monad.Aff (sequential)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE, log, warn)
import Control.Execution.Immediate (SET_IMMEDIATE_SHIM)

import React.DOM (text) as R
import MaterialUI.InjectTapEvent (INJECT_TAP_EVENT)
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.SvgIcon (svgIcon)
import MaterialUI.SvgIcon as SvgIcon
import MaterialUI.Types (createStyles)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)
import WebSocket (WEBSOCKET)
import Network.HTTP.Affjax (AJAX)
import Browser.WebStorage (WEB_STORAGE)
import Crypto.Scrypt (SCRYPT)
import IxSignal.Extra (onAvailableIx)
import Queue.Types (readOnly, writeOnly)
import Queue.One as One
import Queue.One.Aff as OneIO



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

  ( newBlogPostQueues :: OneIO.IOQueues Effects Unit (Maybe NewBlogPost)
    ) <- OneIO.newIOQueues


  defaultMain
    { env
    , palette
    , deps: do
        blogDependencies blogQueues
    , extraProcessing: \link {siteLinks,authTokenSignal} -> case link of
        NewBlogPostLink ->
          -- submit new blog post
          let handleNewBlogPostDialog mNewBlogPost = case mNewBlogPost of
                Nothing -> siteLinks (RootLink Nothing)
                Just newBlogPost ->
                  let withAuthToken authToken =
                        let newBlogPosted mPostId = case mPostId of
                              Nothing -> do
                                warn "Error: couldn't post new blog post?"
                                siteLinks (RootLink Nothing)
                              Just newPostId -> do
                                log $ "Blog posted! " <> show newPostId
                                siteLinks (RootLink Nothing)
                        in  OneIO.callAsyncEff blogQueues.newBlogPostQueues
                              newBlogPosted
                              (AccessInitIn {token: authToken, subj: newBlogPost})
                  in  onAvailableIx withAuthToken "newBlogPost" authTokenSignal
          in  void $ setTimeout 1000 $
                OneIO.callAsyncEff newBlogPostQueues handleNewBlogPostDialog unit
        _ -> pure unit
    , extraRedirect: \link mDetails -> case link of
        NewBlogPostLink -> case mDetails of
          Nothing -> Just (RootLink Nothing)
          Just (UserDetails {user: User {roles}})
            | Array.elem Editor roles -> Nothing
            | otherwise -> Just (RootLink Nothing)
        _ -> Nothing
    , leftDrawer:
      { buttons: drawersButtons
      }
    , topbar:
      { imageSrc: toLocation Logo40Png
      , buttons: topbarButtons
      }
    , content: \params -> content params {blogQueues,newBlogPostQueues}
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
      { content: messages {siteErrorQueue: readOnly siteErrorQueue}
      }
    , extendedNetwork:
      [ Button.withStyles
        (\_ ->
          { root: createStyles
            { background: "#c62828"
            , color: "#fff"
            , textTransform: "none"
            , "&:hover":
              { background: "#ff5f52"
              }
            }
          }
        )
        \{classes} ->
          button
          { href: "https://localcooking.com/"
          , classes: Button.createClasses classes
          , variant: Button.raised
          }
          [ svgIcon
            { viewBox: "0 0 279 279"
            , color: SvgIcon.inherit
            }
            [ mainBrand
            ]
          , R.text " Customers"
          ]
      , R.text " "
      , Button.withStyles
        (\_ ->
          { root: createStyles
            { background: "#1565c0"
            , color: "#fff"
            , textTransform: "none"
            , "&:hover":
              { background: "#5e92f3"
              }
            }
          }
        )
        \{classes} ->
          button
          { href: "https://chef.localcooking.com/"
          , classes: Button.createClasses classes
          , variant: Button.raised
          }
          [ svgIcon
            { viewBox: "0 0 279 279"
            , color: SvgIcon.inherit
            }
            [ mainBrand
            ]
          , R.text " Chefs"
          ]
      , R.text " "
      , Button.withStyles
        (\_ ->
          { root: createStyles
            { background: "#1b5e20"
            , color: "#fff"
            , textTransform: "none"
            , "&:hover":
              { background: "#4c8c4a"
              }
            }
          }
        )
        \{classes} ->
          button
          { href: "https://farm.localcooking.com/"
          , classes: Button.createClasses classes
          , variant: Button.raised
          }
          [ svgIcon
            { viewBox: "0 0 279 279"
            , color: SvgIcon.inherit
            }
            [ mainBrand
            ]
          , R.text " Farms"
          ]
      , R.text " "
      , Button.withStyles
        (\_ ->
          { root: createStyles
            { background: "#7b1fa2"
            , color: "#fff"
            , textTransform: "none"
            , "&:hover":
              { background: "#ae52d4"
              }
            }
          }
        )
        \{classes} ->
          button
          { href: "https://restaurant.localcooking.com/"
          , classes: Button.createClasses classes
          , variant: Button.raised
          }
          [ svgIcon
            { viewBox: "0 0 279 279"
            , color: SvgIcon.inherit
            }
            [ mainBrand
            ]
          , R.text " Restaurants"
          ]
      , R.text " "
      , Button.withStyles
        (\_ ->
          { root: createStyles
            { background: "#546e7a"
            , color: "#fff"
            , textTransform: "none"
            , "&:hover":
              { background: "#819ca9"
              }
            }
          }
        )
        \{classes} ->
          button
          { href: "https://admin.localcooking.com/"
          , classes: Button.createClasses classes
          , variant: Button.raised
          }
          [ svgIcon
            { viewBox: "0 0 279 279"
            , color: SvgIcon.inherit
            }
            [ mainBrand
            ]
          , R.text " Admins"
          ]
      ]
    }
