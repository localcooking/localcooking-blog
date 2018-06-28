module Spec.Content.Root where

import Links (SiteLinks, AboutPageLinks (..))
import User (UserDetails)
import LocalCooking.Thermite.Params (LocalCookingParams, LocalCookingState, LocalCookingAction, performActionLocalCooking, whileMountedLocalCooking, initLocalCookingState)
import LocalCooking.Database.Schema (StoredBlogPostId)
import LocalCooking.Semantics.Blog (BlogPostSynopsis (..))
import LocalCooking.Semantics.Common (WithId (..))
import LocalCooking.Dependencies.Blog (BlogQueues)

import Prelude
import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Data.URI (URI)
import Data.URI.URI as URI
import Data.URI.Location (Location, toLocation)
import Data.Lens (Lens', Prism', lens, prism')
import Data.Array as Array
import Data.Argonaut.JSONUnit (JSONUnit (..))
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (warn)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (div, em, img, strong, text) as R
import React.DOM.Props as RP
import React.Signal.WhileMounted as Signal
import React.Queue.WhileMounted as Queue
import DOM.HTML.Window.Extra (WindowSize (Laptop))

import MaterialUI.Types (createStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Divider (divider)
import MaterialUI.Grid (grid)
import MaterialUI.Grid as Grid
import MaterialUI.List (list)
import MaterialUI.ListItem (listItem)
import MaterialUI.ListItemText (listItemText)
import MaterialUI.ListItemIcon (listItemIcon)
import MaterialUI.CircularProgress (circularProgress)
import MaterialUI.CircularProgress as CircularProgress
import MaterialUI.Table (table, tableBody, tableCell, tableHead, tableRow)
import MaterialUI.Icons.Search (searchIcon)
import MaterialUI.Icons.PictureInPicture (pictureInPictureIcon)
import MaterialUI.Icons.ShoppingCart (shoppingCartIcon)
import MaterialUI.Icons.Timelapse (timelapseIcon)
import MaterialUI.Icons.LocalShipping (localShippingIcon)
import MaterialUI.Icons.RestaurantMenu (restaurantMenuIcon)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue.One as One
import Queue.One.Aff as OneIO
import Queue.Types (allowReading, allowWriting)



type State =
  { localCooking :: LocalCookingState SiteLinks UserDetails
  , blogPosts :: Maybe (Array (WithId StoredBlogPostId BlogPostSynopsis))
  }

initialState :: LocalCookingState SiteLinks UserDetails -> State
initialState localCooking =
  { localCooking
  , blogPosts: Nothing
  }

data Action
  = LocalCookingAction (LocalCookingAction SiteLinks UserDetails)
  | GotBlogPosts (Array (WithId StoredBlogPostId BlogPostSynopsis))

type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  | eff)

getLCState :: Lens' State (LocalCookingState SiteLinks UserDetails)
getLCState = lens (_.localCooking) (_ { localCooking = _ })


spec :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> T.Spec (Effects eff) State Unit Action
spec params = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      LocalCookingAction a -> performActionLocalCooking getLCState a props state
      GotBlogPosts xs -> void $ T.cotransform _ {blogPosts = Just xs}

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ typography
        { variant: if state.localCooking.windowSize < Laptop
                      then Typography.headline
                      else Typography.display1
        , align: Typography.right
        , color: Typography.primary
        , style: createStyles {marginBottom: "1em"}
        } [R.text "Blog Posts"]
      , divider {}
      , case state.blogPosts of
          Nothing -> circularProgress {mode: CircularProgress.indeterminate}
          Just xs
            | Array.null xs -> R.em [] [R.text "No blog posts"]
            | otherwise -> table {}
            [ tableHead {}
              [ tableRow {}
                [ tableCell {} $ R.text "Author"
                , tableCell {} $ R.text "Date"
                , tableCell {} $ R.text "Headline"
                ]
              ]
            , tableBody {} $
              let renderPost (WithId {content: BlogPostSynopsis {author,timestamp,headline,permalink}}) =
                    tableRow
                      { hover: true
                      -- , onClick: mkEffFn1 \_ -> dispatch (ClickedUser u)
                      }
                      [ tableCell {} $ R.text $ show author
                      , tableCell {} $ R.text $ show timestamp
                      , tableCell {} $ R.text headline
                      ]
              in  renderPost <$> xs
            ]
      ]


root :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> { blogQueues :: BlogQueues (Effects eff)
        }
     -> R.ReactElement
root params {blogQueues} =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec params
          ) (initialState (unsafePerformEff (initLocalCookingState params)))
      OneIO.IOQueues{input: getBlogPostsInput, output: getBlogPostsOutput} =
        blogQueues.getBlogPostsQueues
      reactSpec' =
          Queue.whileMountedOne
            (allowReading getBlogPostsOutput)
            (\this mXs -> case mXs of
                Nothing -> unsafeCoerceEff $ warn "getBlogPosts error"
                Just xs -> unsafeCoerceEff $ dispatcher this $ GotBlogPosts xs
            )
        $ whileMountedLocalCooking
            params
            "Spec.Content"
            LocalCookingAction
            (\this -> unsafeCoerceEff <<< dispatcher this)
        $ reactSpec
          { componentDidMount = \this ->
              unsafeCoerceEff $ One.putQueue (allowWriting getBlogPostsInput) JSONUnit
          }
  in  R.createElement (R.createClass reactSpec') unit []
