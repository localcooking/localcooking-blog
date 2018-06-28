module Spec.Content.Root where

import Links (SiteLinks (NewBlogPostLink))
import User (UserDetails (..))
import LocalCooking.Thermite.Params (LocalCookingParams, LocalCookingState, LocalCookingAction, performActionLocalCooking, whileMountedLocalCooking, initLocalCookingState)
import LocalCooking.Database.Schema (StoredBlogPostId)
import LocalCooking.Semantics.Blog (BlogPostSynopsis (..), GetBlogPost)
import LocalCooking.Semantics.Common (WithId (..), User (..))
import LocalCooking.Dependencies.Blog (BlogQueues)
import LocalCooking.Common.User.Role (UserRole (Editor))

import Prelude
import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Data.URI (URI)
import Data.URI.URI as URI
import Data.URI.Location (Location, toLocation)
import Data.Lens (Lens', Prism', lens, prism')
import Data.Array as Array
import Data.Argonaut.JSONUnit (JSONUnit (..))
import Data.String.Permalink (Permalink)
import Control.Monad.Base (liftBase)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (warn)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (div, em, img, strong, text) as R
import React.DOM.Props as RP
import React.DOM.Props.PreventDefault (preventDefault)
import React.Signal.WhileMounted as Signal
import React.Queue.WhileMounted as Queue
import DOM.HTML.Window.Extra (WindowSize (Laptop))

import MaterialUI.Types (createStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Button (button)
import MaterialUI.Button as Button
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
import Queue.Types (allowReading, allowWriting, WRITE)



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
  | OpenBlogPost Permalink
  | OpenNewBlogPost

type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  | eff)

getLCState :: Lens' State (LocalCookingState SiteLinks UserDetails)
getLCState = lens (_.localCooking) (_ { localCooking = _ })


spec :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> { openBlogPostQueues :: OneIO.IOQueues (Effects eff) GetBlogPost (Maybe Unit)
        , blogQueues :: BlogQueues (Effects eff)
        }
     -> T.Spec (Effects eff) State Unit Action
spec params {openBlogPostQueues,blogQueues} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      LocalCookingAction a -> performActionLocalCooking getLCState a props state
      GotBlogPosts xs -> void $ T.cotransform _ {blogPosts = Just xs}
      OpenNewBlogPost -> liftEff $ params.siteLinks NewBlogPostLink
      OpenBlogPost permalink -> do
        mPost <- liftBase $ OneIO.callAsync blogQueues.getBlogPostQueues permalink
        case mPost of
          Nothing -> liftEff $ unsafeCoerceEff $ warn $ "couldn't find blog post! " <> show permalink
          Just post -> void $ liftBase $ OneIO.callAsync openBlogPostQueues post

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ typography
        { variant: if state.localCooking.windowSize < Laptop
                      then Typography.headline
                      else Typography.display1
        , align: Typography.center
        , color: Typography.primary
        , style: createStyles {marginBottom: "1em"}
        } [R.text "Blog Posts"]
      , divider {}
      , case state.localCooking.userDetails of
          Nothing -> R.text ""
          Just (UserDetails {user: User {roles}})
            | not (Array.elem Editor roles) -> R.text ""
            | otherwise ->
              button
                { variant: Button.raised
                , color: Button.primary
                , onClick: mkEffFn1 preventDefault
                , onTouchTap: mkEffFn1 \e -> do
                    preventDefault e
                    dispatch OpenNewBlogPost
                , href: URI.print $ params.toURI $ toLocation NewBlogPostLink
                } [R.text "New Blog Post"]
      , case state.blogPosts of
          Nothing -> circularProgress {mode: CircularProgress.indeterminate}
          Just xs
            | Array.null xs -> typography
                { variant: Typography.body1
                , align: Typography.center
                } [R.em [] [R.text "No blog posts"]]
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
                      , onClick: mkEffFn1 \_ -> dispatch (OpenBlogPost permalink)
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
        , openBlogPostQueues :: OneIO.IOQueues (Effects eff) GetBlogPost (Maybe Unit)
        }
     -> R.ReactElement
root params {blogQueues,openBlogPostQueues} =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec params {openBlogPostQueues,blogQueues}
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
