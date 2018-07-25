module Spec.Topbar.Buttons where

import Links (SiteLinks)
import User (UserDetails)
import LocalCooking.Thermite.Params (LocalCookingParams, LocalCookingStateLight, LocalCookingActionLight, initLocalCookingStateLight, performActionLocalCookingLight, whileMountedLocalCookingLight)

import Prelude
import Data.UUID (GENUUID)
import Data.Lens (Lens', lens)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import Thermite as T
import React (ReactElement, createClass, createElement) as R



type State =
  { localCooking :: LocalCookingStateLight SiteLinks
  }

initialState :: LocalCookingStateLight SiteLinks -> State
initialState localCooking =
  { localCooking
  }

data Action
  = LocalCookingAction (LocalCookingActionLight SiteLinks)
  | Clicked SiteLinks

type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , uuid :: GENUUID
  | eff)

getLCState :: Lens' State (LocalCookingStateLight SiteLinks)
getLCState = lens (_.localCooking) (_ { localCooking = _ })


spec :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> Array R.ReactElement
     -> T.Spec (Effects eff) State Unit Action
spec params@{siteLinks,toURI} prefix = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      LocalCookingAction a -> performActionLocalCookingLight getLCState a props state
      Clicked x -> liftEff (siteLinks x)

    render :: T.Render State Unit Action
    render dispatch props state children =
      prefix <>
      []
      -- [ button
      --   { color: Button.primary
      --   , disabled: state.currentPage == MealsLink
      --   , onClick: mkEffFn1 preventDefault
      --   , onTouchTap: mkEffFn1 \e -> do
      --       preventDefault e
      --       dispatch (Clicked MealsLink)
      --   , href: URI.print $ toURI $ toLocation MealsLink
      --   , variant: Button.raised
      --   } [R.text "Meals"]
      -- , button
      --   { color: Button.secondary
      --   , disabled: state.currentPage == ChefsLink
      --   , onClick: mkEffFn1 preventDefault
      --   , onTouchTap: mkEffFn1 \e -> do
      --       preventDefault e
      --       dispatch (Clicked ChefsLink)
      --   , href: URI.print $ toURI $ toLocation ChefsLink
      --   , variant: Button.raised
      --   } [R.text "Chefs"]
      -- ]


topbarButtons :: forall eff
               . LocalCookingParams SiteLinks UserDetails (Effects eff)
              -> Array R.ReactElement
              -> R.ReactElement
topbarButtons params prefix =
  let {spec:reactSpec,dispatcher} =
        T.createReactSpec
          ( spec params prefix
          ) (initialState (unsafePerformEff (initLocalCookingStateLight params)))
      reactSpec' =
        whileMountedLocalCookingLight
          params
          "Spec.Content"
          LocalCookingAction
          (\this -> unsafeCoerceEff <<< dispatcher this)
          reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
