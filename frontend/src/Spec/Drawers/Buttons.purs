
module Spec.Drawers.Buttons where

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
     -> R.ReactElement
     -> T.Spec (Effects eff) State Unit Action
spec params@{siteLinks} prefix = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      LocalCookingAction a -> performActionLocalCookingLight getLCState a props state
      Clicked x -> liftEff (siteLinks x)

    -- FIXME generate href links via params.toURI
    render :: T.Render State Unit Action
    render dispatch props state children =
      [ prefix
      -- , divider {}
      -- , listItem
      --   { button: true
      --   , onClick: mkEffFn1 \_ -> unsafeCoerceEff $ siteLinks MealsLink
      --   }
      --   [ listItemIcon {} restaurantMenuIcon
      --   , listItemText
      --     { primary: "Meals"
      --     }
      --   ]
      -- , divider {}
      -- , listItem
      --   { button: true
      --   , onClick: mkEffFn1 \_ -> unsafeCoerceEff $ siteLinks ChefsLink
      --   }
      --   [ listItemIcon {} $ svgIcon {viewBox: chefHatViewBox, color: SvgIcon.action}
      --       [chefHat]
      --   , listItemText
      --     { primary: "Chefs"
      --     }
      --   ]
      ]


drawersButtons :: forall eff
                . LocalCookingParams SiteLinks UserDetails (Effects eff)
               -> R.ReactElement
               -> R.ReactElement
drawersButtons params prefix =
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
