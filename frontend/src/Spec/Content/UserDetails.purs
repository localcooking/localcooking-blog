module Spec.Content.UserDetails where

import Links (SiteLinks (UserDetailsLink), UserDetailsLinks (..))
import User (UserDetails)
import Spec.Content.UserDetails.General (general)
import LocalCooking.Thermite.Params (LocalCookingParams, LocalCookingStateLight, LocalCookingActionLight, performActionLocalCookingLight, whileMountedLocalCookingLight, initLocalCookingStateLight)

import Prelude
import Data.Lens (Lens', Prism', lens, prism')

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (text) as R
import React.Signal.WhileMounted as Signal

import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Exception (EXCEPTION)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal



type State =
  { localCooking :: LocalCookingStateLight SiteLinks
  }

initialState :: LocalCookingStateLight SiteLinks -> State
initialState localCooking =
  { localCooking
  }

data Action
  = LocalCookingActionLight (LocalCookingActionLight SiteLinks)

type Effects eff =
  ( ref :: REF
  , uuid :: GENUUID
  , exception :: EXCEPTION
  | eff)

getLCState :: Lens' State (LocalCookingStateLight SiteLinks)
getLCState = lens (_.localCooking) (_ { localCooking = _ })


spec :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> T.Spec (Effects eff) State Unit Action
spec params@{siteLinks} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      LocalCookingActionLight a -> performActionLocalCookingLight getLCState a props state

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ case state.localCooking.currentPage of -- TODO pack currentPageSignal listener to this level, so
                            -- side buttons aren't redrawn
          UserDetailsLink mUserDetails -> case mUserDetails of
            Nothing -> general
            Just x -> case x of
              UserDetailsGeneralLink -> general
              _ -> R.text ""
          _ -> R.text ""
      ]


userDetails :: forall eff
             . LocalCookingParams SiteLinks UserDetails (Effects eff)
            -> R.ReactElement
userDetails params =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec params
          ) (initialState (unsafePerformEff (initLocalCookingStateLight params)))
      reactSpec' =
        whileMountedLocalCookingLight
          params
          "Spec.Content"
          LocalCookingActionLight
          (\this -> unsafeCoerceEff <<< dispatcher this)
          reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
