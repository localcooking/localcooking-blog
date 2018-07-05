module User where

import LocalCooking.Global.User.Class (class UserDetails)
import LocalCooking.Semantics.Common (User)

import Prelude
import Data.Maybe (Maybe)
import Data.Generic (class Generic, gShow, gEq)



data PreUserDetails = PreUserDetails (Maybe User)


newtype UserDetails = UserDetails
  { user :: User
  }

derive instance genericUserDetails :: Generic UserDetails

instance showUserDetails :: Show UserDetails where
  show = gShow

instance eqUserDetails :: Eq UserDetails where
  eq = gEq

instance userDetailsUserDetails :: UserDetails UserDetails where
  getUser (UserDetails {user}) = user
  setUser user (UserDetails u) = UserDetails u {user = user}
