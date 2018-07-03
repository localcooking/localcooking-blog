module Test.Main where

import Links (SiteLinks)
import Facebook.State (FacebookLoginState)

import Prelude
import Data.Either (Either (..))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.URI.Location (class ToLocation, class FromLocation, toLocation, fromLocation)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.QuickCheck (quickCheck, Result (..))



main :: Eff _ Unit
main = do
  log "Location Iso:"
  log "    SiteLinks:"
  quickCheck (\(x :: SiteLinks) -> locationIso x)
  log ""
  log "JSON Iso:"
  log "    FacebookLoginState:"
  quickCheck (\(x :: FacebookLoginState) -> jsonIso x)


locationIso :: forall a. ToLocation a => FromLocation a => Eq a => Show a => a -> Result
locationIso x = case fromLocation (toLocation x) of
  Left e -> Failed $ "Failed parse: " <> e 
  Right y
    | y == x -> Success
    | otherwise -> Failed $ "Not equal parses - x: " <> show x <> ", y: " <> show y


jsonIso :: forall a. EncodeJson a => DecodeJson a => Eq a => Show a => a -> Result
jsonIso x = case decodeJson (encodeJson x) of
  Left e -> Failed $ "Failed parse: " <> e 
  Right y
    | y == x -> Success
    | otherwise -> Failed $ "Not equal parses - x: " <> show x <> ", y: " <> show y
