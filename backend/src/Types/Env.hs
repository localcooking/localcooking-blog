{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , NamedFieldPuns
  #-}

module Types.Env where

import LocalCooking.Common.Password (HashedPassword)
import Types.Keys (Keys)

import Data.TimeMap (TimeMap)
import qualified Data.TimeMap as TimeMap
import Data.Word (Word64)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.URI.Auth (URIAuth (..))
import Data.URI.Auth.Host (URIAuthHost (..))
import Data.Default (Default (..))
import qualified Data.Strict.Maybe as Strict
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Pool (destroyAllResources)
import Control.Concurrent.Async (Async, cancel)
import Control.Concurrent.STM (TVar, newTVar, atomically)
import Crypto.Saltine.Core.Box (Nonce, newNonce)
import System.IO.Unsafe (unsafePerformIO)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Database.Persist.Sql (ConnectionPool)



data Managers = Managers
  { managersFacebook  :: Manager
  , managersReCaptcha :: Manager
  }

instance Default Managers where
  def = unsafePerformIO defManagers

defManagers :: IO Managers
defManagers = do
  managersFacebook <- newTlsManager -- FIXME could bug out from facebook booting us
  managersReCaptcha <- newTlsManager
  pure Managers
    { managersFacebook
    , managersReCaptcha
    }


data Development = Development
  { devCacheBuster :: Nonce
  }

instance Default Development where
  def = unsafePerformIO defDevelopment

defDevelopment :: IO Development
defDevelopment = do
  devCacheBuster <- newNonce
  pure Development
    { devCacheBuster
    }

isDevelopment :: Env -> Bool
isDevelopment Env{envDevelopment} = case envDevelopment of
  Nothing -> False
  Just _ -> True



data Env = Env
  { envHostname    :: URIAuth
  , envSMTPHost    :: URIAuthHost
  , envDevelopment :: Maybe Development
  , envTls         :: Bool
  , envKeys        :: Keys
  , envManagers    :: Managers
  , envDatabase    :: ConnectionPool
  , envSalt        :: HashedPassword
  }

instance Default Env where
  def = Env
    { envHostname    = URIAuth Strict.Nothing Localhost (Strict.Just 3000)
    , envSMTPHost    = Localhost
    , envDevelopment = def
    , envTls         = False
    , envKeys        = error "No access to secret keys in default environment"
    , envManagers    = def
    , envDatabase    = error "No database"
    , envSalt        = error "No salt"
    }


releaseEnv :: Env -> IO ()
releaseEnv Env{envDatabase} =
  destroyAllResources envDatabase
