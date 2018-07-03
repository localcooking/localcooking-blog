{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , NamedFieldPuns
  , ScopedTypeVariables
  , RecordWildCards
  , DataKinds
  , QuasiQuotes
  #-}

module Server.HTTP where

import Links (SiteLinks (RootLink, NewBlogPostLink, UserDetailsLink), UserDetailsLinks (..))

import LocalCooking.Types (Env (..))
import LocalCooking.Function.System (SystemM)

import Web.Routes.Nested (RouterT, match, matchHere, matchGroup, matchAny, textOnly, l_, (</>), o_)
import Network.Wai.Trans (MiddlewareT)
import Network.HTTP.Types (status302)
import qualified Data.Text.Encoding        as T
import Data.URI (printURI)
import Data.Url (packLocation)
import qualified Data.Strict.Maybe as Strict
import Path.Extended (ToLocation (toLocation))
import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift)




httpServer :: Env -> (SiteLinks -> MiddlewareT SystemM) -> RouterT (MiddlewareT SystemM) sec SystemM ()
httpServer Env{envMkURI} handleAuthToken = do

  -- main routes
  matchGroup (l_ "userDetails" </> o_) $ do
    matchHere $ handleAuthToken $ UserDetailsLink Nothing
    match (l_ "general" </> o_) $ handleAuthToken $ UserDetailsLink $ Just UserDetailsGeneral
    match (l_ "security" </> o_) $ handleAuthToken $ UserDetailsLink $ Just UserDetailsSecurity
  --   match (l_ "orders" </> o_) handleAuthToken
  --   match (l_ "diet" </> o_) handleAuthToken
  --   match (l_ "allergies" </> o_) handleAuthToken
  matchGroup (l_ "newBlogPost" </> o_) $
    matchHere $ handleAuthToken NewBlogPostLink
  -- matchGroup (l_ "chefs" </> o_) $
  --   matchHere handleAuthToken
  matchAny $ \_ _ resp -> do
    resp $ textOnly "" status302
      [ ( "Location"
        , T.encodeUtf8
          $ printURI
          $ envMkURI
          $ toLocation $ RootLink Nothing
        )
      ]

  -- -- images -- FIXME handled by NGINX, so...
  -- matchGroup (l_ "images" </> o_) $ do
  --   forM_ images $ \(file, content) -> do
  --     let (file', ext) = T.breakOn "." (T.pack file)
  --     match (l_ file' </> o_) $ action $ get $
  --       bytestring (Other (T.dropWhile (== '.') ext)) (LBS.fromStrict content)

