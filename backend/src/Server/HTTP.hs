{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , NamedFieldPuns
  , ScopedTypeVariables
  , RecordWildCards
  , DataKinds
  #-}

module Server.HTTP where

import LocalCooking.Common.AuthToken (AuthToken)
import Server.Dependencies.AuthToken (authTokenServer, AuthTokenInitIn (AuthTokenInitInFacebookCode), AuthTokenInitOut (AuthTokenInitOutSuccess))
import Server.Assets (favicons, frontend)
import Types (AppM, runAppM, HTTPException (..))
import Types.Env (Env (..), Managers (..), isDevelopment, Development (..))
import Types.FrontendEnv (FrontendEnv (..))
import Types.Keys (Keys (..))
import Template (html)
import Login.Error (AuthError (..), PreliminaryAuthToken (..))
import Facebook.Types (FacebookLoginCode (..))
import Facebook.State (FacebookLoginState (..))

import Web.Routes.Nested (RouterT, match, matchHere, matchGroup, action, post, get, json, text, textOnly, l_, (</>), o_, route)
import Web.Dependencies.Sparrow.Types (ServerContinue (ServerContinue, serverContinue), ServerReturn (ServerReturn, serverInitOut))
import Network.Wai (strictRequestBody, queryString)
import Network.Wai.Middleware.ContentType (bytestring, FileExt (Other, JavaScript))
import Network.Wai.Trans (MiddlewareT)
import Network.WebSockets (defaultConnectionOptions)
import Network.WebSockets.Trans (websocketsOrT)
import Network.HTTP.Types (status302)
import Network.HTTP.Client (httpLbs, responseBody, parseRequest)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Lazy   as LBS
import Data.URI (URI (..), printURI)
import Data.Aeson (FromJSON (..), (.:))
import Data.Aeson.Types (typeMismatch, Value (String, Object))
import qualified Data.Aeson as Aeson
import Data.TimeMap (TimeMap)
import qualified Data.TimeMap as TimeMap
import Data.TimeMap.Multi (TimeMultiMap)
import qualified Data.TimeMap.Multi as TimeMultiMap
import qualified Data.Attoparsec.Text as Atto
import qualified Data.IxSet as IxSet
import Data.Monoid ((<>))
import qualified Data.Strict.Maybe as Strict
import Data.Strict.Tuple (Pair (..))
import Control.Applicative ((<|>))
import Control.Monad (join, when, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift)
import Control.Exception.Safe (throwM)
import Control.Logging (log', warn')
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMapChan (TMapChan, newTMapChan)
import qualified Control.Concurrent.STM.TMapChan as TMapChan
import Control.Concurrent.Chan.Scope (Scope (..))
import Control.Concurrent.STM.TChan.Typed (TChanRW)
import Crypto.Saltine.Core.Box (newNonce)
import qualified Crypto.Saltine.Class as NaCl
import System.IO.Error (userError)



router :: RouterT (MiddlewareT AppM) sec AppM ()
router
  = do
  matchHere $ \app req resp ->
    case join $ lookup "authToken" $ queryString req of
      Nothing -> (action $ get $ html Nothing "") app req resp
      Just json -> case Aeson.decode $ LBS.fromStrict json of
        Nothing -> do
          log' $ "Bad authToken encoding: " <> T.pack (show json)
          (action $ get $ html Nothing "") app req resp
        Just eToken -> (action $ get $ html (Just eToken) "") app req resp

  match (l_ "about" </> o_) $ action $ get $ html Nothing "" -- FIXME SEO, authToken handling forall endpoints

  forM_ favicons $ \(file, content) -> do
    let (file', ext) = T.breakOn "." (T.pack file)
    match (l_ file' </> o_) $ action $ get $
      bytestring (Other (T.dropWhile (== '.') ext)) (LBS.fromStrict content)

  match (l_ "index.js" </> o_) $ \app req resp -> do
    Env{envDevelopment} <- ask
    case envDevelopment of
      Nothing -> pure ()
      Just Development{devCacheBuster} -> case join $ lookup "cache_buster" $ queryString req of
        Nothing -> fail "No cache busting parameter!"
        Just cacheBuster
          | cacheBuster == BS16.encode (NaCl.encode devCacheBuster) -> pure ()
          | otherwise -> fail "Wrong cache buster!" -- FIXME make cache buster generic
    (action $ get $ bytestring JavaScript $ LBS.fromStrict frontend) app req resp

  match (l_ "facebookLoginReturn" </> o_) $ \app req resp -> do
    Env{envHostname,envTls} <- ask
    let qs = queryString req
    ( eToken :: Either AuthError AuthToken
      , mFbState :: Maybe FacebookLoginState
      ) <- case do  let bad = do
                          errorCode <- join $ lookup "error_code" qs
                          errorMessage <- join $ lookup "error_message" qs
                          pure $ Left $ FBLoginReturnBad (T.decodeUtf8 errorCode) (T.decodeUtf8 errorMessage)
                        denied = do
                          error' <- join $ lookup "error" qs
                          errorReason <- join $ lookup "error_reason" qs
                          errorDescription <- join $ lookup "error_description" qs
                          if error' == "access_denied" && errorReason == "user_denied"
                            then pure $ Left $ FBLoginReturnDenied $ T.decodeUtf8 errorDescription
                            else Nothing
                        good = do
                          code <- fmap T.decodeUtf8 $ join $ lookup "code" qs
                          (state :: FacebookLoginState) <- do -- FIXME decide a monomorphic state to share for CSRF prevention
                            x <- join $ lookup "state" qs
                            join $ Aeson.decode $ LBS.fromStrict x
                          pure $ Right (FacebookLoginCode code, state)
                    bad <|> good <|> denied of

              Nothing -> do
                liftIO $ do
                  putStr "Bad /facebookLoginReturn parse: "
                  print qs
                pure (Left FBLoginReturnBadParse, Nothing)
              Just eX -> case eX of
                Left e -> pure (Left e, Nothing)
                Right (code, state) -> do
                  mCont <- authTokenServer $ AuthTokenInitInFacebookCode code
                  case mCont of
                    Nothing -> pure (Left FBLoginReturnNoUser, Just state)
                    Just ServerContinue{serverContinue} -> do
                      ServerReturn{serverInitOut = AuthTokenInitOutSuccess authToken} <- serverContinue undefined
                      pure (Right authToken, Just state)

    -- TODO redirect smart with FacebookLoginState
    let redirectUri = URI (Strict.Just $ if envTls then "https" else "http")
                          True
                          envHostname
                          []
                          ["authToken" :!: Strict.Just (T.decodeUtf8 $ LBS.toStrict $ Aeson.encode $ PreliminaryAuthToken $ Just eToken)]
                          Strict.Nothing
    resp $ textOnly "" status302 [("Location", T.encodeUtf8 $ printURI redirectUri)]

  match (l_ "facebookLoginDeauthorize" </> o_) $ \app req resp -> do
    body <- liftIO $ strictRequestBody req
    log' $ "Got deauthorized: " <> T.pack (show body)
    (action $ post $ \_ -> text "") app req resp




httpServer :: RouterT (MiddlewareT AppM) sec AppM ()
           -> MiddlewareT AppM
httpServer dependencies = \app req resp -> do
  loginRefs <- liftIO $ atomically newTMapChan
  route ( do dependencies
             router
        ) app req resp
