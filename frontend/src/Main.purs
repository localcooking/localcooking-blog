module Main where

import Spec (app)
import Window (widthToWindowSize)
import Links (SiteLinks (..), siteLinksParser, siteLinksToDocumentTitle)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Either (Either (..))
import Data.Time.Duration (Milliseconds (..))
import Data.URI (Scheme (..), Host (..), Port (..), Authority (..))
import Data.URI.Location (toURI)
import Data.Int.Parse (parseInt, toRadix)
import Data.Typelevel.Undefined (undefined)
import Data.String (takeWhile) as String
import Data.UUID (GENUUID)
import Data.Traversable (traverse_)
import Data.Foreign (toForeign, unsafeFromForeign)
import Text.Parsing.StringParser (runParser)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Console (CONSOLE, log, error)

import Queue.One as One
import Signal.Internal as Signal
import IxSignal.Internal as IxSignal
import Signal.Time (debounce)
import Signal.DOM (windowDimensions)

import React as R
import ReactDOM (render)
import Thermite as T
import MaterialUI.InjectTapEvent (INJECT_TAP_EVENT, injectTapEvent)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (location, document, history)
import DOM.HTML.Window.Extra (onPopState)
import DOM.HTML.Document (body)
import DOM.HTML.History (pushState, URL (..), DocumentTitle (..))
import DOM.HTML.Location (hostname, protocol, port, pathname)
import DOM.HTML.Types (HISTORY, htmlElementToElement)


main :: Eff ( console        :: CONSOLE
            , injectTapEvent :: INJECT_TAP_EVENT
            , ref            :: REF
            , dom            :: DOM
            , timer          :: TIMER
            , uuid           :: GENUUID
            , exception      :: EXCEPTION
            , history        :: HISTORY
            ) Unit
main = do
  log "Starting Local Cooking frontend..."

  injectTapEvent


  w <- window
  l <- location w
  h <- history w
  scheme <- Just <<< Scheme <<< String.takeWhile (\c -> c /= ':') <$> protocol l
  authority <- do
    host <- hostname l
    p' <- port l
    p <- case parseInt p' (toRadix 10) of
      Nothing ->  pure Nothing -- undefined <$ error "Somehow couldn't parse port"
      Just x -> pure (Just (Port x))
    pure $ Just $ Authority Nothing [Tuple (NameAddress host) p]

  siteLinksSignal <- do
    q <- One.newQueue
    One.onQueue q \(x :: SiteLinks) ->
      pushState (toForeign x) (siteLinksToDocumentTitle x) (URL (show x)) h
    pure (One.writeOnly q)

  windowSizeSignal <- do
    -- debounces and only relays when the window size changes
    sig <- debounce (Milliseconds 100.0) =<< windowDimensions
    initWidth <- (\w' -> w'.w) <$> Signal.get sig
    windowWidthRef <- newRef initWidth
    out <- IxSignal.make (widthToWindowSize initWidth)
    flip Signal.subscribe sig \w' -> do
      lastWindowWidth <- readRef windowWidthRef
      when (w'.w /= lastWindowWidth) $ do
        writeRef windowWidthRef w'.w
        IxSignal.set (widthToWindowSize w'.w) out
    pure out

  currentPageSignal <- do
    initSiteLink <- do
      p <- pathname l
      if p == ""
        then pure RootLink
        else case runParser siteLinksParser p of
          Left e -> throw (show e)
          Right x -> pure x
    sig <- IxSignal.make initSiteLink
    onPopState (\x -> IxSignal.set (unsafeFromForeign x) sig) w
    pure sig


  let props = unit
      {spec: reactSpec, dispatcher} =
        app
          { toURI : \location -> toURI {scheme, authority, location}
          , windowSizeSignal
          , currentPageSignal
          , siteLinks: One.putQueue siteLinksSignal
          }
      component = R.createClass reactSpec
  traverse_ (render (R.createFactory component props) <<< htmlElementToElement) =<< body =<< document w
