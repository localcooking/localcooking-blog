{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , ScopedTypeVariables
  , QuasiQuotes
  , DataKinds
  #-}

module Server where

import Server.HTTP (httpServer)
import Server.Assets (favicons, frontend, frontendMin)
import Links (SiteLinks)

import LocalCooking.Server (LocalCookingArgs (..))
import LocalCooking.Colors (LocalCookingColors (..))
import LocalCooking.Dependencies.Blog (blogDependencies)
import LocalCooking.Dependencies.Tag (tagDependencies)

import Text.Lucius (Color (..))


server :: LocalCookingArgs SiteLinks sec
server = LocalCookingArgs
  { localCookingArgsFrontend = frontend
  , localCookingArgsFrontendMin = frontendMin
  , localCookingArgsFavicons = favicons
  , localCookingArgsHTTP = httpServer
  , localCookingArgsDeps = do
      blogDependencies
      tagDependencies
  , localCookingArgsColors = LocalCookingColors
    { localCookingColorsMain = Color 114 91 83
    , localCookingColorsActive = Color 161 136 127
    , localCookingColorsHover = Color 211 184 174
    }
  }
