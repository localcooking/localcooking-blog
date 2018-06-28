{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  #-}

module Server.Assets where

import Data.FileEmbed (embedFile, embedDir)
import Data.ByteString (ByteString)



-- | The frontend javascript code, as a utf8 encoded binary bytestring.
frontend :: ByteString
frontend = $(embedFile "../frontend/index.js")

frontendMin :: ByteString
frontendMin = $(embedFile "../frontend/index.min.js")

favicons :: [(FilePath, ByteString)]
favicons = $(embedDir "../logo/localcooking-blog-favicon")

images :: [(FilePath, ByteString)]
images = $(embedDir "images")
