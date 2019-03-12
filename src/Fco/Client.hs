{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Client where

import BasicPrelude

import Data.Aeson (encode, object, (.=), Object, Value (Object, String))
import Data.Aeson.Parser (json)
import Crypto.Hash (MD5(..), SHA1(..), hashWith)
import Data.ByteArray.Encoding (convertToBase, Base (Base64, Base64URLUnpadded))
import qualified Data.ByteString as B
import Data.Conduit (($$))
import Data.Conduit.Attoparsec (sinkParser)
import Data.Conduit.Combinators (sourceHandle)
import qualified Data.Hash as H
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import Encoding (toBase62)

import Network.HTTP.Client (
        newManager, parseRequest, requestBody, requestHeaders,
        responseBody, responseStatus, withResponse,
        RequestBody(RequestBodyLBS))
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.IO (withFile, IOMode (ReadMode))

import Fco.Core.Struct (lookupString)


-- general utility functions

getHash0 :: Text -> Text  -- using the data-hash package
getHash0 = T.pack . toBase62 . H.asWord64 . H.hash . T.unpack

getHash1 :: Text -> Text  -- using the hashable package (in Prelude)
getHash1 = T.pack . toBase62 . fromIntegral . hash

getHash :: Text -> Text   -- standardized, portable version
getHash = decodeUtf8 . convertToBase Base64URLUnpadded
              . hashWith MD5 . encodeUtf8

