{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Client where

import BasicPrelude

import Data.Aeson (encode, object, (.=))
import Data.Aeson.Parser (json)
import Data.Conduit (($$))
import Data.Conduit.Attoparsec(sinkParser)
import Network.HTTP.Client (
        newManager, parseRequest, requestBody, requestHeaders,
        responseBody, responseStatus, withResponse,
        RequestBody(RequestBodyLBS))
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Data.Text (pack)


run :: IO ()
run = do
    manager <- newManager tlsManagerSettings
    baseReq <- parseRequest "POST https://getpocket.com/v3/get"
    let reqData = object [
          "consumer_key" .= ("12345-abcd1234abcd1234abcd1234"::String),
          "access_token" .= ("5678defg-5678-defg-5678-defg56"::String),
          "detailType" .= ("complete"::String)]
    let req = baseReq {
          requestBody = RequestBodyLBS $ encode reqData,
          requestHeaders = [
                ("Content-Type", "application/json; charset=utf-8"), 
                ("X-Accept", "application/json")]}
    withResponse req manager $ \resp -> do
        putStrLn $
            "Status Code: " ++ (pack $ show (statusCode $ responseStatus resp))
        value <- bodyReaderSource (responseBody resp) $$ sinkParser json
        print value
    return ()
