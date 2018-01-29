{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Client where

import BasicPrelude

import Data.Aeson (decode, encode, object, (.=), Object, Value (Object))
import Data.Aeson.Parser (json)
import qualified Data.ByteString.Lazy as BS
import Data.Conduit (($$))
import Data.Conduit.Attoparsec (sinkParser)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
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


loadFromFile :: IO ()
path = "~/development/data/pocket/output-180122.json"
loadFromFile = do 
    input <- BS.readFile path
    let value = decode (input :: BS.ByteString) :: Maybe Object
    let v2 = case value of
          --Just v1 -> HM.keys v1
          Just v1 -> HM.lookup "list" v1
          Nothing -> Nothing
    let v4 = case v2 of
          Just (Object v3) -> extractTags v3
          Nothing -> Set.fromList ["error"]
    print v4


extractTags :: HM.HashMap Text Value -> Set.Set Text
extractTags list = foldl extractAndAdd Set.empty list
  where extractAndAdd :: Set.Set Text -> Value -> Set.Set Text
        extractAndAdd set (Object v) = case HM.lookup "tags" v of
          Just (Object x) -> Set.union (Set.fromList (HM.keys x)) set
          Nothing -> Set.fromList ["error"]
  