{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Client where

import BasicPrelude

import Data.Aeson (encode, object, (.=), Object, Value (Object, String))
import Data.Aeson.Parser (json)
import Data.Conduit (($$))
import Data.Conduit.Attoparsec (sinkParser)
import Data.Conduit.Combinators (sourceHandle)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import Network.HTTP.Client (
        newManager, parseRequest, requestBody, requestHeaders,
        responseBody, responseStatus, withResponse,
        RequestBody(RequestBodyLBS))
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.IO (withFile, IOMode (ReadMode))
import Data.Text (pack)

import Fco.Core.Config (loadConfig)
import Fco.Core.Struct (lookupString)


run :: IO ()
run = queryPocket >>= print
--run = loadFromFile >>= print

queryPocket :: IO (Set.Set Text)
queryPocket = do
    manager <- newManager tlsManagerSettings
    baseReq <- parseRequest "POST https://getpocket.com/v3/get"
    conf <- loadConfig
    let reqData = object [
          --"consumer_key" .= ("12345-abcd1234abcd1234abcd1234"::String),
          --"access_token" .= ("5678defg-5678-defg-5678-defg56"::String),
          "consumer_key" .= lookupString "consumer_key" conf,
          "access_token" .= lookupString "access_token" conf,
          "detailType" .= ("complete"::String)]
    let req = baseReq {
          requestBody = RequestBodyLBS $ encode reqData,
          requestHeaders = [
                ("Content-Type", "application/json; charset=utf-8"), 
                ("X-Accept", "application/json")]}
    withResponse req manager $ \resp -> do
        putStrLn $
            "Status Code: " ++ (pack $ show (statusCode $ responseStatus resp))
        (bodyReaderSource (responseBody resp) $$ sinkParser json) >>= processValue


loadFromFile :: FilePath -> IO (Set.Set Text)
loadFromFile path = do
    conf <- loadConfig
    --let path = "test/data/pocketdata.json"
    withFile path ReadMode $ \handle ->
      (sourceHandle handle $$ sinkParser json) >>= processValue


processValue :: Value -> IO (Set.Set Text)
processValue value = do
    --print value
    let v2 = case value of
          Object v1 -> HM.lookup "list" v1
          _ -> Nothing
    let v4 = case v2 of
          Just (Object v3) -> extractTags v3
          Nothing -> Set.fromList ["error"]
    return v4


extractTags :: HM.HashMap Text Value -> Set.Set Text
extractTags list = foldr extractAndAdd Set.empty list
  where extractAndAdd :: Value -> Set.Set Text -> Set.Set Text
        extractAndAdd (Object v) set = case HM.lookup "tags" v of
          Just (Object x) -> Set.union (Set.fromList (HM.keys x)) set
          Nothing -> Set.fromList ["error"]
