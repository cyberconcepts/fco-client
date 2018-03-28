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


data LinkData = LinkData Text (Set.Set Text)
                  deriving (Eq, Ord, Show)


run :: IO ()
run = queryPocket >>= processValue >>= print
--run = loadFromFile >>= processValue >>= print


queryPocket :: IO Value
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
        bodyReaderSource (responseBody resp) $$ sinkParser json


loadFromFile :: FilePath -> IO Value
loadFromFile path = do
    conf <- loadConfig
    withFile path ReadMode $ \handle ->
      sourceHandle handle $$ sinkParser json


processValue :: Value -> IO (HM.HashMap Text LinkData)
processValue value = do
    --print value
    let links = extractData $ extractLinkList value
    let tags = collectTags links
    return links


extractLinkList :: Value -> HM.HashMap Text Value
extractLinkList value = case lookupList value of
          Just (Object list) -> list
          Nothing -> HM.empty
  where lookupList v = case v of
          Object v1 -> HM.lookup "list" v1
          _ -> Nothing


extractData :: HM.HashMap Text Value -> HM.HashMap Text LinkData
extractData list = foldr extractAndAdd HM.empty list
  where extractAndAdd :: Value -> HM.HashMap Text LinkData
                               -> HM.HashMap Text LinkData
        extractAndAdd (Object v) hashMap = 
                HM.insert (getUrl v) (makeValue v) hashMap
        getUrl v = lookupString "given_url" v
        makeValue v = LinkData (lookupString "given_title" v) (getTags v)
        getTags v = case HM.lookup "tags" v of
            Just (Object x) -> Set.fromList (HM.keys x)
            Nothing -> Set.fromList ["error"]


collectTags :: HM.HashMap Text LinkData -> Set.Set Text
collectTags links = foldr getTags Set.empty links
  where getTags (LinkData _ tags) set = Set.union tags set

