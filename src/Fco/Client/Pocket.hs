{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- |
--
--

module Fco.Client.Pocket where

import BasicPrelude

import Data.Aeson (encode, object, (.=), Object, Value (Object, String))
import Data.Aeson.Parser (json)
import Data.Conduit ((.|), runConduit)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Conduit.Combinators (sourceHandle)
import qualified Data.Hash as H
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import Network.HTTP.Client (
        newManager, parseRequest, requestBody, requestHeaders,
        responseBody, responseStatus, withResponse,
        RequestBody(RequestBodyLBS))
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.IO (withFile, IOMode (ReadMode))

import Control.Concurrent.Actor.Config (loadConfig)
import Fco.Core.Struct (lookup, lookupString)


data LinkData = LinkData Text (Set.Set Text)
                  deriving (Eq, Ord, Show)


run :: IO ()
run = queryPocket "../../data/config-fco.yaml"  >>= processValue >>= print
--run = loadFromFile >>= processValue >>= print


queryPocket :: FilePath -> IO Value
queryPocket path = do
    manager <- newManager tlsManagerSettings
    baseReq <- parseRequest "POST https://getpocket.com/v3/get"
    conf <- loadConfig path
    let (Just pconf) = HM.lookup "client-pocket" conf
        reqData = object [
          "consumer_key" .= HM.lookup "consumer-key" pconf,
          "access_token" .= HM.lookup "access-token" pconf,
          "detailType" .= ("complete"::String)]
        req = baseReq {
          requestBody = RequestBodyLBS $ encode reqData,
          requestHeaders = [
                ("Content-Type", "application/json; charset=utf-8"), 
                ("X-Accept", "application/json")]}
    withResponse req manager $ \resp -> do
        putStrLn $
            "Status Code: " ++ (T.pack $ show (statusCode $ responseStatus resp))
        runConduit (bodyReaderSource (responseBody resp) .| sinkParser json)


loadFromFile :: FilePath -> IO Value
loadFromFile path = do
    withFile path ReadMode $ \handle ->
      runConduit (sourceHandle handle .| sinkParser json)


processValue :: Value -> IO (HM.HashMap Text LinkData)
processValue value = do
    --print value
    let links = extractData $ extractLinkList value
    let tags = collectTags links
    storeTags tags
    return links


storeTags :: Set.Set Text -> IO ()
storeTags tags = return ()


-- extract data from Pocket JSON

collectTags :: HM.HashMap Text LinkData -> Set.Set Text
collectTags links = foldr getTags Set.empty links
  where getTags (LinkData _ tags) set = Set.union tags set


extractLinkList :: Value -> HM.HashMap Text Value
extractLinkList (Object v) = case HM.lookup "list" v of
          Just (Object list) -> list
          _ -> error "'list' item not found or malformed."


extractData :: HM.HashMap Text Value -> HM.HashMap Text LinkData
extractData list = foldr extractAndAdd HM.empty list
  where extractAndAdd (Object v) hashMap = 
                HM.insert (getUrl v) (makeValue v) hashMap
        getUrl v = lookupString "given_url" v
        makeValue v = LinkData (lookupString "given_title" v) (getTags v)
        getTags v = case HM.lookup "tags" v of
            Just (Object x) -> Set.fromList (HM.keys x)
            Nothing -> Set.fromList []
            _ -> error "'tags' item not found or malformed."

