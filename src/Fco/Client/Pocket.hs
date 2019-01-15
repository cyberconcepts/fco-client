{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- |
--
--

module Fco.Client.Pocket where

import BasicPrelude

import Data.Aeson (encode, object, (.=), Object, Value (Object, String))
import Data.Aeson.Parser (json)
--import Crypto.Hash (MD5(..), SHA1(..), hashWith)
--import Data.ByteArray.Encoding (convertToBase, Base (Base64, Base64URLUnpadded))
--import qualified Data.ByteString as B
import Data.Conduit (($$))
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

import Fco.Core.Config (loadPocketConfig)
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
    conf <- loadPocketConfig
    let reqData = object [
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
            "Status Code: " ++ (T.pack $ show (statusCode $ responseStatus resp))
        bodyReaderSource (responseBody resp) $$ sinkParser json


loadFromFile :: FilePath -> IO Value
loadFromFile path = do
    conf <- loadPocketConfig
    withFile path ReadMode $ \handle ->
      sourceHandle handle $$ sinkParser json


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
            _ -> error "'tags' item not found or malformed."

