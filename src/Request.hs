{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Request
    ( sendRequest
    , decodeDunkRequest
    ) where

import GHC.Generics
import Data.Aeson.Types (Value, Parser, parseMaybe)
import Data.Aeson
import Network.HTTP.Types (RequestHeaders)
import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.HTTP.Client as H
import Network.HTTP.Client (Request, RequestBody(RequestBodyLBS), httpLbs, newManager, defaultManagerSettings, responseBody, responseStatus)
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Data.Map.Strict (Map, toList)
import Data.Maybe (fromMaybe)
import qualified Data.CaseInsensitive as CI

data DunkRequest = DunkRequest
    { baseUrl :: String
    , path :: String
    , method :: String
    , headers :: Map String String
    , reqBody :: Maybe Value
    } deriving (Show)

instance FromJSON DunkRequest where
    parseJSON (Object v) = DunkRequest <$> (v .: "baseUrl")
                                       <*> (v .: "path" )
                                       <*> (v .: "method")
                                       <*> (v .: "headers")
                                       <*> (v .: "reqBody")
    parseJSON _ = undefined


data DunkResponse = DunkResponse
    { body :: Maybe Value
    , statusCode :: Int
    } deriving (Generic, ToJSON)

mkHeader :: (String, String) -> (CI.CI BS.ByteString , BS.ByteString)
mkHeader  (s1, s2) = (CI.mk $ BS.fromString s1, BS.fromString s2) 

dunkReqToHttpRequest :: DunkRequest -> IO Request
dunkReqToHttpRequest dReq = do
    request <- H.parseRequest url
    pure $ request { H.method = BS.fromString $ method dReq
                    , H.requestBody = RequestBodyLBS reqBody'
                    , H.requestHeaders = headers'
                    }
    where
        url = baseUrl dReq ++ path dReq
        reqBody' = encode $ fromMaybe "" (reqBody dReq)
        headers' = map mkHeader $ toList $ headers dReq


decodeDunkRequest :: BSL.ByteString -> IO (Either String DunkRequest)
decodeDunkRequest json = do
    case decode json :: Maybe DunkRequest of
      Nothing -> pure $ Left "Invalid Request" 
      Just req ->  pure $ Right req

sendRequest :: DunkRequest -> IO DunkResponse
sendRequest dReq = do
    manager <- newManager defaultManagerSettings
    request <- dunkReqToHttpRequest dReq
    response <- httpLbs request manager
    pure $ DunkResponse
            { body = decode $ responseBody response
            , statusCode = HTTPTypes.statusCode $ responseStatus response
            }
    where 
        request = dunkReqToHttpRequest dReq
        body' = responseBody 
