{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Lib
    ( run
    ) where

import Dhall.JSON
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Aeson.Types  (Value, Parser, parseMaybe)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Char (chr)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Network.HTTP.Client as H
import Network.HTTP.Types (RequestHeaders)
import qualified Data.CaseInsensitive as CI
import System.Environment (getArgs)

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



mkHeader :: (String, String) -> (CI.CI BSU.ByteString , BSU.ByteString)
mkHeader  (s1, s2) = (CI.mk $ BSU.fromString s1, BSU.fromString s2) 

parseDhall :: FilePath -> String -> IO Value
parseDhall filePath dhallText = codeToValue defaultConversion ForbidWithinJSON (Just filePath) dhallText'
    where
        dhallText' = T.pack dhallText


dunkToRequest :: DunkRequest -> IO H.Request 
dunkToRequest dReq = do
    request <- H.parseRequest url
    pure $ request { H.method = BSU.fromString $ method dReq
            , H.requestBody = H.RequestBodyLBS reqBody'
            , H.requestHeaders = headers'
            }
    where
        url = baseUrl dReq ++ path dReq
        reqBody' = encode $ fromMaybe "" (reqBody dReq)
        headers' = map mkHeader $ M.toList $ headers dReq
        

run :: IO ()
run = do
    args <- getArgs
    filePath <- case args of
      [filePath] -> pure filePath
      _ -> error "Invalid Argument"

    dhall <- readFile filePath
    json <- encode <$> parseDhall filePath dhall

    request <- case decode json :: Maybe DunkRequest of
      Nothing -> error "Invalid Request"
      Just req -> dunkToRequest req
          

    manager <- H.newManager H.defaultManagerSettings

    response <- H.httpLbs request manager
    let body = H.responseBody response
    
    putStrLn $ BLU.toString body

