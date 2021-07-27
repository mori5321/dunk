{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Main where

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

tmpFilePath :: String
tmpFilePath = "./dhall/login.dhall"

main :: IO ()
main = do
    -- 1. Parse Dhall
    dhall <- readFile tmpFilePath
    json <- encode <$> parseDhall tmpFilePath dhall

    manager <- H.newManager H.defaultManagerSettings

    case decode json :: Maybe DunkRequest of
        Nothing -> print "Parse Error"
        Just req -> do
            -- 2. Dhall Request to HTTP Reqquest
            let url = baseUrl req ++ path req
            request' <- H.parseRequest url

            let reqBody' =  encode $ fromMaybe "" (reqBody req)
            let headers' = M.toList $ headers req
            let headers'' = map mkHeader headers'
            let request = request'
                    { H.method = BSU.fromString $ method req
                    , H.requestBody = H.RequestBodyLBS reqBody'
                    , H.requestHeaders = headers''
                    }

            -- 3. Send Request and Receive Response
            response <- H.httpLbs request manager
            let body = H.responseBody response

            -- 4. Presentation
            putStrLn $ BLU.toString body

