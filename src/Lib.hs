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

import Request (sendRequest, decodeDunkRequest)

import Dhall.JSON
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import Data.Aeson.Types  (Value, Parser, parseMaybe)
import Data.Aeson (encode)
import Data.Char (chr)
import System.Environment (getArgs)
import Data.Text as T


parseDhall :: FilePath -> String -> IO Value
parseDhall filePath dhallText = codeToValue defaultConversion ForbidWithinJSON (Just filePath) dhallText'
    where
        dhallText' = T.pack dhallText

run :: IO ()
run = do
    args <- getArgs
    filePath <- case args of
      [filePath] -> pure filePath
      _ -> error "Invalid Argument"

    dhall <- readFile filePath
    json <- encode <$> parseDhall filePath dhall


    request' <- decodeDunkRequest json
    request <- case request' of
                    Left msg -> error msg
                    Right req -> pure req

    
    response <- sendRequest request
    
    putStrLn $ BSLU.toString $ encode response

