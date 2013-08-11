{-# LANGUAGE OverloadedStrings #-}

module MyProblems (
    getProblems,
    Problem (..)
) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack)
import Network.HTTP

import Config
import AST

data Problem = Problem {
    problemID   :: String,
    problemSize :: Int,
    problemOps  :: [Op],
    solved      :: Maybe Bool,
    timeLeft    :: Maybe Double
} deriving Show

instance FromJSON Problem where
    parseJSON (Object v) = Problem            <$>
                            v .:  "id"        <*>
                            v .:  "size"      <*>
                            v .:  "operators" <*>
                            v .:? "solved"    <*>
                            v .:? "timeLeft"
    parseJSON _          = mzero

problemsURI :: String
problemsURI = "http://icfpc2013.cloudapp.net/myproblems?auth=" ++ apiKey ++ "vpsH1H"

getProblems :: IO (Maybe [Problem])
getProblems = do
    rsp <- simpleHTTP (postRequest problemsURI)
    code <- getResponseCode rsp
    case code of
        (2,0,0) -> decode . pack <$> getResponseBody rsp
        (4,2,9) -> do
            let time = 6000000 -- in microseconds
            putStrLn $ "trying again in " ++ show time ++ " microseconds"
            threadDelay time
            getProblems
        _ -> do
            putStrLn $ "getProblems returned error code: " ++ show code
            return Nothing
