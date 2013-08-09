{-# LANGUAGE OverloadedStrings #-}

module MyProblems (
    getProblems
) where

import Control.Applicative
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
    bdy <- pack <$> getResponseBody rsp
    return (decode bdy)

