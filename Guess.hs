{-# LANGUAGE OverloadedStrings #-}

module Guess (
    GuessRequest (..),
    GuessResponse (..),
    rawGuess
) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Config
import Network.HTTP
import Data.ByteString.Lazy.Char8 (pack, unpack)

import AST
import PrettyPrint


type ID = String

data GuessRequest = GReq {
    grID      :: String,
    grProgram :: String
} deriving Show

instance ToJSON GuessRequest where
    toJSON (GReq gid prog) = object ["id" .= gid, "program" .= prog]

data GuessResponse = GRes {
    grStatus    :: String,
    grValues    :: Maybe [String],
    grMessage   :: Maybe String,
    grLightning :: Maybe Bool
} deriving Show

instance FromJSON GuessResponse where
    parseJSON (Object v) = GRes                 <$>
                            v .: "status"       <*>
                            v .:? "values"      <*>
                            v .:? "message"     <*>
                            v .:? "lightning"
    parseJSON _             = mzero

guessURI :: String
guessURI = "http://icfpc2013.cloudapp.net/guess?auth=" ++ apiKey ++ "vpsH1H"

rawGuess :: GuessRequest -> IO (Maybe GuessResponse)
rawGuess g = do
    rsp <- simpleHTTP $ postRequestWithBody guessURI "text/plain" (unpack (encode g))
    code <- getResponseCode rsp
    case code of
        (2,0,0) -> decode . pack <$> getResponseBody rsp
        _       -> do
            putStrLn $ "makeGuess returned error code: " ++ (show code)
            return Nothing

runGuess :: Program -> ID -> IO ()
runGuess prog cid = rawGuess (GReq cid $ ppProgram prog "") >>= print

{-randomGuess :: Program -> IO ()
randomGuess g = do
                rid <- getRandID
                runGuess g rid-}