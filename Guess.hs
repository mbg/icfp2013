{-# LANGUAGE OverloadedStrings #-}

module Guess (
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

data Guess = G {
    grID      :: String,
    grProgram :: String
}

instance ToJSON Guess where
    toJSON (G gid prog) = object ["id" .= gid, "program" .= prog]

data GuessResponse = GR {
    grStatus    :: String,
    grValues    :: Maybe [String],
    grMessage   :: Maybe String,
    grLightning :: Maybe Bool
}

instance FromJSON GuessResponse where
    parseJSON (Object v) = GR                   <$>
                            v .: "status"       <*>
                            v .:? "values"      <*>
                            v .:? "message"     <*>
                            v .:? "lightning"
    parseJSON _             = mzero

guessURI :: String
guessURI = "http://icfpc2013.cloudapp.net/guess?auth=" ++ apiKey ++ "vpsH1H"

makeGuess :: Guess -> IO (Maybe GuessResponse)
makeGuess g = do
    rsp <- simpleHTTP $ postRequestWithBody guessURI "text/plain" (unpack (encode g))
    getResponseCode rsp  >>= \x -> case x of
        (2,0,0) -> decode . pack <$> getResponseBody rsp
        code    ->  do
                putStrLn $ "makeGuess returned error code: " ++ (show code)
                return Nothing


makeGuessOn :: Guess -> IO ()
makeGuessOn g = do
    agr <- makeGuess g
    case agr of
        Nothing -> putStrLn "No Response"
        (Just gr) -> do
                     putStrLn (grStatus gr)
                     case (grMessage gr) of
                        (Just msg) -> putStrLn msg
                        Nothing -> putStrLn "No Message"
                     case (grValues gr) of
                        (Just val) -> mapM_ putStrLn val
                        Nothing -> putStrLn "No Values"

runGuess :: Program -> ID -> IO ()
runGuess prog cid = makeGuessOn (G cid $(ppProgram prog) "")
