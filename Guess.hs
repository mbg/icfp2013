{-# LANGUAGE OverloadedStrings #-}

module Guess (
) where

import Control.Applicative
import Data.Aeson
import Config
import Network.HTTP
import Data.ByteString.Lazy.Char8 (pack, unpack)

data Guess = G {
	grID :: String,
	grProgram :: String
}

instance ToJSON Guess where
	toJSON (G gid prog) = object ["id" .= gid, "program" .= prog]

data GuessResponse = GR {
	grStatus :: String,
	grValues :: Maybe [String],
	grMessage :: Maybe String,
	grLightning :: Maybe Bool	
}

instance FromJSON GuessResponse where
	parseJSON (Object v) = GR 					<$>
							v .: "status" 		<*>
							v .:? "values"      <*>
							v .:? "message"		<*>
							v .:? "lightning" 

guessURI :: String
guessURI = "http://icfpc2013.cloudapp.net/guess?auth=" ++ apiKey ++ "vpsH1H"

makeGuess :: Guess -> IO (Maybe GuessResponse)
makeGuess g = do
	rsp <- simpleHTTP $ postRequestWithBody guessURI "text/plain" (unpack (encode g))
	bdy <- pack <$> getResponseBody rsp
	return (decode bdy)
