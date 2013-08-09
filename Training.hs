{-# LANGUAGE OverloadedStrings #-}

module Training (
) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Network.HTTP

import Config
import AST
import PrettyPrint
import Eval
import Guess

data TrainRequest = TR {
    trSize :: Maybe Int,
    trOps  :: Maybe String
}

instance ToJSON TrainRequest where
    toJSON (TR size ops) = object ["size" .= size, "operators" .= ops]

data TrainingProblem = TP {
    tpProgram :: String,
    tpID      :: String,
    tpSize    :: Int,
    tpOps     :: [String]
}

instance FromJSON TrainingProblem where
    parseJSON (Object v) = TP                <$>
                            v .: "challenge" <*>
                            v .: "id"        <*>
                            v .: "size"      <*>
                            v .: "operators"
    parseJSON _          = mzero

trainURI :: String
trainURI = "http://icfpc2013.cloudapp.net/train?auth=" ++ apiKey ++ "vpsH1H"
    
requestProblem :: TrainRequest -> IO (Maybe TrainingProblem)
requestProblem tr = do
    rsp <- simpleHTTP $ postRequestWithBody trainURI "text/plain" (unpack (encode tr))
    bdy <- pack <$> getResponseBody rsp
    return (decode bdy)

runTraining :: IO ()
runTraining = requestProblem (TR Nothing Nothing) >>= \mtp -> case mtp of
    Nothing   -> putStrLn "Unable to obtain training program."
    (Just tp) -> putStrLn (tpProgram tp)
