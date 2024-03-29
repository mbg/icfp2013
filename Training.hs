{-# LANGUAGE OverloadedStrings #-}

module Training (
    getID,
    getRandID,
    runTraining
) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Maybe (fromJust)
import Data.Word
import Network.HTTP
import System.Random

import Config
import AST
import PrettyPrint
import Eval
import Guess
import Brute

data TrainRequest = TR {
    trSize :: Maybe Int,
    trOps  :: Maybe String
} deriving Show

instance ToJSON TrainRequest where
    toJSON (TR size ops) = object ["size" .= size, "operators" .= ops]

data TrainingProblem = TP {
    tpProgram :: String,
    tpID      :: String,
    tpSize    :: Int,
    tpOps     :: [Op]
} deriving Show

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
    code <- getResponseCode rsp
    case code of
        (2,0,0) -> decode . pack <$> getResponseBody rsp
        (4,2,9) -> do
            let time = 6000000 -- in microseconds
            putStrLn $ "trying again in " ++ show time ++ " microseconds"
            threadDelay time
            requestProblem tr
        _ -> do
            putStrLn $ "requestProblem returned error code: " ++ show code
            return Nothing

getID :: TrainRequest -> IO String
getID tr = do
        arg <- requestProblem tr
        return $ maybe "Lawl" (\x -> (tpID x)) arg

getRandID :: IO String
getRandID = getID (TR Nothing Nothing)

runTrainingWith :: TrainRequest -> IO ()
runTrainingWith tr = do
    mtp <- requestProblem tr
    case mtp of
        Nothing   -> putStrLn "Couldn't parse problem"
        (Just tp) -> do
            putStrLn $ tpProgram tp
            inputs <- inputSpread
            er <- eval (Left (tpID tp)) inputs
            case er of
                Nothing   -> putStrLn "Couldn't evaluate"
                (Just rs) -> let exps = brute (tpSize tp) (tpOps tp) defaultArgs64 rs in do
                    putStrLn "eval returned the following:"
                    mapM_ print exps
                    case exps of
                        []       -> putStrLn "no expressions!"
                        (prog:_) -> rawGuess (GReq (tpID tp) (ppProgram prog "")) >>=
                                     maybe (putStrLn "brute failed") (\res -> case res of
                                        GRes "win" _ _ _          -> putStrLn $ "correctly guessed " ++ show (tpID tp)
                                        GRes "mismatch" _  _  _   -> putStrLn $ "mismatch on " ++ show (tpID tp) ++ ", moving on"
                                        GRes "error" _ (Just e) _ -> putStrLn $ "on problem " ++ show (tpID tp) ++ " we got the errror " ++ e
                                        _                         -> putStrLn "unrecognised guess response")
inputLength = 10

runTraining :: IO ()
runTraining = runTrainingWith (TR (Just inputLength) (Just ""))

inputSpread :: IO [Word64]
inputSpread = replicateM inputLength randomIO