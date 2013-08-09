{-# LANGUAGE OverloadedStrings #-}

module Eval (
    evalRemotely,
    evalString,
    defaultArgs,
    EvalRequest(..),
    EvalResponse(..)
) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Maybe (fromJust)
import Network.HTTP

import Config
import PrettyPrint 

data EvalRequest = EReq {
    ereqID      :: Maybe String,
    ereqProgram :: Maybe String,
    ereqArgs    :: [String]
}

instance ToJSON EvalRequest where
    toJSON (EReq id prg args) = object ["id" .= id, "program" .= prg, "arguments" .= args]

data EvalResponse = ERes {
    eresStatus :: String,
    eresOuts   :: Maybe [String],
    eresMsg    :: Maybe String
}

instance FromJSON EvalResponse where
    parseJSON (Object v) = ERes <$> 
                            v .: "status"   <*>
                            v .:? "outputs" <*>
                            v .:? "message"

evalURI :: String
evalURI = "http://icfpc2013.cloudapp.net/eval?auth=" ++ apiKey ++ "vpsH1H"
                            
evalRemotely :: EvalRequest -> IO (Maybe EvalResponse) 
evalRemotely req = do
    rsp <- simpleHTTP $ postRequestWithBody evalURI "text/plain" (unpack (encode req))
    bdy <- pack <$> getResponseBody rsp
    return (decode bdy)
    
evalString :: String -> IO ()
evalString str = do
    mer <- evalRemotely $ EReq Nothing (Just str) defaultArgs
    case mer of
        Nothing   -> putStrLn "Unable to evaluate string."
        (Just er) -> case (eresStatus er) of
            "ok"    -> mapM_ putStrLn (fromJust $ eresOuts er)
            "error" -> putStrLn $ "error: " ++ (fromJust $ eresMsg er) 

defaultArgs :: [String]
defaultArgs = ["0x0000000000000000", "0x0000000000000001", "0x0000000000000002", "0xFFFFFFFFFFFFFFFF"]

