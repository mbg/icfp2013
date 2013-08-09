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
import Data.Word (Word64)
import Debug.Trace
import Network.HTTP
import Text.Printf (printf)

import AST (Id, Program)
import Config (apiKey)
import PrettyPrint (ppProgram)

eval :: Either Id Program -> [Word64] -> IO (Maybe [Word64])
eval idOrProg args = (interpretResponse =<<) <$> evalRemotely request
    where
    (id, prog) = either
        (\id'   -> (Just id', Nothing                  ))
        (\prog' -> (Nothing , Just $ ppProgram prog' ""))
        idOrProg

    request = EReq id prog $ map (printf "0x%016X") args

    interpretResponse :: EvalResponse -> Maybe [Word64]
    interpretResponse (ERes "ok" mOuts _   ) = map read <$> mOuts
    interpretResponse (ERes _    _     mMsg) = traceShow mMsg Nothing

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
    code <- getResponseCode rsp
    case code of
        (2,0,0) -> decode . pack <$> getResponseBody rsp
        (4,2,9) -> do
            let time = 6000000 -- in microseconds
            putStrLn $ "trying again in " ++ show time ++ " microseconds"
            threadDelay time
            getProblems
        _       -> do
            putStrLn $ "evalRemotely returned error code: " ++ (show code)
            return Nothing

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

defaultArgs64 :: [Word64]
defaultArgs64 = [0x0000000000000000, 0x0000000000000001, 0x0000000000000002, 0xFFFFFFFFFFFFFFFF]

