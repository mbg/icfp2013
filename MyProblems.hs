{-# LANGUAGE OverloadedStrings #-}

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
    problemOps  :: [Operator]
}

instance FromJSON Problem where
    parseJSON (Object v) = Problem      <$>
                            v .: "id"   <*>
                            v .: "size" <*>
                            v .: "operators"
    parseJSON _          = mzero
    
getProblemsStr :: IO String
getProblemsStr = do
    rsp <- simpleHTTP (postRequest "http://icfpc2013.cloudapp.net/myproblems?auth=0288aN0beFycyfRn6qBOyA89Xgg8lEXzdutFgvKgvpsH1H")
    getResponseBody rsp
   
getProblems :: IO (Maybe [Problem])
getProblems = do
    rsp <- simpleHTTP (postRequest "http://icfpc2013.cloudapp.net/myproblems?auth=0288aN0beFycyfRn6qBOyA89Xgg8lEXzdutFgvKgvpsH1H")
    bdy <- pack <$> getResponseBody rsp
    return (decode bdy)
    
debugProblems :: IO ()
debugProblems = do
    mps <- getProblems
    case mps of
        (Just ps) -> putStrLn "Just ps"
        Nothing   -> putStrLn "Nothing"
