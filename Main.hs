import Control.Monad (forM_, replicateM)
import Data.List (intersect, sortBy)
import Data.Ord (comparing)
import Data.Word
import System.Random

import AST
import Brute
import Eval
import Guess
import MyProblems
import Training
import PrettyPrint


training = False
debug = True
inputLength = 5

main = if training then runTraining else do
    (Just problems) <- getProblems
    let okProblems = sortBy (comparing problemSize) $ filter noDifficultOps problems
    if debug then solveProblem (head okProblems)
             else mapM_ solveProblem okProblems

solveProblem :: Problem -> IO ()
solveProblem (Problem _ _ _ (Just True) _) = return ()
solveProblem (Problem iden size ops _ _) = do
        inputs   <- inputSpread
        mOutputs <- eval (Left iden) inputs
        case mOutputs of
            Nothing      -> putStrLn "eval failed"
            Just outputs -> case brute size ops inputs outputs of
                []       -> return () -- couldn't find any solutions
                (prog:_) -> rawGuess (GReq iden (ppProgram prog "")) >>= maybe (putStrLn "brute failed") (\res -> case res of
                    GRes "win" _ _ _          -> putStrLn $ "correctly guessed " ++ show iden
                    GRes "mismatch" _  _  _   -> putStrLn $ "mismatch on " ++ show iden ++ ", moving on"
                    GRes "error" _ (Just e) _ -> putStrLn $ "on problem " ++ show iden ++ " we got the errror " ++ e
                    _                         -> putStrLn "unrecognised guess response")

inputSpread :: IO [Word64]
inputSpread = replicateM inputLength randomIO

noDifficultOps :: Problem -> Bool
noDifficultOps (Problem _ _ ops _ _) = null $ intersect [OIfZero, OTFold, OFold, Bonus] ops