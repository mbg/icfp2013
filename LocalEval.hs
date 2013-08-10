
module LocalEval (
    localEval,
    vectorise,
    evalUnary,
    evalBinary
) where

import Data.Bits
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Word

import AST

vectorise' :: Int -> Word64 -> [Word64]
vectorise' 8 v = []
vectorise' n v = (v .&. 0x00000000000000FF) : vectorise' (n+1) (shiftR v 8)

vectorise :: Word64 -> [Word64]
vectorise = vectorise' 0

evalUnary :: Op1 -> Word64 -> Word64
evalUnary Not   = complement
evalUnary Shl1  = flip shiftL 1
evalUnary Shr1  = flip shiftR 1
evalUnary Shr4  = flip shiftR 4
evalUnary Shr16 = flip shiftR 16

evalBinary :: Op2 -> Word64 -> Word64 -> Word64
evalBinary And  = (.&.)
evalBinary Or   = (.|.)
evalBinary Xor  = xor
evalBinary Plus = (+)

evalExpr :: M.Map Id Word64 -> Expr -> Word64
evalExpr m Zero    = 0
evalExpr m One     = 1
evalExpr m (Id id) = fromJust $ M.lookup id m
evalExpr m (IfZero c t f)
    | evalExpr m c == 0 = evalExpr m t
    | otherwise         = evalExpr m f
evalExpr m (Fold e0 e1 y z e) = let
    e0v = vectorise (evalExpr m e0)
    e1v = evalExpr m e1 in foldl (\yv zv -> evalExpr (M.insert y yv (M.insert z zv m)) e) e1v e0v
evalExpr m (UnaryOp op e)     = evalUnary op (evalExpr m e)
evalExpr m (BinaryOp op e e') = evalBinary op (evalExpr m e) (evalExpr m e')

localEval :: Program -> Word64 -> Word64
localEval (Lambda id e) v = evalExpr (M.singleton id v) e

testProgram :: Program
testProgram = Lambda "x" (Fold (Id "x") Zero "y" "z" (BinaryOp Or (Id "y") (Id "z")))

solution :: Word64
solution = 0x0000000000000011 .|.
 (0x0000000000000022 .|.
 (0x0000000000000033 .|.
 (0x0000000000000044 .|.
 (0x0000000000000055 .|.
 (0x0000000000000066 .|.
 (0x0000000000000077 .|.
 (0x0000000000000088 .|.
 (0x0000000000000000))))))))
