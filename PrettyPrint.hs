
module PrettyPrint (
    ppProgram
) where

import Data.Char (toLower)

import AST

space :: ShowS
space = showChar ' '

ppInParens :: ShowS -> ShowS
ppInParens f = showChar '(' . f . showChar ')'

ppProgram :: Program -> ShowS
ppProgram (Lambda id e) = ppInParens $
    showString "lambda " .
    ppInParens (showString id) .
    space .
    ppExpr e
    
ppExpr :: Expr -> ShowS
ppExpr Zero       = showChar '0'
ppExpr One        = showChar '1'
ppExpr (Id id)    = showString id
ppExpr (IfZero c t f) = ppInParens $
    showString "if0 " .
    ppExpr c . space .
    ppExpr t . space .
    ppExpr f
ppExpr (Fold e0 e1 x y e2) = ppInParens $
    showString "fold " .
    ppExpr e0 . space .
    ppExpr e1 . space .
    ppInParens ( 
        showString "lambda " .
        ppInParens (showString x . space . showString y) .
        space .
        ppExpr e2
    )
ppExpr (UnaryOp op e0) = ppInParens $
    ppUnary op . space .
    ppExpr e0
ppExpr (BinaryOp op e0 e1) = ppInParens $
    ppBinary op . space .
    ppExpr e0 . space .
    ppExpr e1
    
ppUnary :: Op1 -> ShowS
ppUnary = showString . map toLower . show

ppBinary :: Op2 -> ShowS
ppBinary = showString . map toLower . show