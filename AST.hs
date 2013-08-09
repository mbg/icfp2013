{-# LANGUAGE OverloadedStrings #-}

module AST (
    Id(..),
    Program(..),
    Expr(..),
    Op1(..),
    Op2(..)
) where

import Control.Monad (mzero)

import Data.Aeson

type Id = String

data Program = Lambda Id Expr

data Expr = Zero
          | One
          | Id Id
          | If Expr Expr Expr
          | Fold Expr Expr Id Id Expr
          | UnaryOp Op1 Expr
          | BinaryOp Op2 Expr Expr

data Op1 = Not | Shl1 | Shr1 | Shr4 | Shr16 deriving Show
data Op2 = And | Or | Xor | Plus            deriving Show
