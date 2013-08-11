{-# LANGUAGE OverloadedStrings #-}

module AST (
    Id(..),
    Program(..),
    Expr(..),
    Op(..),
    Op1(..),
    Op2(..)
) where

import Control.Applicative ((<$>), (<|>))
import Control.Monad (mzero)

import Data.Aeson

type Id = String

data Program = Lambda Id Expr deriving Show

data Expr = Zero
          | One
          | Id Id
          | IfZero Expr Expr Expr
          | Fold Expr Expr Id Id Expr
          | UnaryOp Op1 Expr
          | BinaryOp Op2 Expr Expr
    deriving Show

data Op1 = Not | Shl1 | Shr1 | Shr4 | Shr16                    deriving Show
data Op2 = And | Or | Xor | Plus                               deriving Show
data Op = Op1 Op1 | Op2 Op2 | OIfZero | OTFold | OFold | Bonus deriving Show

instance FromJSON Op1 where
    parseJSON (String "not"  ) = return Not
    parseJSON (String "shl1" ) = return Shl1
    parseJSON (String "shr1" ) = return Shr1
    parseJSON (String "shr4" ) = return Shr4
    parseJSON (String "shr16") = return Shr16
    parseJSON _                = mzero

instance FromJSON Op2 where
    parseJSON (String "and" ) = return And
    parseJSON (String "or"  ) = return Or
    parseJSON (String "xor" ) = return Xor
    parseJSON (String "plus") = return Plus
    parseJSON _               = mzero

instance FromJSON Op where
    parseJSON (String "if0")   = return OIfZero
    parseJSON (String "tfold") = return OTFold
    parseJSON (String "fold")  = return OFold
    parseJSON (String "bonus") = return Bonus
    parseJSON other            = (Op1 <$> parseJSON other) <|>
                                 (Op2 <$> parseJSON other)