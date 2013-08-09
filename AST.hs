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

data Program = Lambda Id Expr

data Expr = Zero
          | One
          | Id Id
          | IfZero Expr Expr Expr
          | Fold Expr Expr Id Id Expr
          | UnaryOp Op1 Expr
          | BinaryOp Op2 Expr Expr

data Op1 = Not | Shl1 | Shr1 | Shr4 | Shr16            deriving Show
data Op2 = And | Or | Xor | Plus                       deriving Show
data Op = Op1 Op1 | Op2 Op2 | OIFZero | OTFold | OFold deriving Show

instance ToJSON Op1 where
    toJSON Not   = String "not"
    toJSON Shl1  = String "shl1"
    toJSON Shr1  = String "shr1"
    toJSON Shr4  = String "shr4"
    toJSON Shr16 = String "shr16"
instance FromJSON Op1 where
    parseJSON (String "not"  ) = return Not
    parseJSON (String "shl1" ) = return Shl1
    parseJSON (String "shr1" ) = return Shr1
    parseJSON (String "shr4" ) = return Shr4
    parseJSON (String "shr16") = return Shr16
    parseJSON _                = mzero

instance ToJSON Op2 where
    toJSON And  = String "and"
    toJSON Or   = String "or"
    toJSON Xor  = String "xor"
    toJSON Plus = String "plus"
instance FromJSON Op2 where
    parseJSON (String "and" ) = return And
    parseJSON (String "or"  ) = return Or
    parseJSON (String "xor" ) = return Xor
    parseJSON (String "plus") = return Plus
    parseJSON _               = mzero

instance ToJSON Op where
    toJSON (Op1 op1) = toJSON op1
    toJSON (Op2 op2) = toJSON op2
    toJSON OIFZero   = String "if0"
    toJSON OTFold    = String "tfold"
    toJSON OFold     = String "fold"
instance FromJSON Op where
    parseJSON (String "if0")   = return OIFZero
    parseJSON (String "tfold") = return OTFold
    parseJSON (String "fold")  = return OFold
    parseJSON other            = (Op1 <$> parseJSON other) <|>
                                 (Op2 <$> parseJSON other)