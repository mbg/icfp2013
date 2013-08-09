{-# LANGUAGE OverloadedStrings #-}

module AST (
    Op1(..),
    Op2(..),
    Operator(..)
) where

import Control.Monad (mzero)

import Data.Aeson

data Op1 = Not | Shl1 | Shr1 | Shr4 | Shr16
data Op2 = And | Or | Xor | Plus 

instance FromJSON Op1 where
    parseJSON (String "not")   = return Not
    parseJSON (String "shl1")  = return Shl1
    parseJSON (String "shr1")  = return Shr1
    parseJSON (String "shr4")  = return Shr4
    parseJSON (String "shr16") = return Shr16
    parseJSON _                = mzero
    
instance FromJSON Op2 where
    parseJSON (String "and")  = return And
    parseJSON (String "or")   = return Or
    parseJSON (String "xor")  = return Xor
    parseJSON (String "plus") = return Plus
    parseJSON _               = mzero

type Operator = Either Op1 Op2 