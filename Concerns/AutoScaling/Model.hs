{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings, DeriveAnyClass, StandaloneDeriving #-}
module Concerns.AutoScaling.Model where

import GHC.Generics
import Control.DeepSeq

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib

import Data.ByteString.Lazy.Char8 as Char8 (pack, unpack)
import Data.Aeson
import Data.Aeson.TH


type Id = String
type Type = String
type TypeCPUs = Int
type Load = Double
type TypeRAM = Double
type TypeCost = Double
type State = Int
type Status = Int
type SecurityGroupRef = String

data ASInstance = ASInstance Id Type State Status SecurityGroupRef Load deriving (Show, Eq)
deriving instance NFData ASInstance
deriveBiGULGeneric ''ASInstance
deriveJSON defaultOptions ''ASInstance

data ASInstanceType = ASInstanceType Id TypeCPUs TypeRAM TypeCost deriving (Show, Eq)
deriving instance NFData ASInstanceType
deriveBiGULGeneric ''ASInstanceType
deriveJSON defaultOptions ''ASInstanceType

data ASView = ASView [ASInstance] [ASInstanceType] deriving (Show)
deriving instance NFData ASView
deriveBiGULGeneric ''ASView
deriveJSON defaultOptions ''ASView


unseralizeAS :: String -> ASView
unseralizeAS x = case (decode (Char8.pack x) :: Maybe ASView) of
    Just x -> x
    Nothing -> error ("Parsing ASView from json is impossible : " ++ x)
