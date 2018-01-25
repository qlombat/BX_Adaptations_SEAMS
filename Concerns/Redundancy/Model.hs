{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings, DeriveAnyClass, StandaloneDeriving #-}
module Concerns.Redundancy.Model where

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
type TypeRAM = Double
type TypeCost = Double
type InstanceRefs = [String]
type SecurityGroupRef = String
type State = Int
type Status = Int


data RInstance = RInstance Id Type State Status SecurityGroupRef deriving (Show, Eq)
deriving instance NFData RInstance
deriveBiGULGeneric ''RInstance
deriveJSON defaultOptions ''RInstance

data RSecurityGroup = RSecurityGroup Id InstanceRefs deriving (Show)
deriving instance NFData RSecurityGroup
deriveBiGULGeneric ''RSecurityGroup
deriveJSON defaultOptions ''RSecurityGroup

data RInstanceType = RInstanceType Id deriving (Show, Eq)
deriving instance NFData RInstanceType
deriveBiGULGeneric ''RInstanceType
deriveJSON defaultOptions ''RInstanceType

data RView = RView [RInstance] [RSecurityGroup] [RInstanceType] deriving (Show)
deriving instance NFData RView
deriveBiGULGeneric ''RView
deriveJSON defaultOptions ''RView

unseralizeR :: String -> RView
unseralizeR x = case (decode (Char8.pack x) :: Maybe RView) of
    Just x -> x
    Nothing -> error ("Parsing RView from json is impossible : " ++ x)
