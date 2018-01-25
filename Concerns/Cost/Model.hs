{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings, DeriveAnyClass, StandaloneDeriving #-}
module Concerns.Cost.Model where

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
type Load = Double

data CInstance = CInstance Id Type Load deriving (Show, Eq)
deriving instance NFData CInstance
deriveBiGULGeneric ''CInstance
deriveJSON defaultOptions ''CInstance


data CInstanceType = CInstanceType Id TypeCost deriving (Show, Eq)
deriving instance NFData CInstanceType
deriveBiGULGeneric ''CInstanceType
deriveJSON defaultOptions ''CInstanceType


data CView = CView [CInstance] [CInstanceType] deriving (Show)
deriving instance NFData CView
deriveBiGULGeneric ''CView
deriveJSON defaultOptions ''CView

unseralizeC :: String -> CView
unseralizeC x = case (decode (Char8.pack x) :: Maybe CView) of
    Just x -> x
    Nothing -> error ("Parsing CView from json is impossible : " ++ x)
