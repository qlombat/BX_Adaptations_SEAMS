{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings, DeriveAnyClass, StandaloneDeriving #-}
module Concerns.Firewall.Model where

import GHC.Generics
import Control.DeepSeq

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib

import Data.ByteString.Lazy.Char8 as Char8 (pack, unpack)
import Data.Aeson
import Data.Aeson.TH

type Outbound = Bool
type FromPort = Maybe Int
type ToPort = Maybe Int
type Ip = String
type Protocol = String
type Id = String

data FRule = FRule Outbound FromPort ToPort Ip Protocol deriving (Show, Eq)
deriving instance NFData FRule
deriveBiGULGeneric ''FRule
deriveJSON defaultOptions ''FRule


data FSecurityGroup = FSecurityGroup Id [FRule] deriving (Show)
deriving instance NFData FSecurityGroup
deriveBiGULGeneric ''FSecurityGroup
deriveJSON defaultOptions ''FSecurityGroup


data FView = FView [FSecurityGroup] deriving (Show)
deriving instance NFData FView
deriveBiGULGeneric ''FView
deriveJSON defaultOptions ''FView

unseralizeF :: String -> FView
unseralizeF x = case (decode (Char8.pack x) :: Maybe FView) of
    Just x -> x
    Nothing -> error ("Parsing FView from json is impossible : " ++ x)
