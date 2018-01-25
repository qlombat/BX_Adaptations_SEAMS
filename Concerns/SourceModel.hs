{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings, DeriveAnyClass, StandaloneDeriving #-}
module Concerns.SourceModel where

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
type Description = String
type Type = String
type Ami = String
type SecurityGroupRef = String
type InstanceRefs = [String]
type FromPort = Maybe Int
type ToPort = Maybe Int
type Ip = String
type Protocol = String
type State = Int
type Status = Int
type TypeCPUs = Int
type FirewallStatus = Int
type Load = Double
type TypeRAM = Double
type TypeCost = Double
type Outbound = Bool

{- States code of Instances
0 : pending
16 : running
32 : shutting-down
48 : terminated
64 : stopping
80 : stopped
-}

--http://docs.aws.amazon.com/fr_fr/AWSEC2/latest/APIReference/API_DescribeInstances.html
data Instance = Instance Id Type Ami State Status SecurityGroupRef Load deriving (Show, Eq, Read)
deriving instance NFData Instance
deriveBiGULGeneric ''Instance
deriveJSON defaultOptions ''Instance

-- http://docs.aws.amazon.com/fr_fr/AWSEC2/latest/APIReference/API_IpPermission.html
data FirewallRule = FirewallRule Outbound FromPort ToPort Ip Protocol deriving (Show, Eq, Read)
deriving instance NFData FirewallRule
deriveBiGULGeneric ''FirewallRule
deriveJSON defaultOptions ''FirewallRule

-- http://docs.aws.amazon.com/fr_fr/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html
-- http://docs.aws.amazon.com/fr_fr/AWSEC2/latest/APIReference/API_DeleteSecurityGroup.html
data SecurityGroup = SecurityGroup Id Description InstanceRefs [FirewallRule] deriving (Show, Read)
deriving instance NFData SecurityGroup
deriveBiGULGeneric ''SecurityGroup
deriveJSON defaultOptions ''SecurityGroup

--https://aws.amazon.com/fr/ec2/instance-types/
data InstanceType = InstanceType Id TypeCPUs TypeRAM TypeCost deriving (Show, Eq, Read)
deriving instance NFData InstanceType
deriveBiGULGeneric ''InstanceType
deriveJSON defaultOptions ''InstanceType

--http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_HostInstance.html
data Source = Source [Instance] [SecurityGroup] [InstanceType] deriving (Show, Read)
deriving instance NFData Source
deriveBiGULGeneric ''Source
deriveJSON defaultOptions ''Source

unseralizeSource :: String -> Source
unseralizeSource x = case (decode (Char8.pack x) :: Maybe Source) of
    Just x -> x
    Nothing -> error ("Parsing FView from json is impossible : " ++ x)
