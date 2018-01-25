{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Lib.Monitor(monitor) where

import System.Process
import Data.ByteString.Lazy.Char8 as Char8
import Data.Maybe
import Data.Aeson

import Lib.Monitor.AWSInstance as AWSInstance
import Lib.Monitor.AWSSecurityGroup as AWSSecurityGroup
import Lib.Monitor.Utils
import Concerns.SourceModel

instanceTypes :: [InstanceType]
instanceTypes = [
    InstanceType "t2.nano" 1 0.5 0.0058,
    InstanceType "t2.micro" 1 1 0.0116,
    InstanceType "t2.small" 1 2 0.023,
    InstanceType "t2.medium" 2 4 0.0464,
    InstanceType "t2.large" 2 8 0.0928,
    InstanceType "t2.xlarge" 4 16 0.1856,
    InstanceType "t2.2xlarge" 8 32 0.3712]


monitor :: String -> String -> String -> IO Source
monitor access secret region = do
    -- configure AWS
    _ <- readProcess "aws" ["configure"] (access ++ "\n" ++ secret ++ "\n" ++ region ++ "\n" ++ "json\n")

    -- retrieve instances
    resInstances <- readProcess "aws" ["ec2", "describe-instances", "--filter", "Name=tag:BiGUL,Values=CloudBx"] []
    jsonInstances <- return $ fromJust (decode (Char8.pack resInstances) :: Maybe AWSDescribeInstancesResponse)
    instances <- updateLoadOfInstances (fromAWSToSourceInstances jsonInstances)
    --mapM_ (\x ->  Prelude.putStrLn (show x ++ "\n")) (instances)
    --retrieve security Groups
    resSG <- readProcess "aws" ["ec2", "describe-security-groups", "--filter", "Name=tag:BiGUL,Values=CloudBx"] []
    jsonSG <- return $ fromJust (decode (Char8.pack resSG) :: Maybe AWSDescribeSecurityGroupsResponse)
    securityGroups <- return (linkInstancesToSecurityGroup instances (fromAWSToSourceSecurityGroups jsonSG))

    return $ Source instances securityGroups instanceTypes
