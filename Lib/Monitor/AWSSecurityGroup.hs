{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Lib.Monitor.AWSSecurityGroup where

import Data.Aeson
import Data.String
import Data.Maybe
import Data.List as L
import GHC.Generics
import System.Process
import Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Char as Char
import Data.Time
import Data.Time.Format (formatTime)
import Concerns.SourceModel
import Control.Monad
import Lib.Monitor.Utils

--data to retrieve AWS security groups information
data AWSDescribeSecurityGroupsResponse = AWSDescribeSecurityGroupsResponse
    { securityGroups :: [AWSSecurityGroup]
    } deriving (Show, Generic)
instance FromJSON AWSDescribeSecurityGroupsResponse where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = capitalized }

data AWSSecurityGroup = AWSSecurityGroup{
    groupName :: String,
    description :: String,
    ipPermissions :: [AWSRule],
    ipPermissionsEgress :: [AWSRule]
    } deriving (Show, Generic)
instance FromJSON AWSSecurityGroup where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = capitalized }

data AWSRule = AWSRule{
    fromPort :: Maybe Int,
    toPort :: Maybe Int,
    ipRanges :: [AWSIpRange],
    ipProtocol :: String
    } deriving (Show, Generic)
instance FromJSON AWSRule where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = capitalized }


data AWSIpRange = AWSIpRange
    { cidrIp :: String
    } deriving (Show, Generic)
instance FromJSON AWSIpRange where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = capitalized }


fromAWSToSourceSecurityGroups :: AWSDescribeSecurityGroupsResponse -> [SecurityGroup]
fromAWSToSourceSecurityGroups d = Prelude.foldl (\acc o -> acc ++ [SecurityGroup (groupName o) (description o)[] (rules o)]) [] (securityGroups d)
    where
        rules :: AWSSecurityGroup -> [FirewallRule]
        rules o = L.map (fromAWSRuleToRules False) (ipPermissions o) ++ L.map (fromAWSRuleToRules True) (ipPermissionsEgress o)
        fromAWSRuleToRules :: Outbound -> AWSRule -> FirewallRule
        fromAWSRuleToRules outbound o = FirewallRule outbound (fromPort o) (toPort o) (cidrIp (L.head (ipRanges o))) (ipProtocol o)

linkInstancesToSecurityGroup :: [Instance] -> [SecurityGroup] -> [SecurityGroup]
linkInstancesToSecurityGroup [] sgs = sgs
linkInstancesToSecurityGroup ((Instance identifier _ _ _ _ sgIden _):xs) sgs = case L.find (\(SecurityGroup iden _ _ _) -> iden == sgIden ) sgs of
    Just sg@(SecurityGroup iden desc insts rules) -> linkInstancesToSecurityGroup xs ((SecurityGroup iden desc (identifier:insts) rules):(L.filter (\(SecurityGroup iden _ _ _) -> iden /= sgIden ) sgs))
    Nothing -> linkInstancesToSecurityGroup xs sgs
