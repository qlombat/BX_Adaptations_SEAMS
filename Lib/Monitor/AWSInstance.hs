{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Lib.Monitor.AWSInstance where

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

-- Date to retrieve AWS instances information
data AWSDescribeInstancesResponse = AWSDescribeInstancesResponse
    { reservations :: [AWSReservation]
    } deriving (Show, Generic)
instance FromJSON AWSDescribeInstancesResponse where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = capitalized }

data AWSReservation = AWSReservation
    { instances :: [AWSInstance]
    } deriving (Show, Generic)
instance FromJSON AWSReservation where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = capitalized }

data AWSInstance = AWSInstance
    { instanceId :: String,
    instanceType :: String,
    state :: AWSState,
    securityGroups :: [AWSInstanceSecurityGroup],
    imageId :: String
    } deriving (Show, Generic)
instance FromJSON AWSInstance where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = capitalized }

data AWSState = AWSState
    { code :: Int,
    name :: String
    } deriving (Show, Generic)
instance FromJSON AWSState where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = capitalized }

data AWSInstanceSecurityGroup = AWSInstanceSecurityGroup
    { groupName :: String,
    groupId :: String
    } deriving (Show, Generic)
instance FromJSON AWSInstanceSecurityGroup where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = capitalized }

data AWSGetMetricsResponse = AWSGetMetricsResponse
    { datapoints :: [AWSDatapoint]
    } deriving (Show, Generic)
instance FromJSON AWSGetMetricsResponse where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = capitalized }

data AWSDatapoint = AWSDatapoint
    { timestamp :: String,
    average :: Double,
    unit :: String
    } deriving (Show, Generic, Eq)
instance FromJSON AWSDatapoint where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = capitalized }

fromAWSToSourceInstances :: AWSDescribeInstancesResponse -> [Instance]
fromAWSToSourceInstances d = Prelude.foldl (\acc o -> acc ++ (createInstance o) ) [] (reservations d)
    where
        createInstance :: AWSReservation -> [Instance]
        createInstance o = L.map (\inst -> Instance (instanceId inst) (instanceType inst) (imageId inst) (code (state inst)) 0 (instSg inst) 0) (instances o)

        instSg :: AWSInstance -> String
        instSg o    | L.length (securityGroups (o)) > 0 = (groupName (L.head (securityGroups (o))))
                    | otherwise = ""

updateLoadOfInstances :: [Instance] -> IO [Instance]
updateLoadOfInstances instances = do
    res <- foldM fn [] instances
    return res
    where
        fn :: [Instance] -> Instance -> IO [Instance]
        fn acc (Instance identifier insttype ami state status sg _)
            | state == 16 = do
                currentTime <- getCurrentTime
                timeEnd <- getCurrentTime
                timeStart <- return (addUTCTime (-3600) currentTime)
                r <- readProcess "aws" ["cloudwatch","get-metric-statistics", "--namespace", "AWS/EC2", "--metric-name","CPUUtilization", "--statistics","Average", "--dimensions", "Name=InstanceId,Value=" ++ identifier, "--start-time", (iso8601 timeStart), "--end-time", (iso8601 timeEnd), "--period","300"] []
                json <- return $ fromJust (decode (Char8.pack r) :: Maybe AWSGetMetricsResponse)
                --Prelude.putStrLn ("start time : " ++ (show timeStart))
                --Prelude.putStrLn ("stop time : " ++ (show timeEnd))
                --Prelude.putStrLn (show json)
                mostRecent <- return (getMostRecent (datapoints json))
                return (case mostRecent of
                    Just x -> (acc ++ [Instance identifier insttype ami state status sg ((average x)/100)])
                    Nothing -> (acc ++ [Instance identifier insttype ami state status sg 0]))
            | otherwise = do
                return (acc ++ [Instance identifier insttype ami state status sg 0])
        getMostRecent :: [AWSDatapoint] -> Maybe AWSDatapoint
        getMostRecent x = L.foldl (\acc o -> if acc == Nothing then Just o else if (parseISO8601 (timestamp o)) > (parseISO8601 (timestamp (fromJust acc))) then Just o else acc) Nothing x
