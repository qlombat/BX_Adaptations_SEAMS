{-# LANGUAGE FlexibleContexts,  TemplateHaskell,  TypeFamilies, OverloadedStrings #-}

{-
    This file is an example using the synchronization for BiGUL.
    This example contains 3 files :
        - ContextGenerator.hs : this file is provided by the programmer to generate a custom Context
        - rules.txt : this file contains all the rules base on the custom context. it's also provided by the programmer that use our solution
        - example.hs : this file uses our Synchronizer to execute somes Concerns in different order according to the rules and the context
-}

module Examples.ExampleAws (main1, main2) where

import Concerns.SourceModel

import Concerns.AutoScaling
import Concerns.AutoScaling.Analysis
import Concerns.AutoScaling.Plan
import Concerns.AutoScaling.Model

import Concerns.Redundancy
import Concerns.Redundancy.Analysis
import Concerns.Redundancy.Plan
import Concerns.Redundancy.Model

import Concerns.Cost
import Concerns.Cost.Analysis
import Concerns.Cost.Plan
import Concerns.Cost.Model

import Concerns.Firewall
import Concerns.Firewall.Analysis
import Concerns.Firewall.Plan
import Concerns.Firewall.Model

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib

import Data.ByteString.Lazy.Char8 as Char8 (pack, unpack)
import Data.Aeson

import Lib.Synchronizer
import Lib.Synchronizer.Context
import Lib.Synchronizer.RuleBase
import Lib.Synchronizer.RuleParser
import Lib.Monitor
import Lib.Executor


import Examples.ContextGenerator


{-
    Following lines correspond to a possible data given by AWS. For our example, we have a small set of data.
-}
instanceTypes :: [InstanceType]
instanceTypes = [
    InstanceType "t2.small" 1 2 0.04,
    InstanceType "t2.medium" 2 4 0.08,
    InstanceType "m4.4xlarge" 16 64 1.391]
{-
    Useful functions
-}

analysisAndPlanAutoScaling :: ASView -> ASView
analysisAndPlanAutoScaling v = autoScalingPlan (autoScalingAnalysis 0.1 v) v


{-
    Example of using the synchronizer (Without webservices)
        1: Creation of the context with the function given by the ContextGenerator file
        2: Definition of the rules from the files rules.txt
        3: Definition of our concerns
        4: Execetion of our concerns

    After that we show the updated source and each updated view

    Notes :
        in this case we use "Concern" type to define our concern. It's mean that we know all the analysis and planning step localy.
        For an example where each concern is a webservice see main2
-}

main1auto = do
    access <- return "AKIAI5ERKY5DQO5AJSFQ"
    secret <- return "951u9XGCtDEdXWBpsfM/bu1GC126MqgjzRHJ7U2A"
    region <- return "ap-northeast-1"
    keypair <- return "quentin.pub"
    image <- return "ami-bec974d8"
    lb <- return "BiGUL-CloudBx"


    putStrLn("\x1b[36mRetrieving current data from aws ec2 ...\x1b[0m ")
    source <- monitor access secret region
    putStrLn("\x1b[36mGeneration of the context ...\x1b[0m ")
    ctx <- makeContext -- (1)
    putStrLn("\x1b[36mParsing of the rules ...\x1b[0m ")
    rules <- ruleParser "Examples/rules.txt" -- (2)
    putStrLn("\x1b[36mGeneration of the new infrastructure ...\x1b[0m ")
    concerns <- return ([
        (Concern "Cost" costUpdate (costPlan 200)),
        (Concern "AutoScaling" autoScalingUpdate analysisAndPlanAutoScaling),
        (Concern "Redundancy" redundancyUpdate redundancyPlan),
        (Concern "Firewall" firewallUpdate firewallPlan)])  -- (3)
    res <- (executeSeq ctx rules Last source concerns)  -- (4)
    putStrLn("\x1b[36mExecution in progress ...\x1b[0m ")
    _ <- executeAWS access secret region keypair image lb (fst res)
    putStrLn("\x1b[32m" ++ "Execution finished !\x1b[0m ")
    return ()


main1 = do
    putStrLn("We need some information about your AWS infrastructure:")
    putStr(" - AWS Access Key ID : ")
    access <- getLine
    putStr(" - AWS Secret Access Key : ")
    secret <- getLine
    putStr(" - Default region name : ")
    region <- getLine
    putStr(" - keypair name : ")
    keypair <- getLine
    putStr(" - Default image to use : ")
    image <- getLine
    putStr(" - web load balancer : ")
    lb <- getLine


    putStrLn("\x1b[36mRetrieving current data from aws ec2 ...\x1b[0m")
    source <- monitor access secret region
    putStrLn("\x1b[36mGeneration of the context ...\x1b[0m")
    ctx <- makeContext -- (1)
    putStrLn("\x1b[36mParsing of the rules ...\x1b[0m")
    rules <- ruleParser "Examples/rules.txt" -- (2)
    putStrLn("\x1b[36mGeneration of the new infrastructure ...\x1b[0m")
    concerns <- return ([
        (Concern "Cost" costUpdate (costPlan 200)),
        (Concern "AutoScaling" autoScalingUpdate analysisAndPlanAutoScaling),
        (Concern "Redundancy" redundancyUpdate redundancyPlan),
        (Concern "Firewall" firewallUpdate firewallPlan)])  -- (3)
    res <- (executeSeq ctx rules Last source concerns)  -- (4)
    putStrLn("\x1b[36mExecution in progress ...\x1b[0m")
    _ <- executeAWS access secret region keypair image lb (fst res)
    putStrLn("\x1b[32m" ++ "Execution finished !\x1b[0m ")
    return ()



{-
    Serialization of our views
-}

serialize :: ToJSON a => a -> String
serialize x = Char8.unpack $ encode x

{-
    Example of using the synchronizer (With webservices)
        1: Creation of the context with the function given by the ContextGenerator file
        2: Definition of the rules from the files rules.txt
        3: Definition of our concerns
        4: Execetion of our concerns

    After that we show the updated source and each updated view

    Notes :
        in this case we use "ConcernRemote" type to define our concern. It's mean that the analysis and planning step of each concern is made by a webservice.
        That's why we have to define an url, a port and a boolean
        For an example where each analysis And planning step is known localy see main1
-}
main2 = do
    putStrLn("We need some information about your AWS infrastructure:")
    putStr(" - AWS Access Key ID : ")
    access <- getLine
    putStr(" - AWS Secret Access Key : ")
    secret <- getLine
    putStr(" - Default region name : ")
    region <- getLine
    putStr(" - keypair name : ")
    keypair <- getLine
    putStr(" - Default image to use : ")
    image <- getLine
    putStr(" - web load balancer : ")
    lb <- getLine


    putStrLn("\x1b[36mRetrieving current data from aws ec2 ...\x1b[0m")
    source <- monitor access secret region
    putStrLn("\x1b[36mGeneration of the context ...\x1b[0m")
    ctx <- makeContext -- (1)
    putStrLn("\x1b[36mParsing of the rules ...\x1b[0m")
    rules <- ruleParser "Examples/rules.txt" -- (2)
    putStrLn("\x1b[36mGeneration of the new infrastructure ...\x1b[0m")
    concerns <- return ([
        (ConcernRemote "Cost" costUpdate "https://www.lapromessedhelene.be/clone" 443 True serialize unseralizeC),
        (ConcernRemote "AutoScaling" autoScalingUpdate "https://www.lapromessedhelene.be/clone" 443 True serialize unseralizeAS),
        (ConcernRemote "Redundancy" redundancyUpdate "https://www.lapromessedhelene.be/clone" 443 True serialize unseralizeR),  -- (3)
        (ConcernRemote "Firewall" firewallUpdate "https://www.lapromessedhelene.be/clone" 443 True serialize unseralizeF)])  -- (3)
    res <- (executeSeq ctx rules Last source concerns)  -- (4)
    putStrLn("\x1b[36mExecution in progress ...\x1b[0m")
    _ <- executeAWS access secret region keypair image lb (fst res)
    putStrLn("\x1b[32m" ++ "Execution finished !\x1b[0m")
    return ()
