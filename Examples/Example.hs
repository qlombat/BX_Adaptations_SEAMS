{-# LANGUAGE FlexibleContexts,  TemplateHaskell,  TypeFamilies, OverloadedStrings #-}

{-
    This file is an example using the synchronization for BiGUL.
    This example contains 3 files :
        - ContextGenerator.hs : this file is provided by the programmer to generate a custom Context
        - rules.txt : this file contains all the rules base on the custom context. it's also provided by the programmer that use our solution
        - example.hs : this file uses our Synchronizer to execute somes Concerns in different order according to the rules and the context
-}

module Examples.Example (main1, main2) where

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

import Lib.Synchronizer
import Lib.Synchronizer.Context
import Lib.Synchronizer.RuleBase
import Lib.Synchronizer.RuleParser

import Data.ByteString.Lazy.Char8 as Char8 (pack, unpack)
import Data.Aeson

import Examples.ContextGenerator



{-
    Following lines correspond to a possible data given by AWS. For our example, we have a small set of data.
-}
instanceTypes :: [InstanceType]
instanceTypes = [
    InstanceType "t2.small" 1 2 0.04,
    InstanceType "t2.medium" 2 4 0.08,
    InstanceType "m4.4xlarge" 16 64 1.391]

securityGroups :: [SecurityGroup]
securityGroups = [SecurityGroup "sg-5a2115d26ca21" "" ["inst-5a2115d26cc1b","inst-5a2115d26cfe1","inst-5a2115d26d3d4"] []]

instances :: [Instance]
instances = [
    Instance "inst-5a2115d26cc1b" "m4.4xlarge" "ami-5a2115d26cdc9" 16 0 "sg-5a2115d26ca21" 0.714325,
    Instance "inst-5a2115d26cfe1" "t2.medium" "ami-5a2115d26d1c4" 16 0 "sg-5a2115d26ca21" 0.736548,
    Instance "inst-5a2115d26d3d4" "t2.small" "ami-5a2115d26d5d2" 16 0 "sg-5a2115d26ca21" 0.778803]

source :: Source
source = Source instances securityGroups instanceTypes


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

main1 = do
    ctx <- makeContext -- (1)
    rules <- ruleParser "Examples/rules.txt" -- (2)
    concerns <- return ([
        (Concern "Cost" costUpdate (costPlan 200)),
        (Concern "AutoScaling" autoScalingUpdate analysisAndPlanAutoScaling),
        (Concern "Redundancy" redundancyUpdate redundancyPlan),  -- (3)
        (Concern "Firewall" firewallUpdate firewallPlan)])  -- (3)
    res <- (executeSeq ctx rules Newer source concerns)  -- (4)
    putStrLn("Source updated")
    putStrLn( show (fst res)  ++ "\n")
    putStrLn("views updated")
    mapM_ (\x ->  putStrLn (show x ++ "\n")) (snd res)



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
    ctx <- makeContext -- (1)
    rules <- ruleParser "Examples/rules.txt" -- (2)
    concerns <- return ([
        (ConcernRemote "Cost" costUpdate "http://13.115.109.164/duduloma/Conflict/AWS/Scripts/analyse_and_plan_cost.php" 80 False serialize unseralizeC),
        (ConcernRemote "AutoScaling" autoScalingUpdate "http://13.115.109.164/duduloma/Conflict/AWS/Scripts/analyse_and_plan_auto_scaling.php" 80 False serialize unseralizeAS),
        (ConcernRemote "Redundancy" redundancyUpdate "http://13.115.109.164/duduloma/Conflict/AWS/Scripts/analyse_and_plan_redundancy.php" 80 False serialize unseralizeR),  -- (3)
        (ConcernRemote "Firewall" firewallUpdate "http://13.115.109.164/duduloma/Conflict/AWS/Scripts/analyse_and_plan_firewall.php" 80 False serialize unseralizeF)])  -- (3)
    res <- (executeSeq ctx rules Newer source concerns)  -- (4)
    putStrLn("Source updated")
    putStrLn(show (fst res)  ++ "\n")
    putStrLn("views updated")
    mapM_ (\x ->  putStrLn (show x ++ "\n")) (snd res)
