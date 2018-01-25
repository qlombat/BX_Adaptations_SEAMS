{-# LANGUAGE FlexibleContexts,  TemplateHaskell,  TypeFamilies, OverloadedStrings, ExistentialQuantification #-}

{-
    This file is an example using the synchronization for BiGUL.
    This example contains 3 files :
        - ContextGenerator.hs : this file is provided by the programmer to generate a custom Context
        - rules.txt : this file contains all the rules base on the custom context. it's also provided by the programmer that use our solution
        - example.hs : this file uses our Synchronizer to execute somes Concerns in different order according to the rules and the context
-}

module Examples.Experiment where

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
import Generics.BiGUL.Lib.List


import Lib.Synchronizer
import Lib.Synchronizer.Context
import Lib.Synchronizer.RuleBase
import Lib.Synchronizer.RuleParser

import Data.Map (map, fromList)
import Data.List (isInfixOf)
import Data.Char (toLower)


{-
    Following lines correspond to a possible data given by AWS. For our example, we have a small set of data.
-}
instanceTypes :: [InstanceType]
instanceTypes = [
    InstanceType "t2.nano" 1 0.5 0.0058,
    InstanceType "t2.micro" 1 1 0.0116,
    InstanceType "t2.small" 1 2 0.023,
    InstanceType "t2.medium" 2 4 0.0464,
    InstanceType "t2.large" 2 8 0.0928,
    InstanceType "t2.xlarge" 4 16 0.1856,
    InstanceType "t2.2xlarge" 8 32 0.3712]

securityGroups :: [SecurityGroup]
securityGroups = [
    SecurityGroup "CloudBx-Web1" "" ["inst-5a2115d26cc1b","inst-5a2115d26cfe1"] [FirewallRule False (Just 80) (Just 80) "0.0.0.0/0" "TCP", FirewallRule True (Just 80) (Just 80) "0.0.0.0/0" "TCP"],
    SecurityGroup "CloudBx-Web2" "" ["inst-5a2115d26h8s3","inst-5a2115d26s5d4","inst-5a2115d26z5s7","inst-5a2115d26g4f8","inst-5a2115d26u5k9"] [FirewallRule True (Just 3606) (Just 3606) "0.0.0.0/0" "TCP"],
    SecurityGroup "CloudBx-Web4" "" [] [],
    SecurityGroup "CloudBx-Web5" "" [] [],
    SecurityGroup "CloudBx-Web6" "" [] [],
    SecurityGroup "CloudBx-Web7" "" [] [],
    SecurityGroup "CloudBx-Database" "" ["inst-5a2115d26d3d4","inst-5a2115d26f5s8"] [FirewallRule False (Just 10) (Just 4000) "0.0.0.0/0" "TCP",FirewallRule False (Just 3306) (Just 3306) "0.0.0.0/0" "TCP"]]

instances :: [Instance]
instances = [
    Instance "inst-5a2115d26cc1b" "t2.large" "ami-5a2115d26cdc9" 16 0 "CloudBx-Web1" 0.714325,
    Instance "inst-5a2115d26cfe1" "t2.medium" "ami-5a2115d26d1c4" 16 0 "CloudBx-Web1" 0.736548,
    Instance "inst-5a2115d26d3d4" "t2.small" "ami-5a2115d26d5d2" 16 0 "CloudBx-Database" 0.778803,
    Instance "inst-5a2115d26f5s8" "t2.small" "ami-5a2115d26d5d2" 16 0 "CloudBx-Database" 0.778803,
    Instance "inst-5a2115d26h8s3" "t2.micro" "ami-5a2115d26d5d2" 16 0 "CloudBx-Web2" 0.1524,
    Instance "inst-5a2115d26s5d4" "t2.micro" "ami-5a2115d26d5d2" 16 0 "CloudBx-Web2" 0.16054,
    Instance "inst-5a2115d26z5s7" "t2.micro" "ami-5a2115d26d5d2" 16 0 "CloudBx-Web2" 0.2015,
    Instance "inst-5a2115d26g4f8" "t2.micro" "ami-5a2115d26d5d2" 16 0 "CloudBx-Web2" 0.0125,
    Instance "inst-5a2115d26u5k9" "t2.micro" "ami-5a2115d26d5d2" 16 0 "CloudBx-Web2" 0.1925]

source :: Source
source = Source instances securityGroups instanceTypes

{-
    Useful functions
-}

analysisAndPlanAutoScaling :: ASView -> ASView
analysisAndPlanAutoScaling v = autoScalingPlan (autoScalingAnalysis 0.3 v) v

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

saveExperiment :: String -> Rules -> Source -> Source -> [MasterView] -> IO ()
saveExperiment name rulesTriggered oldSource newSource newViews = do
    (Source instances securityGroups instanceTypes) <- return oldSource
    writeFile ("Reports/" ++ name ++ ".md") "# Report"
    appendFile ("Reports/" ++ name ++ ".md") ("\n## Source Instances (total: " ++ show (length instances) ++ ", running: " ++ show (length (Prelude.filter (\(Instance _ _ _ state status _ _) -> (state == 16 && status /= 2) || (state /= 16 && status == 1)) instances)) ++ " )\n")
    appendFile ("Reports/" ++ name ++ ".md") "```\n"
    mapM_ (\x ->  appendFile ("Reports/" ++ name ++ ".md") (show x ++ "\n")) instances
    appendFile ("Reports/" ++ name ++ ".md") "```\n"
    appendFile ("Reports/" ++ name ++ ".md") ("\n## Source firewall (" ++ show (length securityGroups) ++ ")\n")
    appendFile ("Reports/" ++ name ++ ".md") "```\n"
    mapM_ (\x ->  appendFile ("Reports/" ++ name ++ ".md") (show x ++ "\n")) securityGroups
    appendFile ("Reports/" ++ name ++ ".md") "```\n"

    (Source instances securityGroups instanceTypes) <- return newSource
    appendFile ("Reports/" ++ name ++ ".md") ("\n\n## New source Instances (total: " ++ show (length instances) ++ ", running: " ++ show (length (Prelude.filter (\(Instance _ _ _ state status _ _) -> (state == 16 && status /= 2) || (state /= 16 && status == 1)) instances)) ++ " )\n")
    appendFile ("Reports/" ++ name ++ ".md") "```\n"
    mapM_ (\x ->  appendFile ("Reports/" ++ name ++ ".md") (show x ++ "\n")) instances
    appendFile ("Reports/" ++ name ++ ".md") "```\n"
    appendFile ("Reports/" ++ name ++ ".md") ("\n## New source firewall (" ++ show (length securityGroups) ++ ")\n")
    appendFile ("Reports/" ++ name ++ ".md") "```\n"
    mapM_ (\x ->  appendFile ("Reports/" ++ name ++ ".md") (show x ++ "\n")) securityGroups
    appendFile ("Reports/" ++ name ++ ".md") "```\n"

    (Source instances securityGroups instanceTypes) <- return oldSource
    appendFile ("Reports/" ++ name ++ ".md") ("\n## Rules triggered:\n")
    appendFile ("Reports/" ++ name ++ ".md") "```\n"
    mapM_ (\x ->  appendFile ("Reports/" ++ name ++ ".md") (show x ++ "\n")) rulesTriggered
    appendFile ("Reports/" ++ name ++ ".md") "```\n"


    appendFile ("Reports/" ++ name ++ ".md") ("\n## Concerns execution order : \n")
    mapM_ (\x ->  appendFile ("Reports/" ++ name ++ ".md") ("* " ++ nameview x ++ "\n")) newViews

    return ()
    where
        nameview :: MasterView -> String
        nameview (MasterView v) = case show (v) of
            'R':'V':'i':'e':'w':_ -> "Redundancy"
            'C':'V':'i':'e':'w':_ -> "Cost"
            'F':'V':'i':'e':'w':_ -> "Firewall"
            'A':'S':'V':'i':'e':'w':_ -> "AutoScaling"
            otherwise -> "Unkwon"

{-
  No special event, just a normal day

  Rules triggered :
    (HourOfDay > 6) and (HourOfDay < 20) : Redundancy, *, Cost
-}
exp1 = do
    ctx <- return (fromList [
        ("SecurityEmergency", B False),
        ("FireEmergency", B False),
        ("HourOfDay", I 14),
        ("DayOfWeek", I 3),
        ("WeekOfYear", I 20),
        ("Month", I 5),
        ("AdvertisingRevenue", I 250),
        ("PeriodOfReduction", B False)])
    rules <- ruleParser "Examples/rules.txt" -- (2)
    rulesTriggered <- return $ evalRules ctx rules
    concerns <- return ([
        (Concern "Cost" costUpdate (costPlan 0.5)),
        (Concern "Redundancy" redundancyUpdate redundancyPlan),
        (Concern "AutoScaling" autoScalingUpdate analysisAndPlanAutoScaling),
        (Concern "Firewall" firewallUpdate firewallPlan)])  -- (3)
    res <- (executeSeq ctx rules Safety source concerns)  -- (4)
    saveExperiment "Exp1" rulesTriggered source (fst res) (snd res)

{-
    Christmas

    Rules triggered :
        ((DayOfWeek == 6) and (HourOfDay > 18)) or ((DayOfWeek == 6) or (DayOfWeek == 7)) : Redundancy, AutoScaling, *
        ((HourOfDay > 6) and (HourOfDay < 10)) or ((HourOfDay > 17) and (HourOfDay < 22)) : AutoScaling, Redundancy, Firewall, *
        (HourOfDay <= 6) and (HourOfDay >= 20) : Redundancy, Cost, Firewall, *
        PeriodOfReduction == True : Redundancy, AutoScaling, *, Cost


    Rules kept:
        ((DayOfWeek == 6) and (HourOfDay > 18)) or ((DayOfWeek == 6) or (DayOfWeek == 7)) : Redundancy, AutoScaling, *
        PeriodOfReduction == True : Redundancy, AutoScaling, *, Cost

-}
exp2 = do
    ctx <- return (fromList [
        ("SecurityEmergency", B False),
        ("FireEmergency", B False),
        ("HourOfDay", I 20),
        ("DayOfWeek", I 6),
        ("WeekOfYear", I 51),
        ("Month", I 12),
        ("AdvertisingRevenue", I 600),
        ("PeriodOfReduction", B True)])
    rules <- ruleParser "Examples/rules.txt" -- (2)
    rulesTriggered <- return $ evalRules ctx rules
    concerns <- return ([
        (Concern "Cost" costUpdate (costPlan 0.5)),
        (Concern "Redundancy" redundancyUpdate redundancyPlan),
        (Concern "AutoScaling" autoScalingUpdate analysisAndPlanAutoScaling),
        (Concern "Firewall" firewallUpdate firewallPlan)])  -- (3)
    res <- (executeSeq ctx rules Safety source concerns)  -- (4)
    saveExperiment "Exp2" rulesTriggered source (fst res) (snd res)

-- Christmas, with Security emergency
exp3 = do
    ctx <- return (fromList [
        ("SecurityEmergency", B True),
        ("FireEmergency", B False),
        ("HourOfDay", I 20),
        ("DayOfWeek", I 6),
        ("WeekOfYear", I 51),
        ("Month", I 12),
        ("AdvertisingRevenue", I 600),
        ("PeriodOfReduction", B True)])
    rules <- ruleParser "Examples/rules.txt" -- (2)
    rulesTriggered <- return $ evalRules ctx rules
    concerns <- return ([
        (Concern "Cost" costUpdate (costPlan 0.5)),
        (Concern "Redundancy" redundancyUpdate redundancyPlan),
        (Concern "AutoScaling" autoScalingUpdate analysisAndPlanAutoScaling),
        (Concern "Firewall" firewallUpdate firewallPlan)])  -- (3)
    res <- (executeSeq ctx rules Safety source concerns)  -- (4)
    saveExperiment "Exp3" rulesTriggered source (fst res) (snd res)

-- FireEmergency
exp4 = do
    ctx <- return (fromList [
        ("SecurityEmergency", B False),
        ("FireEmergency", B True),
        ("HourOfDay", I 14),
        ("DayOfWeek", I 3),
        ("WeekOfYear", I 20),
        ("Month", I 5),
        ("AdvertisingRevenue", I 250),
        ("PeriodOfReduction", B False)])
    rules <- ruleParser "Examples/rules.txt" -- (2)
    rulesTriggered <- return $ evalRules ctx rules
    concerns <- return ([
        (Concern "Cost" costUpdate (costPlan 0.5)),
        (Concern "Redundancy" redundancyUpdate redundancyPlan),
        (Concern "AutoScaling" autoScalingUpdate analysisAndPlanAutoScaling),
        (Concern "Firewall" firewallUpdate firewallPlan)])  -- (3)
    res <- (executeSeq ctx rules Safety source concerns)  -- (4)
    saveExperiment "Exp4" rulesTriggered source (fst res) (snd res)
