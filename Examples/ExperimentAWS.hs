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
import Lib.Monitor
import Lib.Executor


import Data.Map (map, fromList)
import Data.List (isInfixOf)
import Data.Char (toLower)
import Data.Time
import Data.Time.Format (formatTime)



{-
    Useful functions
-}

analysisAndPlanAutoScaling :: ASView -> ASView
analysisAndPlanAutoScaling v = autoScalingPlan (autoScalingAnalysis 0.5 v) v

saveExperiment :: String -> Rules -> Source -> Source -> [MasterView] -> IO ()
saveExperiment name rulesTriggered oldSource newSource newViews = do
    cTime <- getCurrentTime
    time <- return $ (formatTime defaultTimeLocale "%H-%M-%S" cTime)
    (Source instances securityGroups instanceTypes) <- return oldSource
    writeFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") "# Report"
    appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") ("\n## Source Instances (total: " ++ show (length instances) ++ ", running: " ++ show (length (Prelude.filter (\(Instance _ _ _ state status _ _) -> (state == 16 && status /= 2) || (state /= 16 && status == 1)) instances)) ++ " )\n")
    appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") "```\n"
    mapM_ (\x ->  appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") (show x ++ "\n")) instances
    appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") "```\n"
    appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") ("\n## Source firewall (" ++ show (length securityGroups) ++ ")\n")
    appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") "```\n"
    mapM_ (\x ->  appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") (show x ++ "\n")) securityGroups
    appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") "```\n"

    (Source instances securityGroups instanceTypes) <- return newSource
    appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") ("\n\n## New source Instances (total: " ++ show (length instances) ++ ", running: " ++ show (length (Prelude.filter (\(Instance _ _ _ state status _ _) -> (state == 16 && status /= 2) || (state /= 16 && status == 1)) instances)) ++ " )\n")
    appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") "```\n"
    mapM_ (\x ->  appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") (show x ++ "\n")) instances
    appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") "```\n"
    appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") ("\n## New source firewall (" ++ show (length securityGroups) ++ ")\n")
    appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") "```\n"
    mapM_ (\x ->  appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") (show x ++ "\n")) securityGroups
    appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") "```\n"

    (Source instances securityGroups instanceTypes) <- return oldSource
    appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") ("\n## Rules triggered:\n")
    appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") "```\n"
    mapM_ (\x ->  appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") (show x ++ "\n")) rulesTriggered
    appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") "```\n"


    appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") ("\n## Concerns execution order : \n")
    mapM_ (\x ->  appendFile ("Reports/" ++ name ++ "-" ++ time ++ ".md") ("* " ++ nameview x ++ "\n```\n" ++ show x ++ "\n```\n")) newViews

    return ()
    where
        nameview :: MasterView -> String
        nameview (MasterView v) = case show (v) of
            'R':'V':'i':'e':'w':_ -> "Redundancy"
            'C':'V':'i':'e':'w':_ -> "Cost"
            'F':'V':'i':'e':'w':_ -> "Firewall"
            'A':'S':'V':'i':'e':'w':_ -> "AutoScaling"
            otherwise -> "Unkwon"


filterDatabase :: BiGUL Source Source
filterDatabase = $(rearrS [| \(Source insts sg t) -> (insts, sg, t)|])$
                    $(rearrV [| \(Source insts sg t) -> (insts, sg, t)|])$
                    $(update [p|(x,y,z)|] [p|(x,y,z)|] [d|x = filterInstances; y = filterSecurityGroup; z = Replace|])
    where
        filterInstances = align
                            -- identify not deleted item
                            (\(Instance _ _ _ _ _ sg _) -> not (isInfixOf "database" (lower sg)))
                            -- match between source and view
                            (\(Instance s _ _ _ _ _ _) (Instance v _ _ _ _ _ _) -> s == v)
                            -- Update function
                            $(update [p|x|] [p|x|] [d|x = Replace|])
                            -- Create an item in the source from an item in the view
                            (\x-> x)
                            -- How to delete an item from the source list
                            (\_ -> Nothing)
        filterSecurityGroup = align
                            -- identify not deleted item
                            (\(SecurityGroup name _ _ _) -> not (isInfixOf "database" (lower name)))
                            -- match between source and view
                            (\(SecurityGroup s _ _ _) (SecurityGroup v _ _ _) -> s == v)
                            -- Update function
                            $(update [p|x|] [p|x|] [d|x = Replace|])
                            -- Create an item in the source from an item in the view
                            (\x-> x)
                            -- How to delete an item from the source list
                            (\_ -> Nothing)

        lower :: String -> String
        lower x = Prelude.map Data.Char.toLower x


{-
  No special event, just a normal day

  Rules triggered :
    (HourOfDay > 6) and (HourOfDay < 20) : Redundancy, *, Cost
-}
exp1 = do
    access <- return "AKIAI4BI63OOGVN7VCQA"
    secret <- return "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"
    region <- return "ap-northeast-1"
    keypair <- return "Quentin"
    image <- return "ami-937ee3f5"
    lb <- return "BiGUL-CloudBx"

    putStrLn("\x1b[36mRetrieving current data from aws ec2 ...\x1b[0m ")
    source <- monitor access secret region


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
    putStrLn("\x1b[36mGeneration of the new infrastructure ...\x1b[0m ")
    concerns <- return ([
        (Concern "Cost" (filterDatabase `Compose` costUpdate) (costPlan 0.5)),
        (Concern "Redundancy" (filterDatabase `Compose` redundancyUpdate) redundancyPlan),
        (Concern "AutoScaling" (filterDatabase `Compose` autoScalingUpdate) analysisAndPlanAutoScaling),
        (Concern "Firewall" firewallUpdate firewallPlan)])  -- (3)
    res <- (executeSeq ctx rules Safety source concerns)  -- (4)

    putStrLn("\x1b[36mExecution in progress ...\x1b[0m ")
    _ <- executeAWS access secret region keypair image lb (fst res)

    saveExperiment "Exp1-AWS" rulesTriggered source (fst res) (snd res)

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
    access <- return "AKIAI4BI63OOGVN7VCQA"
    secret <- return "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"
    region <- return "ap-northeast-1"
    keypair <- return "Quentin"
    image <- return "ami-937ee3f5"
    lb <- return "BiGUL-CloudBx"

    putStrLn("\x1b[36mRetrieving current data from aws ec2 ...\x1b[0m ")
    source <- monitor access secret region


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
        (Concern "Cost" (filterDatabase `Compose` costUpdate) (costPlan 0.5)),
        (Concern "Redundancy" (filterDatabase `Compose` redundancyUpdate) redundancyPlan),
        (Concern "AutoScaling" (filterDatabase `Compose` autoScalingUpdate) analysisAndPlanAutoScaling),
        (Concern "Firewall" firewallUpdate firewallPlan)])  -- (3)
    res <- (executeSeq ctx rules Safety source concerns)  -- (4)

    putStrLn("\x1b[36mExecution in progress ...\x1b[0m ")
    _ <- executeAWS access secret region keypair image lb (fst res)
    saveExperiment "Exp2-AWS" rulesTriggered source (fst res) (snd res)

-- Christmas, with Security emergency
exp3 = do
    access <- return "AKIAI4BI63OOGVN7VCQA"
    secret <- return "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"
    region <- return "ap-northeast-1"
    keypair <- return "Quentin"
    image <- return "ami-937ee3f5"
    lb <- return "BiGUL-CloudBx"

    putStrLn("\x1b[36mRetrieving current data from aws ec2 ...\x1b[0m ")
    source <- monitor access secret region


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
        (Concern "Cost" (filterDatabase `Compose` costUpdate) (costPlan 0.5)),
        (Concern "Redundancy" (filterDatabase `Compose` redundancyUpdate) redundancyPlan),
        (Concern "AutoScaling" (filterDatabase `Compose` autoScalingUpdate) analysisAndPlanAutoScaling),
        (Concern "Firewall" firewallUpdate firewallPlan)])  -- (3)
    res <- (executeSeq ctx rules Safety source concerns)  -- (4)

    putStrLn("\x1b[36mExecution in progress ...\x1b[0m ")
    _ <- executeAWS access secret region keypair image lb (fst res)
    saveExperiment "Exp3-AWS" rulesTriggered source (fst res) (snd res)

-- FireEmergency
exp4 = do
    access <- return "AKIAI4BI63OOGVN7VCQA"
    secret <- return "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"
    region <- return "ap-northeast-1"
    keypair <- return "Quentin"
    image <- return "ami-937ee3f5"
    lb <- return "BiGUL-CloudBx"

    putStrLn("\x1b[36mRetrieving current data from aws ec2 ...\x1b[0m ")
    source <- monitor access secret region


    ctx <- return (fromList [
        ("SecurityEmergency", B False),
        ("FireEmergency", B True),
        ("HourOfDay", I 14),
        ("DayOfWeek", I 3),
        ("WeekOfYear", I 20),
        ("Month", I 5),
        ("AdvertisingRevenue", I 250),
        ("PeriodOfReduction", B False)])
    rules <- ruleParser "Examples/rules.txt"
    rulesTriggered <- return $ evalRules ctx rules
    concerns <- return ([
        (Concern "Cost" (filterDatabase `Compose` costUpdate) (costPlan 0.5)),
        (Concern "Redundancy" (filterDatabase `Compose` redundancyUpdate) redundancyPlan),
        (Concern "AutoScaling" (filterDatabase `Compose` autoScalingUpdate) analysisAndPlanAutoScaling),
        (Concern "Firewall" firewallUpdate firewallPlan)])  -- (3)
    res <- (executeSeq ctx rules Safety source concerns)  -- (4)

    putStrLn("\x1b[36mExecution in progress ...\x1b[0m ")
    _ <- executeAWS access secret region keypair image lb (fst res)
    saveExperiment "Exp4-AWS" rulesTriggered source (fst res) (snd res)



exp5 = do
    access <- return "AKIAI4BI63OOGVN7VCQA"
    secret <- return "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"
    region <- return "ap-northeast-1"
    keypair <- return "Quentin"
    image <- return "ami-937ee3f5"
    lb <- return "BiGUL-CloudBx"

    putStrLn("\x1b[36mRetrieving current data from aws ec2 ...\x1b[0m ")
    source <- monitor access secret region


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
        (Concern "Cost" (filterDatabase `Compose` costUpdate) (costPlan 0.5)),
        (Concern "Redundancy" (filterDatabase `Compose` redundancyUpdate) redundancyPlan),
        (Concern "AutoScaling" (filterDatabase `Compose` autoScalingUpdate) analysisAndPlanAutoScaling),
        (Concern "Firewall" firewallUpdate firewallPlan)])  -- (3)
    res <- (executeSeq ctx rules Safety source concerns)  -- (4)

    putStrLn("\x1b[36mExecution in progress ...\x1b[0m ")
    _ <- executeAWS access secret region keypair image lb (fst res)
    saveExperiment "Exp5-AWS" rulesTriggered source (fst res) (snd res)
