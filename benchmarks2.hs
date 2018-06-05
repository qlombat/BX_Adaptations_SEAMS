{-# LANGUAGE FlexibleContexts,  TemplateHaskell,  TypeFamilies, OverloadedStrings #-}


import Criterion.Main
import Control.DeepSeq
import Control.Exception (evaluate)
import Data.Maybe
import Data.Map (fromList)

import Data.ByteString.Lazy.Char8 as Char8 (pack, unpack)
import Data.Aeson

import Data.Map (map, fromList)
import Data.List (isInfixOf)
import Data.Char (toLower)
import Data.Time
import Data.Time.Format (formatTime)



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



serialize :: ToJSON a => a -> String
serialize x = Char8.unpack $ encode x


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


analysisAndPlanAutoScaling :: ASView -> ASView
analysisAndPlanAutoScaling v = autoScalingPlan (autoScalingAnalysis 0.4 v) v



main :: IO ()
main = do
    access <- return "AKIAI4BI63OOGVN7VCQA"
    secret <- return "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"
    region <- return "ap-northeast-1"
    keypair <- return "Quentin"
    image <- return "ami-1955cc7f"
    lb <- return "BiGUL-CloudBx"

    --sourceIO <- monitor access secret region
    sourceIO <- readFile "../Experiments/Exp2/Source/source.json"
    source <- evaluate $ force $ unseralizeSource sourceIO

    cTime <- getCurrentTime
    time <- return $ (formatTime defaultTimeLocale "%H-%M-%S" cTime)
    writeFile ("../Reports/bench2-source-" ++ time ++ ".json") (serialize source)


    rulesIO <- ruleParser "../Examples/rules.txt"
    rules <- evaluate $ force $ rulesIO

    ctx <- return (fromList [
        ("SecurityEmergency", B False),
        ("FireEmergency", B False),
        ("HourOfDay", I 14),
        ("DayOfWeek", I 3),
        ("WeekOfYear", I 20),
        ("Month", I 5),
        ("AdvertisingRevenue", I 250),
        ("PeriodOfReduction", B True)])

    concerns <- return ([
        (ConcernRemote "Cost" (filterDatabase `Compose` costUpdate) "http://ip-172-31-17-83.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_cost.php" 80 False serialize unseralizeC),
        (ConcernRemote "Firewall" firewallUpdate "http://ip-172-31-18-141.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_firewall.php" 80 False serialize unseralizeF),  -- (3)
        (ConcernRemote "AutoScaling" (filterDatabase `Compose` autoScalingUpdate) "http://ip-172-31-16-82.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_auto_scaling.php" 80 False serialize unseralizeAS),
        (ConcernRemote "Redundancy" (filterDatabase `Compose` redundancyUpdate) "http://ip-172-31-24-174.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_redundancy.php" 80 False serialize unseralizeR)])  -- (3)

    concernsLocal <- return ([
        (Concern "Cost" (filterDatabase `Compose` costUpdate) (costPlan 1.5)),
        (Concern "Redundancy" (filterDatabase `Compose` redundancyUpdate) redundancyPlan),
        (Concern "AutoScaling" (filterDatabase `Compose` autoScalingUpdate) analysisAndPlanAutoScaling),
        (Concern "Firewall" firewallUpdate firewallPlan)])  -- (3)

    orderSafety <- evaluate $ force $ determineOrder ctx rules ["Cost", "AutoScaling", "Redundancy", "Firewall"] Safety
    orderLast <- evaluate $ force $ determineOrder ctx rules ["Cost", "AutoScaling", "Redundancy", "Firewall"] Last


    (sourceCUpdated, MasterView ioVCost) <- execBranch source (ConcernRemote "Cost" (filterDatabase `Compose` costUpdate) "http://ip-172-31-17-83.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_cost.php" 80 False serialize unseralizeC)
    (sourceCFUpdated, MasterView ioVF) <- execBranch sourceCUpdated (ConcernRemote "Firewall" firewallUpdate "http://ip-172-31-18-141.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_firewall.php" 80 False serialize unseralizeF)
    (sourceCFAUpdated, MasterView ioVAS) <- execBranch sourceCFUpdated (ConcernRemote "AutoScaling" (filterDatabase `Compose` autoScalingUpdate) "http://ip-172-31-16-82.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_auto_scaling.php" 80 False serialize unseralizeAS)
    (sourceCFARUpdated, MasterView ioVR) <- execBranch sourceCFAUpdated (ConcernRemote "Redundancy" (filterDatabase `Compose` redundancyUpdate) "http://ip-172-31-24-174.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_redundancy.php" 80 False serialize unseralizeR)

    sourceCUpdated <- evaluate $ force $ sourceCUpdated
    sourceCFUpdated <- evaluate $ force $ sourceCFUpdated
    sourceCFAUpdated <- evaluate $ force $ sourceCFAUpdated
    sourceCFARUpdated <- evaluate $ force $ sourceCFARUpdated

    vCost <- return $ unseralizeC (serialize ioVCost)
    vFirewall <- return $ unseralizeF (serialize ioVF)
    vRedundancy <- return $ unseralizeR (serialize ioVR)
    vAS <- return $ unseralizeAS (serialize ioVAS)


    defaultMain [
        bgroup "Monitor" [
            --bench "AWS"  $ nfIO (monitor access secret region),
            bench "Parsing rules"  $ nfIO (ruleParser "../Examples/rules.txt")],
        bgroup "get" [
            bench "cost"  $ nf (get costUpdate) source,
            bench "auto-scaling"  $ nf (get autoScalingUpdate) source,
            bench "redundancy"  $ nf (get redundancyUpdate) source,
            bench "firewall"  $ nf (get firewallUpdate) source],
        bgroup "Synchronizer" [
            bench "Determine order Safety"  $ nf (determineOrder ctx rules ["Cost", "AutoScaling", "Redundancy", "Firewall"]) Safety,
            bench "Determine order Last"  $ nf (determineOrder ctx rules ["Cost", "AutoScaling", "Redundancy", "Firewall"]) Last,
            bench "Order Safety"  $ whnf (orderConcern orderSafety) concerns,
            bench "Order Last"  $ whnf (orderConcern orderLast) concerns],
        bgroup "Analysis and Planning http" [
            bench "All (determine order, ordering and analysis & plan)"  $ nfIO (executeSeq ctx rules Safety source concerns),
            bench "Cost"  $ nfIO (execBranch source (ConcernRemote "Cost" (filterDatabase `Compose` costUpdate) "http://ip-172-31-17-83.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_cost.php" 80 False serialize unseralizeC)),
            bench "Firewall"  $ nfIO (execBranch sourceCUpdated (ConcernRemote "Firewall" firewallUpdate "http://ip-172-31-18-141.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_firewall.php" 80 False serialize unseralizeF)),
            bench "Redundancy"  $ nfIO (execBranch sourceCFUpdated (ConcernRemote "Redundancy" (filterDatabase `Compose` redundancyUpdate) "http://ip-172-31-24-174.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_redundancy.php" 80 False serialize unseralizeR)),
            bench "AutoScaling"  $ nfIO (execBranch sourceCFAUpdated (ConcernRemote "AutoScaling" (filterDatabase `Compose` autoScalingUpdate) "http://ip-172-31-16-82.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_auto_scaling.php" 80 False serialize unseralizeAS))],
        bgroup "Analysis and Planning local" [
            bench "All (determine order, ordering and analysis & plan)"  $ nfIO (executeSeq ctx rules Safety source concernsLocal),
            bench "Cost"  $ nfIO (execBranch source (Concern "Cost" (filterDatabase `Compose` costUpdate) (costPlan 0.5))),
            bench "Firewall"  $ nfIO (execBranch sourceCUpdated (Concern "Firewall" firewallUpdate firewallPlan)),
            bench "Redundancy"  $ nfIO (execBranch sourceCFUpdated (Concern "Redundancy" (filterDatabase `Compose` redundancyUpdate) redundancyPlan)),
            bench "AutoScaling"  $ nfIO (execBranch sourceCFAUpdated (Concern "AutoScaling" (filterDatabase `Compose` autoScalingUpdate) analysisAndPlanAutoScaling))],
        bgroup "put" [
            bench "cost"  $ nf (put costUpdate source) vCost,
            bench "auto-scaling"  $ nf (put autoScalingUpdate source) vAS,
            bench "redundancy"  $ nf (put redundancyUpdate source) vRedundancy,
            bench "firewall"  $ nf (put firewallUpdate source) vFirewall]]

    -- _ <- executeAWS access secret region keypair image lb sourceCFARUpdated
    return ()
