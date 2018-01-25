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

    sourceIO <- monitor access secret region
    source <- evaluate $ force $ sourceIO

    cTime <- getCurrentTime
    time <- return $ (formatTime defaultTimeLocale "%H-%M-%S" cTime)
    writeFile ("../Reports/bench-source-" ++ time ++ ".json") (serialize source)


    rulesIO <- ruleParser "../Examples/rules.txt"
    rules <- evaluate $ force $ rulesIO

    ctx <- evaluate $ force $ (fromList [
        ("SecurityEmergency", B False),
        ("FireEmergency", B False),
        ("HourOfDay", I 14),
        ("DayOfWeek", I 3),
        ("WeekOfYear", I 20),
        ("Month", I 5),
        ("AdvertisingRevenue", I 0),
        ("PeriodOfReduction", B False)])

    concerns <- return ([
        (ConcernRemote "Firewall" firewallUpdate "http://ip-172-31-18-141.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_firewall.php" 80 False serialize unseralizeF),  -- (3)
        (ConcernRemote "AutoScaling" (filterDatabase `Compose` autoScalingUpdate) "http://ip-172-31-16-82.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_auto_scaling.php" 80 False serialize unseralizeAS),
        (ConcernRemote "Redundancy" (filterDatabase `Compose` redundancyUpdate) "http://ip-172-31-24-174.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_redundancy.php" 80 False serialize unseralizeR),  -- (3)
        (ConcernRemote "Cost" (filterDatabase `Compose` costUpdate) "http://ip-172-31-17-83.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_cost.php" 80 False serialize unseralizeC)])

    concernsLocal <- return ([
        (Concern "Cost" (filterDatabase `Compose` costUpdate) (costPlan 0.5)),
        (Concern "AutoScaling" (filterDatabase `Compose` autoScalingUpdate) analysisAndPlanAutoScaling),
        (Concern "Redundancy" (filterDatabase `Compose` redundancyUpdate) redundancyPlan),
        (Concern "Firewall" firewallUpdate firewallPlan)])  -- (3)


    orderSafety <- evaluate $ force $ dertermineOrder ctx rules ["Cost", "AutoScaling", "Redundancy", "Firewall"] Safety
    orderNewer <- evaluate $ force $ dertermineOrder ctx rules ["Cost", "AutoScaling", "Redundancy", "Firewall"] Newer


    (sourceFUpdated, MasterView ioVF) <- execBranch source (ConcernRemote "Firewall" firewallUpdate "http://ip-172-31-18-141.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_firewall.php" 80 False serialize unseralizeF)
    (sourceFAUpdated, MasterView ioVAS) <- execBranch sourceFUpdated (ConcernRemote "AutoScaling" (filterDatabase `Compose` autoScalingUpdate) "http://ip-172-31-16-82.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_auto_scaling.php" 80 False serialize unseralizeAS)
    (sourceFARUpdated, MasterView ioVR) <- execBranch sourceFAUpdated (ConcernRemote "Redundancy" (filterDatabase `Compose` redundancyUpdate) "http://ip-172-31-24-174.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_redundancy.php" 80 False serialize unseralizeR)
    (sourceFARCCUpdated, MasterView ioVCost) <- execBranch sourceFARUpdated (ConcernRemote "Cost" (filterDatabase `Compose` costUpdate) "http://ip-172-31-17-83.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_cost.php" 80 False serialize unseralizeC)

    sourceFUpdated <- evaluate $ force $ sourceFUpdated
    sourceFAUpdated <- evaluate $ force $ sourceFAUpdated
    sourceFARUpdated <- evaluate $ force $ sourceFARUpdated
    sourceFARCCUpdated <- evaluate $ force $ sourceFARCCUpdated

    vCost <- return $ unseralizeC (serialize ioVCost)
    vFirewall <- return $ unseralizeF (serialize ioVF)
    vRedundancy <- return $ unseralizeR (serialize ioVR)
    vAS <- return $ unseralizeAS (serialize ioVAS)


    defaultMain [
        bgroup "Monitor" [
            -- bench "AWS"  $ nfIO (monitor access secret region),
            bench "Parsing rules"  $ nfIO (ruleParser "../Examples/rules.txt")],
        bgroup "get" [
            bench "cost"  $ nf (get costUpdate) source,
            bench "auto-scaling"  $ nf (get autoScalingUpdate) source,
            bench "redundancy"  $ nf (get redundancyUpdate) source,
            bench "firewall"  $ nf (get firewallUpdate) source],
        bgroup "Synchronizer" [
            bench "Determine order Safety"  $ nf (dertermineOrder ctx rules ["Cost", "AutoScaling", "Redundancy", "Firewall"]) Safety,
            bench "Determine order Newer"  $ nf (dertermineOrder ctx rules ["Cost", "AutoScaling", "Redundancy", "Firewall"]) Newer,
            bench "Order Safety"  $ whnf (orderConcern orderSafety) concerns,
            bench "Order Newer"  $ whnf (orderConcern orderNewer) concerns],
        bgroup "Analysis and Planning http" [
            bench "All (determine order, ordering and analysis & plan)"  $ nfIO (executeSeq ctx rules Safety source concerns),
            bench "Firewall"  $ nfIO (execBranch source (ConcernRemote "Firewall" firewallUpdate "http://ip-172-31-18-141.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_firewall.php" 80 False serialize unseralizeF)),
            bench "AutoScaling"  $ nfIO (execBranch sourceFUpdated (ConcernRemote "AutoScaling" (filterDatabase `Compose` autoScalingUpdate) "http://ip-172-31-16-82.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_auto_scaling.php" 80 False serialize unseralizeAS)),
            bench "Redundancy"  $ nfIO (execBranch sourceFAUpdated (ConcernRemote "Redundancy" (filterDatabase `Compose` redundancyUpdate) "http://ip-172-31-24-174.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_redundancy.php" 80 False serialize unseralizeR)),
            bench "Cost"  $ nfIO (execBranch sourceFARUpdated (ConcernRemote "Cost" (filterDatabase `Compose` costUpdate) "http://ip-172-31-17-83.ap-northeast-1.compute.internal/duduloma/Conflict/AWS/Scripts/analyse_and_plan_cost.php" 80 False serialize unseralizeC))],
        bgroup "Analysis and Planning local" [
            bench "All (determine order, ordering and analysis & plan)"  $ nfIO (executeSeq ctx rules Safety source concernsLocal),
            bench "Firewall"  $ nfIO (execBranch source (Concern "Firewall" firewallUpdate firewallPlan)),
            bench "AutoScaling"  $ nfIO (execBranch sourceFUpdated (Concern "AutoScaling" (filterDatabase `Compose` autoScalingUpdate) analysisAndPlanAutoScaling)),
            bench "Redundancy"  $ nfIO (execBranch sourceFAUpdated (Concern "Redundancy" (filterDatabase `Compose` redundancyUpdate) redundancyPlan)),
            bench "Cost"  $ nfIO (execBranch sourceFARUpdated (Concern "Cost" (filterDatabase `Compose` costUpdate) (costPlan 0.5)))],
        bgroup "put" [
            bench "cost"  $ nf (put costUpdate source) vCost,
            bench "auto-scaling"  $ nf (put autoScalingUpdate source) vAS,
            bench "redundancy"  $ nf (put redundancyUpdate source) vRedundancy,
            bench "firewall"  $ nf (put firewallUpdate source) vFirewall]]

    _ <- executeAWS access secret region keypair image lb sourceFARCCUpdated
    return ()
