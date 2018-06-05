{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Concerns.AutoScaling.Plan(autoScalingPlan) where

import GHC.Generics
import Data.List
import Data.Maybe
import Control.Monad (guard)


import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib

import Concerns.AutoScaling.Model as AS
import Concerns.AutoScaling.Utils

autoScalingPlan :: [(String, Int)] -> AS.ASView -> AS.ASView
autoScalingPlan analyses (ASView instances instanceTypes) = ASView (foldl (\acc (sg, insts) -> case find (\(sg2, i) -> sg == sg2) analyses of
    Just (_,cpuExpected) -> if (cpuExpected - (nbrCPU instanceTypes (instancesRunning insts))) >= 0
        then acc ++ (increaseWithNewInstances cpuExpected sg instanceTypes (increaseWithOffInstances cpuExpected instanceTypes insts))
        else acc ++ (decrease cpuExpected instanceTypes insts)
    Nothing -> error "Analyse is inconsistent with the planner data"
    ) [] (groupBySecurityGroup instances)) instanceTypes


increaseWithOffInstances :: Int -> [ASInstanceType] -> [ASInstance] -> [ASInstance]
increaseWithOffInstances cpu instTypes instances = if (nbrCPU instTypes ((instancesStopped instances) ++ (instancesRunning instances))) < cpu
    then (instancesRunning instances) ++ map runInstance (instancesStopped instances)
    else snd (foldl (\(acc1, acc2) inst -> if acc1 < cpu
        then (acc1 + (findNumberOfCPU instTypes inst)  ,(runInstance inst):acc2)
        else (acc1, inst:acc2)) ((nbrCPU instTypes (instancesRunning instances)),(instancesRunning instances)) (instancesStopped instances))


increaseWithNewInstances ::  Int -> String -> [ASInstanceType] -> [ASInstance] -> [ASInstance]
increaseWithNewInstances cpu currentSG instTypes instances | cpu > (nbrCPU instTypes (instancesRunning instances)) = increaseWithNewInstances cpu currentSG instTypes (((\(ASInstanceType iden _ _ _) -> (ASInstance "" iden 0 1 currentSG 0)) bestInstanceType):instances)
                                                            | otherwise = instances
    where
        bestInstanceType = foldl (\(ASInstanceType iden1 tCPU1 tRam1 tCost1) (ASInstanceType iden2 tCPU2 tRam2 tCost2) -> if (cpu - tCPU2) > 0 && (cpu - tCPU1) > (cpu - tCPU2)
            then (ASInstanceType iden2 tCPU2 tRam2 tCost2)
            else (ASInstanceType iden1 tCPU1 tRam1 tCost1)) (head instTypes) instTypes



compareWeightOfSubSeq :: ([ASInstance], (Int, Int)) -> ([ASInstance], (Int, Int)) -> Ordering
compareWeightOfSubSeq (_,(cpu1, runningInsts1)) (_, (cpu2, runningInsts2)) | cpu1 < cpu2 = LT
                                                                        | cpu1 == cpu2 && runningInsts1 < runningInsts2 = LT
                                                                        | otherwise = GT

decrease :: Int -> [ASInstanceType] -> [ASInstance] -> [ASInstance]
decrease cpu instTypes instances = (removeInstances ((nbrCPU instTypes instances) - cpu) instTypes (instancesRunning instances)) ++ (instancesStopped instances)
    where
        removeInstances :: Int -> [ASInstanceType] -> [ASInstance] -> [ASInstance]
        removeInstances nbToRemove instTypes [] = []
        removeInstances nbToRemove instTypes (x:xs) | (findNumberOfCPU instTypes x) <= nbToRemove = (stopInstance x):(removeInstances (nbToRemove - (findNumberOfCPU instTypes x)) instTypes xs)
                                                    | otherwise = x:(removeInstances nbToRemove instTypes xs)
