{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Concerns.Redundancy.Plan(redundancyPlan) where

import GHC.Generics
import Data.List
import Data.Ord (comparing)

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib

import Concerns.Redundancy.Model
import Concerns.Redundancy.Analysis
import Concerns.Redundancy.Utils

redundancyPlan :: RView -> RView
redundancyPlan inst@(RView instances securityGroups instanceTypes) = completeInstances inst (redundancyAnalysis instances securityGroups)

completeInstances :: RView -> [(RSecurityGroup, Int)] -> RView
completeInstances view [] = view
completeInstances (RView instances securityGroups instanceTypes) (incompleteSG:ss) = completeInstances (RView (handleCompleteness instances instanceTypes incompleteSG) securityGroups instanceTypes) ss

handleCompleteness :: [RInstance] -> [RInstanceType] -> (RSecurityGroup, Int) -> [RInstance]
handleCompleteness instances ((RInstanceType typeId):_) ((RSecurityGroup secId refs), occurence) = case occurence of
            1 -> case find (\(RInstance instId _ state status _) -> ((state == 16 && status /= 2) || (status == 1)) && (elem instId refs)) instances of
                Just (RInstance instId1 instType1 _ _ _) -> case find (\(RInstance instId2 instType2 _ _ _) -> (instId1 /= instId2) && (instType1 == instType2) && (elem instId2 refs)) instances of
                    Just (RInstance i t ste stus sRef) -> (delete (RInstance i t ste stus sRef) instances) ++ [(RInstance i t ste (if (stus == 2) then 0 else 1) sRef)]
                    Nothing -> instances ++ [(RInstance "" instType1 0 1 secId)]
                Nothing -> error ("Source inconsistent 4")
            0 -> case filter (\(RInstance _ instType _ _ _) -> instType == findMostUsedType (filter (\(RInstance _ _ _ _ sgRef) -> sgRef == secId) instances)) instances of
                [] -> case filter (\(RInstance _ t _ _ _) -> t == findMostUsedType instances) instances of
                    ((RInstance _ mostUsedType _ _ _):[]) -> instances ++ [(RInstance "" mostUsedType 0 1 secId), (RInstance "" mostUsedType 0 1 secId)]
                    otherwise -> instances ++ [(RInstance "" typeId 0 1 secId), (RInstance "" typeId 0 1 secId)]
                ((RInstance i t ste stus sRef):[]) -> (delete (RInstance i t ste stus sRef) instances) ++ [(RInstance i t ste (if (stus == 2) then 0 else 1) sRef), (RInstance "" t 0 1 sRef)]
                ((RInstance i1 t1 ste1 stus1 sRef1):(RInstance i2 t2 ste2 stus2 sRef2):_) -> (delete (RInstance i2 t2 ste2 stus2 sRef2) (delete (RInstance i1 t1 ste1 stus1 sRef1) instances))
                                                                                            ++  [(RInstance i1 t1 ste1 (if (stus1 == 2) then 0 else 1) sRef1), (RInstance i2 t2 ste2 (if (stus2 == 2) then 0 else 1) sRef2)]

findMostUsedType :: [RInstance] -> String
findMostUsedType [] = ""
findMostUsedType instances = fst (maximumBy (comparing snd) (frequency (map (\(RInstance _ instType _ _ _) -> instType) instances)))
