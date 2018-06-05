{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Concerns.Redundancy.Analysis(redundancyAnalysis) where

import GHC.Generics
import Data.List as L
import Data.Maybe
import Data.Map

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib

import Concerns.Redundancy.Model
import Concerns.Redundancy.Utils

redundancyAnalysis :: [RInstance] -> [RSecurityGroup] -> [(RSecurityGroup, Int)]
redundancyAnalysis instances securityGroups = (L.map (\(groupName, occurrence) -> case find (\(RSecurityGroup i _) -> groupName == i) securityGroups of
                                                                                Just s -> (s, occurrence)
                                                                                Nothing -> error ("Inconsistent Source")) (findSecurityGroups instances))
                                              ++ (L.map (\securityGroup -> (securityGroup, 0)) (L.filter (\(RSecurityGroup _ ref) -> (length ref == 0)) securityGroups))

findSecurityGroups :: [RInstance] -> [(String, Int)]
findSecurityGroups instances = nub (L.filter (\(_,occ) -> occ < 2) (L.map (\(RInstance _ _ _ _ ref) -> (countRunningInstances (L.filter (\(RInstance _ _ _ _ ref1) -> ref == ref1) instances))) instances))

countRunningInstances :: [RInstance] -> (String, Int)
countRunningInstances (inst@(RInstance i t state status ref):instances) = (ref, length (L.filter (\(RInstance _ _ state status _) -> (state == 16 && status /= 2) || (status == 1)) (inst:instances)))
