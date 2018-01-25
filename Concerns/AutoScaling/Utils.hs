{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Concerns.AutoScaling.Utils where

import GHC.Generics
import Data.List
import Data.Maybe

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib

import Concerns.AutoScaling.Model as AS

groupBySecurityGroup :: [ASInstance] -> [(String, [ASInstance])]
groupBySecurityGroup [] = []
groupBySecurityGroup (x:xs) = (findSecurityGroups x, filter (\y -> findSecurityGroups x == findSecurityGroups y) (x:xs)):(groupBySecurityGroup (filter (\y-> findSecurityGroups x /= findSecurityGroups y) xs))

findSecurityGroups :: ASInstance -> String
findSecurityGroups (ASInstance _ _ _ _ sg _) = sg

findNumberOfCPU :: [ASInstanceType] -> ASInstance -> Int
findNumberOfCPU instTypes (ASInstance _ instType _ _ _ _) = foldl (\acc (ASInstanceType iden cpu _ _) -> if iden == instType then cpu else acc) 0 instTypes

findLoadOfInstance :: [ASInstanceType] -> ASInstance -> Double
findLoadOfInstance instTypes (ASInstance iden instType state status sg l) = l * (fromIntegral (findNumberOfCPU instTypes (ASInstance iden instType state status sg l)))

nbrCPU :: [ASInstanceType] -> [ASInstance]  -> Int
nbrCPU instTypes = foldl (\acc inst -> (findNumberOfCPU instTypes inst) + acc ) 0

runInstance :: ASInstance -> ASInstance
runInstance (ASInstance iden instType state _ sg l) | (state == 16) = (ASInstance iden instType state 0 sg l)
                                                    | otherwise = (ASInstance iden instType state 1 sg l)

stopInstance :: ASInstance -> ASInstance
stopInstance (ASInstance iden instType state _ sg l) = (ASInstance iden instType state 2 sg l)

instancesRunning :: [ASInstance] -> [ASInstance]
instancesRunning = filter (\(ASInstance _ _ state status _ _) -> (state == 16 && status /= 2) || (state /= 16 && status == 1))

instancesCurrentlyRunning :: [ASInstance] -> [ASInstance]
instancesCurrentlyRunning = filter (\(ASInstance _ _ state status _ _) -> (state == 16))

instancesStopped :: [ASInstance] -> [ASInstance]
instancesStopped = filter (\(ASInstance _ _ state status _ _) -> not ((state == 16 && status /= 2) || (state /= 16 && status == 1)))
