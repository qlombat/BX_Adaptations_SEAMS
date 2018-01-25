{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Concerns.AutoScaling.Analysis(autoScalingAnalysis) where

import GHC.Generics
import Data.List
import Data.Maybe

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib

import Concerns.AutoScaling.Model as AS
import Concerns.AutoScaling.Utils

{-
    this function provide the number of cpu in addition to keep the average load below the limit
-}
autoScalingAnalysis :: Double -> AS.ASView -> [(String, Int)]
autoScalingAnalysis avrLoadExpected (ASView instances instTypes) = map (\(a,b) -> (a, ceiling ((foldl (\acc inst ->  (findLoadOfInstance instTypes inst) + acc) 0.0 (instancesCurrentlyRunning b)) / avrLoadExpected))) (groupBySecurityGroup instances)
