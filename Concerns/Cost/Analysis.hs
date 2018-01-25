{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Concerns.Cost.Analysis(costAnalysis) where

import GHC.Generics
import Data.List
import Data.Maybe

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib

import Concerns.Cost.Model

costAnalysis :: CView -> Double
costAnalysis (CView instances instanceTypes) = sum (map (\(CInstanceType ident cost) -> calculateCost instances ident cost) instanceTypes)

calculateCost :: [CInstance] -> String -> Double -> Double
calculateCost instances typeName cost = (fromIntegral (length (filter (\(CInstance _ typeInstance _) -> typeName == typeInstance) instances))) * cost
