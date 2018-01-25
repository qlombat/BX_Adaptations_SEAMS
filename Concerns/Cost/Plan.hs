{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Concerns.Cost.Plan(costPlan) where

import GHC.Generics
import Data.List
import Data.Maybe

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib

import Concerns.Cost.Model
import Concerns.Cost.Analysis

costPlan :: Double -> CView -> CView
costPlan limitCost cView = handlePlan (costAnalysis cView) limitCost cView

handlePlan :: Double -> Double -> CView -> CView
handlePlan currentCost limitCost (CView instances instanceTypes)
            | currentCost > limitCost = handlePlan (costAnalysis newIntermediateSource) limitCost newIntermediateSource
            | otherwise = CView instances instanceTypes
                where
                    newIntermediateSource = CView sortedList instanceTypes

                    sortedList = tail (sortBy sortByCost instances)

                    sortByCost :: CInstance -> CInstance -> Ordering
                    sortByCost (CInstance _ type1 load1) (CInstance _ type2 load2)
                                | load1 < load2 = LT
                                | load1 == load2 = case
                                                    ((find (\(CInstanceType typeName1 _) -> typeName1 == type1) instanceTypes),
                                                    (find (\(CInstanceType typeName2 _) -> typeName2 == type2) instanceTypes))
                                                    of
                                    (Just (CInstanceType _ cost1), Just (CInstanceType _ cost2)) -> if cost1 > cost2 then LT else GT
                                    (Nothing,_) -> error ("Source inconsistent 1")
                                    (_,Nothing) -> error ("Source inconsistent 2")
                                | otherwise = GT
