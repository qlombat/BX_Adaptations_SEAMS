{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Lib.Synchronizer.RuleHandler (addRule) where

import qualified Data.Map
import Data.List
import Control.Monad
import Data.Char

import Lib.Synchronizer.RuleBase
import Lib.Synchronizer.Context
import Lib.Synchronizer.RuleParser

addRule :: String -> String -> IO [Rule]
addRule fileName newRule = do
    parsedRules <- ruleParser fileName
    return (checkRules parsedRules (parseStringToRule newRule))

checkRules :: [Rule] -> Rule -> [Rule]
checkRules rules (ruleOp, viewsOrder) = filter (\(rOp, vOrder) -> not (checkViewsOrder vOrder viewsOrder)) rules

checkViewsOrder :: [RuleView] -> [RuleView] -> Bool
checkViewsOrder xs _ | (length xs < 2) = True
checkViewsOrder (x1:x2:xs) ys = (checkPairs (x1,x2) ys) && (checkViewsOrder (x2:xs) ys)

checkPairs :: (RuleView, RuleView) -> [RuleView] -> Bool
checkPairs (x1, x2) _ | (x1 == Anything || x2 == Anything) = True
checkPairs (x1, x2) ys = and (map (\(y1, y2) -> if (x1 == y2) && (x2 == y1) then False else True) (makeCombinations ys))

makeCombinations :: [RuleView] -> [(RuleView, RuleView)]
makeCombinations (r1:r2:[]) = [(r1, r2)]
makeCombinations (r:rs) = map (\ruleView -> (r, ruleView)) rs ++ (makeCombinations rs)