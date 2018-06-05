{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings, DeriveAnyClass, StandaloneDeriving,DeriveGeneric #-}
module Lib.Synchronizer.RuleBase(
    RuleOperator(..),
    RuleView(..),
    Rule,
    Rules,
    determineOrder,
    PriorityPolicy(..), evalRules, findCorrectSituations, goodSituation, determineViewPriority) where

import GHC.Generics
import Control.DeepSeq

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Char
import Data.Maybe
import Control.Monad (guard)



import Lib.Synchronizer.Context

-- Data type of rule
data RuleOperator = Equals CtxKey CtxValue |
                    LessThan CtxKey CtxValue |
                    LessOrEqualsThan CtxKey CtxValue |
                    MoreThan CtxKey CtxValue |
                    MoreOrEqualsThan CtxKey CtxValue |
                    Not RuleOperator |
                    And RuleOperator RuleOperator |
                    Or RuleOperator RuleOperator |
                    T | F deriving Generic
instance Show RuleOperator where
    show (Equals k v) = k ++ " == " ++ show v
    show (LessThan k v) = k ++ " < " ++ show v
    show (LessOrEqualsThan k v) = k ++ " <= " ++ show v
    show (MoreThan k v) = k ++ " > " ++ show v
    show (MoreOrEqualsThan k v) = k ++ " >= " ++ show v
    show (Not x) = "not( " ++ show x ++ ")"
    show (And r1 r2) = "(" ++ show r1 ++ ") and (" ++ show r2 ++ ")"
    show (Or r1 r2) = "(" ++ show r1 ++ ") or (" ++  show r2 ++ ")"
    show (T) = "true"
    show (F) = "False"
deriving instance NFData RuleOperator


data RuleView = Anything | V String deriving Generic
deriving instance NFData RuleView
instance Show RuleView where
    show (Anything) = "*"
    show (V v) = show v

instance Eq RuleView where
    (Anything) == (Anything) = True
    (V v1) == (V v2) = (map toUpper v1) == (map toUpper v2) -- case insensitive
    (Anything) == (V v) = False
    (V v) == (Anything) = False

type Rule = (RuleOperator, [RuleView])
type Rules = [Rule]

{-
    The PriorityPolicy determine how the list of rules must be interpreted:
        Neutral : the list of rules must be consistent which means that all true rule cannot be contradictory
        Safety : The rules at the beginning of the list crush all next contradictory rules
        Last : The rules at the end of the list crush all previous contradictory rules
-}
data PriorityPolicy = Neutral | Safety | Last  deriving Generic
deriving instance NFData PriorityPolicy



-- Describe how to evaluate a rule
evalRules :: Context -> Rules -> Rules
evalRules ctx = filter (\(a,b) -> evalBooleanCondition ctx a)

-- Return true when the boolean conditions are true according to the context, false otherwise
evalBooleanCondition :: Context -> RuleOperator -> Bool
evalBooleanCondition ctx (Equals k val)     = case (Map.lookup k ctx) of
                                                Just v -> val == v
                                                Nothing -> False
evalBooleanCondition ctx (LessThan k val)   = case (Map.lookup k ctx) of
                                                Just v -> v < val
                                                Nothing -> False
evalBooleanCondition ctx (LessOrEqualsThan k val)   = case (Map.lookup k ctx) of
                                                Just v -> v <= val
                                                Nothing -> False
evalBooleanCondition ctx (MoreThan k val)   = case (Map.lookup k ctx) of
                                                Just v -> v > val
                                                Nothing -> False
evalBooleanCondition ctx (MoreOrEqualsThan k val)   = case (Map.lookup k ctx) of
                                                Just v -> v >= val
                                                Nothing -> False
evalBooleanCondition ctx (Not r)            = not (evalBooleanCondition ctx r)
evalBooleanCondition ctx (And r1 r2)        = (evalBooleanCondition ctx r1) && (evalBooleanCondition ctx r2)
evalBooleanCondition ctx (Or r1 r2)         = (evalBooleanCondition ctx r1) || (evalBooleanCondition ctx r2)
evalBooleanCondition ctx T                  = True
evalBooleanCondition ctx F                  = False

-- determine is the situation fit the the second situation
goodSituation :: [RuleView] -> [RuleView] -> Bool
goodSituation [] []                     = True
goodSituation _ [Anything]              = True
goodSituation [] (Anything:ys)          = goodSituation [] ys
goodSituation [] _                      = False
goodSituation _ []                      = False
goodSituation x (Anything:Anything:ys)  = goodSituation x (Anything:ys)
goodSituation (x:xs) (Anything:y:ys)    | (x == y) = (goodSituation xs ys)
                                        | otherwise = (goodSituation xs (Anything:y:ys))
goodSituation (x:xs) (y:ys)              = (x == y) && (goodSituation xs ys)

-- determine the priority of each view according to the PriorityPolicy
determineViewPriority :: PriorityPolicy -> [RuleView] -> [[RuleView]] -> [RuleView]
determineViewPriority Neutral listView lstRuleView  = head (findCorrectSituations listView lstRuleView)
determineViewPriority Safety listView lstRuleView    = head (determineViewPrioritySafety listView lstRuleView)
determineViewPriority Last listView lstRuleView    = head (determineViewPrioritySafety listView (reverse lstRuleView))
{-
This function will add rules one by one and skip the ones that are not consistent with previous ones to find a correct situation.

Example :
[*,A,*,B,*], [*,B,*,C,*], [*,C,*,A,*], [*,B,*,D,*] -> [*,A,*,B,*], [*,B,*,C,*], [*,B,*,D,*]

determineViewPrioritySafety :: [RuleView] -> [[RuleView]] -> [[RuleView]]
determineViewPrioritySafety listView [] = findCorrectSituations listView []
determineViewPrioritySafety listView x = findMaxModel [] x
  where
    findMaxModel h [] = case findCorrectSituations listView h of
                          [] -> findCorrectSituations listView (tail h)
                          res -> res
    findMaxModel [] (x:xs) = findMaxModel [x] xs
    findMaxModel h (x:xs) = case findCorrectSituations listView h of
                              [] -> findMaxModel (tail h) (x:xs)
                              res -> findMaxModel (x:h) xs
-}


{-
This function will remove all rules after an inconsistent one to find a correct situation.
Example :
[*,B,*,D,*], [*,C,*,A,*], [*,B,*,C,*], [*,A,*,B,*] ->  [*,B,*,C,*], [*,A,*,B,*]

determineViewPriorityLast :: [RuleView] -> [[RuleView]] -> [[RuleView]]
determineViewPriorityLast listView [] = findCorrectSituations listView []
determineViewPriorityLast listView (x:xs) = case findCorrectSituations listView (x:xs) of
                                              [] -> determineViewPriorityLast listView xs
                                              res -> res


testDetermineViewPriorityLast :: [RuleView] -> [[RuleView]] -> [[RuleView]]
testDetermineViewPriorityLast listView (x:xs) = findMaxModel (findCorrectSituations listView []) x xs
  where
    findMaxModel sol [] [] = sol
    findMaxModel sol currentRule [] = case filter (\s -> (goodSituation s currentRule)) sol of
                          [] -> sol
                          res -> res
    findMaxModel sol currentRule (x:xs) = case filter (\s -> (goodSituation s currentRule)) sol of
                              [] -> findMaxModel sol x xs
                              res -> findMaxModel res x xs
                              -}
perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [ i:j | i <- xs, j <- perms $ delete i xs ]

determineViewPrioritySafety :: [RuleView] -> [[RuleView]] -> [[RuleView]]
determineViewPrioritySafety listView [] = perms (nub [x | x <- listView, x /= Anything])
determineViewPrioritySafety listView (x:xs) = findMaxModel (perms (nub [x | x <- listView, x /= Anything])) x xs
    where
        findMaxModel sol [] [] = sol
        findMaxModel sol currentRule [] = case filter (\s -> (goodSituation s currentRule)) sol of
                              [] -> sol
                              res -> res
        findMaxModel sol currentRule (x:xs) = case filter (\s -> (goodSituation s currentRule)) sol of
                                  [] -> findMaxModel sol x xs
                                  res -> findMaxModel res x xs


-- find a situation that respect all rules
findCorrectSituations :: [RuleView] -> [[RuleView]] -> [[RuleView]]
findCorrectSituations listView lstRuleView = do
    x <- (perms (nub [x | x <- listView, x /= Anything]))
    -- error (show x ++ show lstRuleView ++ show (foldl (\ acc y -> goodSituation x y && acc) True lstRuleView))
    guard $ isNothing (find (\y -> not (goodSituation x y)) lstRuleView)
    return x

determineOrder :: Context -> Rules -> [String] -> PriorityPolicy -> [String]
determineOrder ctx rules lstConcern priorityPolicy = map (\(V a) -> a) (determineViewPriority priorityPolicy lstViews lstRulesView)
    where
        lstViews =  map (\a -> V a) lstConcern
        lstRulesView = map (\(_, a) -> a) (evalRules ctx rules)
