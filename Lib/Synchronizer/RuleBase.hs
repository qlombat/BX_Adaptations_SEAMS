{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings, DeriveAnyClass, StandaloneDeriving,DeriveGeneric #-}
module Lib.Synchronizer.RuleBase(
    RuleOperator(..),
    RuleView(..),
    Rule,
    Rules,
    dertermineOrder,
    PriorityPolicy(..), evalRules, findCorrectSituations) where

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
                    LessThen CtxKey CtxValue |
                    LessOrEqualsThen CtxKey CtxValue |
                    MoreThen CtxKey CtxValue |
                    MoreOrEqualsThen CtxKey CtxValue |
                    Not RuleOperator |
                    And RuleOperator RuleOperator |
                    Or RuleOperator RuleOperator |
                    T | F deriving Generic
instance Show RuleOperator where
    show (Equals k v) = k ++ " == " ++ show v
    show (LessThen k v) = k ++ " < " ++ show v
    show (LessOrEqualsThen k v) = k ++ " <= " ++ show v
    show (MoreThen k v) = k ++ " > " ++ show v
    show (MoreOrEqualsThen k v) = k ++ " >= " ++ show v
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
        Newer : The rules at the end of the list crush all previous contradictory rules
-}
data PriorityPolicy = Neutral | Safety | Newer  deriving Generic
deriving instance NFData PriorityPolicy



-- Describe how to evaluate a rule
evalRules :: Context -> Rules -> Rules
evalRules ctx = filter (\(a,b) -> evalBooleanCondition ctx a)

-- Return true when the boolean conditions are true according to the context, false otherwise
evalBooleanCondition :: Context -> RuleOperator -> Bool
evalBooleanCondition ctx (Equals k val)     = case (Map.lookup k ctx) of
                                                Just v -> val == v
                                                Nothing -> False
evalBooleanCondition ctx (LessThen k val)   = case (Map.lookup k ctx) of
                                                Just v -> v < val
                                                Nothing -> False
evalBooleanCondition ctx (LessOrEqualsThen k val)   = case (Map.lookup k ctx) of
                                                Just v -> v <= val
                                                Nothing -> False
evalBooleanCondition ctx (MoreThen k val)   = case (Map.lookup k ctx) of
                                                Just v -> v > val
                                                Nothing -> False
evalBooleanCondition ctx (MoreOrEqualsThen k val)   = case (Map.lookup k ctx) of
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
goodSituation [] _                      =  True
goodSituation _ []                      = False
goodSituation (x:xs) (Anything:y:ys)    | (x == y) = (goodSituation xs ys)
                                        | otherwise = (goodSituation xs (Anything:y:ys))
goodSituation (x:xs) (y:ys)              = (x == y) && (goodSituation xs ys)

-- determine the priority of each view according to the PriorityPolicy
determineViewPriority :: PriorityPolicy -> [RuleView] -> [[RuleView]] -> [RuleView]
determineViewPriority Neutral listView lstRuleView  = head (findCorrectSituations listView lstRuleView)
determineViewPriority Safety listView lstRuleView    = head (findCorrectSituations listView (determineViewPrioritySafty lstRuleView))
determineViewPriority Newer listView lstRuleView    = head (findCorrectSituations listView (determineViewPrioritySafty (reverse lstRuleView)))

determineViewPrioritySafty :: [[RuleView]] -> [[RuleView]]
determineViewPrioritySafty [] = []
determineViewPrioritySafty (x:xs) = x:(determineViewPrioritySafty (findConsistantRules x xs))
    where
        findConsistantRules :: [RuleView] -> [[RuleView]] -> [[RuleView]]
        findConsistantRules r  = filter (\e -> (goodSituation [x | x <- r, x /= Anything] e))


-- find a situation that respect all rules
findCorrectSituations :: [RuleView] -> [[RuleView]] -> [[RuleView]]
findCorrectSituations listView lstRuleView = do
    x <- (perms (nub [x | x <- listView, x /= Anything]))
    --error (show x ++ show lstRuleView ++ show (goodSituation [V "Cost", Anything] x))
    guard $ foldl (\ acc y -> goodSituation x y && acc) True lstRuleView
    return x
        where
            perms :: Eq a => [a] -> [[a]]
            perms [] = [[]]
            perms xs = [ i:j | i <- xs, j <- perms $ delete i xs ]

dertermineOrder :: Context -> Rules -> [String] -> PriorityPolicy -> [String]
dertermineOrder ctx rules lstConcern priorityPolicy = map (\(V a) -> a) (determineViewPriority priorityPolicy lstViews lstRulesView)
    where
        lstViews =  map (\a -> V a) lstConcern
        lstRulesView = map (\(_, a) -> a) (evalRules ctx rules)
