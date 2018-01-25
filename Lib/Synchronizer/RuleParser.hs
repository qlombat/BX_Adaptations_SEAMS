{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Lib.Synchronizer.RuleParser (ruleParser, parseStringToRule) where

import Data.Text as T
import qualified Data.Map
import Data.List as L
import Control.Monad
import Data.Char

import Lib.Synchronizer.RuleBase
import Lib.Synchronizer.Context


ruleParser :: String -> IO [Rule]
ruleParser fileName = do
    listOfString <- fileIntoList fileName
    return (parseListStringToListRules listOfString)

fileIntoList :: String -> IO [String]
fileIntoList fileName = fmap L.lines (readFile fileName)

parseListStringToListRules :: [String] -> [Rule]
parseListStringToListRules ls = L.map (\s -> parseStringToRule s) ls

parseStringToRule :: String -> Rule
parseStringToRule line = (parseCondition (unpack (res !! 0)), parseRulesOrder (unpack (res !! 1)))
    where
        res = splitOn ":" (removeWhiteSpaces line)

removeWhiteSpaces :: String -> Text
removeWhiteSpaces s = T.strip (pack (L.filter (\c -> c /= ' ') s))

parseRulesOrder :: String -> [RuleView]
parseRulesOrder rules = L.map (\name -> replaceViewNames name) (L.map unpack (splitOn "," (pack rules)))
    where
        replaceViewNames [] = error ("Rules inconsistents")
        replaceViewNames "*" = Anything
        replaceViewNames n = V n

parseCondition :: String -> RuleOperator
parseCondition ('(':ss) = case handleParenthesis "" ss 1 of
    (cond, ('a':ss)) -> And (parseCondition cond) (parseCondition (L.drop 2 ss))
    (cond, ('o':ss)) -> Or (parseCondition cond) (parseCondition (L.drop 1 ss))
    (cond, "") -> parseCondition cond
    (_, _) -> error ("Rules inconsistents")
parseCondition ('n':'o':'t':'(':ss) = Not (parseCondition ('(':ss))
parseCondition condition = case handleCondition "" condition of
    (key, ('<':'=':value)) -> LessOrEqualsThen key (handleValue value)
    (key, ('<':value)) -> LessThen key (handleValue value)
    (key, ('>':'=':value)) -> MoreOrEqualsThen key (handleValue value)
    (key, ('>':value)) -> MoreThen key (handleValue value)
    (key, ('=':'=':value)) -> Equals key (handleValue value)

handleParenthesis :: String -> String -> Int -> (String,String)
handleParenthesis treated rest 0 = (L.init treated, rest)
handleParenthesis treated ('(':ss) int = handleParenthesis (treated ++ "(") ss (int + 1)
handleParenthesis treated (')':ss) int = handleParenthesis (treated ++ ")") ss (int - 1)
handleParenthesis treated (s:ss) int = handleParenthesis (treated ++ [s]) ss int
handleParenthesis _ _ int = error ("Rules inconsistents")

handleCondition :: String -> String -> (String,String)
handleCondition treated ('<':'=':ss) = (treated, '<':'=':ss)
handleCondition treated ('<':ss) = (treated, '<':ss)
handleCondition treated ('>':'=':ss) = (treated, '>':'=':ss)
handleCondition treated ('>':ss) = (treated, '>':ss)
handleCondition treated ('=':ss) = (treated, '=':ss)
handleCondition treated (s:ss) = handleCondition (treated ++ [s]) ss
handleCondition _ _ = error ("Rules inconsistents")

handleValue :: String -> CtxValue
handleValue value
    | (isDigit (L.head value)) = intOrDouble value
    | (value == "True") = B True
    | (value == "False") = B False
    | otherwise = S value

intOrDouble :: String -> CtxValue
intOrDouble value = if (elem '.' value) then D (read value) else I (read value)
