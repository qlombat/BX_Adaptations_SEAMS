{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Concerns.Firewall.Analysis where

import GHC.Generics
import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import Data.List

import Concerns.Firewall.Model

firewallAnalysis :: FView -> [(FSecurityGroup, Int, Int)]
firewallAnalysis (FView ls) = map handleSecurityGroup ls

handleSecurityGroup :: FSecurityGroup -> (FSecurityGroup, Int, Int)
handleSecurityGroup (FSecurityGroup name rs) =
    if (isInfixOf "web" name) then
        case checkPort22 of
            0 -> case checkPort80 of
                0 -> (FSecurityGroup name rs, 0, 0)
                otherwise -> (FSecurityGroup name rs, 0, 1)
            1 -> case checkPort80 of
                0 -> (FSecurityGroup name rs, 1, 0)
                otherwise -> (FSecurityGroup name rs, 1, 1)
            otherwise -> case checkPort80 of
                0 -> (FSecurityGroup name rs, 2, 0)
                otherwise -> (FSecurityGroup name rs, 2, 1)
    else
        if (isInfixOf "database" name) then
            case checkPort22 of
                0 -> case checkPort3306 of
                    0 -> (FSecurityGroup name rs, 0, 2) -- todo : We have to add port 3306
                    otherwise -> (FSecurityGroup name rs, 0, 3)
                1 -> case checkPort3306 of
                    0 -> (FSecurityGroup name rs, 1, 2) -- todo : We have to add port 3306
                    otherwise -> (FSecurityGroup name rs, 1, 3)
                otherwise -> case checkPort3306 of
                    0 -> (FSecurityGroup name rs, 2, 2) -- todo : We have to add port 3306
                    otherwise -> (FSecurityGroup name rs, 2, 3)
        else case checkPort22 of
            0 -> (FSecurityGroup name rs, 0, 4)
            1 -> (FSecurityGroup name rs, 1, 4)
            otherwise -> (FSecurityGroup name rs, 2, 4)

    where
        checkPort22 = (length (filter (\(FRule _ from to _ _) -> ((from <= Just 22) && (to >= Just 22))) rs))
        checkPort80 = (length (filter (\(FRule o from to _ _) -> ((not o) && (from <= Just 80) && (to >= Just 80))) rs))
        checkPort3306 = (length (filter (\(FRule o from to _ _) -> ((not o) && (from <= Just 3306) && (to >= Just 3306))) rs))
