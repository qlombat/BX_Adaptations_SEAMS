{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Concerns.Firewall.Plan where

import GHC.Generics
import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib

import Concerns.Firewall.Model
import Concerns.Firewall.Analysis

firewallPlan :: FView -> FView
firewallPlan fv = FView (addSSH (firewallAnalysis fv))

addSSH :: [(FSecurityGroup, Int, Int)] -> [FSecurityGroup]
addSSH ls =
    map (\(FSecurityGroup i rs, port22, webOrDB) -> case (port22, webOrDB) of
        (0, 0) -> (FSecurityGroup i (addSSHRules (addWebRule rs)))
        (0, 1) -> (FSecurityGroup i (addSSHRules rs))
        (0, 2) -> (FSecurityGroup i (addSSHRules rs))
        (0, 3) -> (FSecurityGroup i (addSSHRules (removeDBAccess rs)))
        (0, 4) -> (FSecurityGroup i (addSSHRules rs))
        (1, 0) -> (FSecurityGroup i (addWebRule (addSSHRules (removeSSHRule rs))))
        (1, 1) -> (FSecurityGroup i (addSSHRules (removeSSHRule rs)))
        (1, 2) -> (FSecurityGroup i (addSSHRules (removeSSHRule rs)))
        (1, 3) -> (FSecurityGroup i (removeDBAccess (addSSHRules (removeSSHRule rs))))
        (1, 4) -> (FSecurityGroup i (addSSHRules (removeSSHRule rs)))
        (2, 0) -> (FSecurityGroup i (addWebRule rs))
        (2, 1) -> (FSecurityGroup i rs)
        (2, 2) -> (FSecurityGroup i rs)
        (2, 3) -> (FSecurityGroup i (removeDBAccess rs))
        (2, 4) -> (FSecurityGroup i rs)) ls
    where
        addSSHRules ls = ls ++ [FRule True (Just 22) (Just 22) "0.0.0.0/0" "TCP", FRule False (Just 22) (Just 22) "0.0.0.0/0" "TCP"]
        removeSSHRule ls = filter (\(FRule _ from to _ _) -> not ((from == Just 22) && (to == Just 22))) ls
        addWebRule ls = ls ++ [FRule False (Just 80) (Just 80) "0.0.0.0/0" "TCP"]
        removeDBAccess ls = concat (map (\(FRule o from to ip pr) -> if ((from <= Just 3306) && (to >= Just 3306)) then [FRule o from (Just 3305) ip pr, FRule o (Just 3307) to ip pr] else [(FRule o from to ip pr)]) (filter (\(FRule _ from to _ _) -> not ((from == Just 3306) && (to == Just 3306))) ls))
