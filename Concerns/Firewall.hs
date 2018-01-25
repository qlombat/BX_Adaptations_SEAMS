{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Concerns.Firewall(firewallUpdate) where

import Data.Function (on)
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import Generics.BiGUL.Lib.List

import Concerns.SourceModel
import Concerns.Firewall.Model

firewallUpdate :: BiGUL Source FView
firewallUpdate = $(rearrS [| \(Source x y z) -> (y, (x, z)) |])$
                      $(rearrV [| \(FView x) -> (x,()) |])$
                      alignSecurityGroups `Prod` (Skip (const ()))

alignSecurityGroups :: BiGUL [SecurityGroup] [FSecurityGroup]
alignSecurityGroups = align
                    -- identify not deleted item
                    (\_ -> True)
                    -- match between source and view
                    (\(SecurityGroup s _ _ _) (FSecurityGroup v _) -> s == v)
                    -- update function
                    $(update [p| SecurityGroup identifier _ _ rules |] [p| FSecurityGroup identifier rules |] [d| identifier = Replace; rules = alignRules |])
                    -- create an item in the source from an item in the view
                    (\(FSecurityGroup vId _) -> SecurityGroup vId "" [] [])
                    -- how to delete an item from the source list
                    (\_ -> Nothing)

alignRules :: BiGUL [FirewallRule] [FRule]
alignRules = align
                        -- identify not deleted item
                        (\_ -> True)
                        -- match between source and view
                        (\(FirewallRule sOutBound sFrom sTo sIp sProtoc) (FRule vOutBound vFrom vTo vIp vProtoc) -> (sOutBound == vOutBound) && (sFrom == vFrom) && (sTo == vTo) && (sIp == vIp) && (sProtoc == vProtoc))
                        -- update function
                        $(update [p| FirewallRule outbound from to ip protoc |] [p| FRule outbound from to ip protoc |] [d| outbound = Replace; from = Replace; to = Replace; ip = Replace; protoc = Replace |])
                        -- create an item in the source from an item in the view
                        (\(FRule outbound from to ip protoc) -> FirewallRule outbound from to ip protoc)
                        -- how to delete an item from the source list
                        (\_ -> Nothing)
