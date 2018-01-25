{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Concerns.Redundancy(redundancyUpdate) where

import GHC.Generics
import Data.List
import Data.Maybe

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import Generics.BiGUL.Lib.List

import Concerns.SourceModel as S
import Concerns.Redundancy.Model as R

{-
    This function synchronize the main source (linked with AWS) and
    the view about the redundancy concern.
-}
redundancyUpdate :: BiGUL S.Source R.RView
redundancyUpdate = $(rearrS [| \(Source x y z) -> (x,y,z)|])$
                    $(rearrV [| \(RView x y z) -> (x,y,z)|])$
                    $(update [p|(x,y,z)|] [p|(x,y,z)|] [d|x = alignInstances; y = alignSecurityGroups; z = alignInstanceTypes|])

alignInstances :: BiGUL [Instance] [RInstance]
alignInstances = align
                    -- identify not deleted item
                    (\(Instance _ _ _ state status _ _) -> (state /= 48))
                    -- match between source and view
                    (\(Instance s _ _ _ _ _ _) (RInstance v _ _ _ _) -> s == v)
                    -- Update function
                    $(update [p|Instance identifier instType _ instState instStatus instGroupRef _|] [p|RInstance identifier instType instState instStatus instGroupRef |] [d|identifier = Replace; instType = Replace; instState = Replace; instStatus = Replace; instGroupRef = Replace|])
                    -- Create an item in the source from an item in the view
                    (\(RInstance vId vType vState vStatus vGroupRef) -> Instance vId vType "" vState vStatus vGroupRef 0)
                    -- How to delete an item from the source list
                    (\(Instance sId sType sAmi sState _ sSecurityGroupRef sLoad) -> Just (Instance sId sType sAmi sState 2 sSecurityGroupRef sLoad))

alignSecurityGroups :: BiGUL [SecurityGroup] [RSecurityGroup]
alignSecurityGroups = align
                        -- identify not deleted item
                        (\_ -> True)
                        -- match between source and view
                        (\(SecurityGroup s _ _ _) (RSecurityGroup v _) -> s == v)
                        -- Update function
                        $(update [p|SecurityGroup identifier _ instanceRefs _|] [p|RSecurityGroup identifier instanceRefs|] [d|identifier = Replace; instanceRefs = Replace|])
                        -- Create an item in the source from an item in the view
                        (\(RSecurityGroup vId vRefs) -> SecurityGroup vId "" vRefs [])
                        -- How to delete an item from the source list
                        (\_ -> Nothing)

alignInstanceTypes :: BiGUL [InstanceType] [RInstanceType]
alignInstanceTypes = align
                        -- identify not deleted item
                        (\_ -> True)
                        -- match between source and view
                        (\(InstanceType s _ _ _) (RInstanceType v) -> s == v)
                        -- Update function
                        $(update [p|InstanceType identifier _ _ _|] [p|RInstanceType identifier|] [d|identifier = Replace|])
                        -- Create an item in the source from an item in the view
                        (\(RInstanceType vId) -> InstanceType vId 0 0 0)
                        -- How to delete an item from the source list
                        (\_ -> Nothing)
