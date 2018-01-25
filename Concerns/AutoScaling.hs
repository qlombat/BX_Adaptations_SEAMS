{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Concerns.AutoScaling(autoScalingUpdate) where

import GHC.Generics
import Data.List
import Data.Maybe

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import Generics.BiGUL.Lib.List

import Concerns.SourceModel as S
import Concerns.AutoScaling.Model as AS

{-
    This function synchronize the main source (linked with AWS) and
    the view about the auto scaling concern.
-}
autoScalingUpdate :: BiGUL S.Source AS.ASView
autoScalingUpdate = $(rearrS [| \(Source x z y) -> (x, y, z)|])$
                    $(rearrV [| \(ASView x y) -> (x,y,())|])$
                    $(update [p|(x,y,z)|] [p|(x,y,z)|] [d|x = alignInstances; y = alignInstanceTypes; z = Skip (const ())|])

alignInstances :: BiGUL [Instance] [ASInstance]
alignInstances = align
                    -- identify not deleted item
                    (\_ -> True)
                    -- match between source and view
                    (\(Instance s _ _ _ _ _ _) (ASInstance v _ _ _ _ _) -> s == v)
                    -- Update function
                    $(update [p|Instance identifier instType _ state status sg load|] [p|ASInstance identifier instType state status sg load|] [d|identifier = Replace; instType = Replace; state = Replace; status= Replace; sg= Replace; load= Replace|])
                    -- Create an item in the source from an item in the view
                    (\(ASInstance vId vType _ _ vSG vLoad) -> Instance vId vType "" 0 1 vSG vLoad)
                    -- How to delete an item from the source list
                    (\(Instance sId sType sAmi sState _ sSecurityGroupRef sLoad) -> Just (Instance sId sType sAmi sState 2 sSecurityGroupRef sLoad))



alignInstanceTypes :: BiGUL [InstanceType] [ASInstanceType]
alignInstanceTypes = align
                        -- identify not deleted item
                        (\_ -> True)
                        -- match between source and view
                        (\(InstanceType s _ _ _) (ASInstanceType v _ _ _) -> s == v)
                        -- Update function
                        $(update [p|InstanceType identifier typeCPUs typeRAM typeCost|] [p|ASInstanceType identifier typeCPUs typeRAM typeCost|] [d|identifier = Replace; typeCPUs = Replace; typeRAM= Replace; typeCost= Replace|])
                        -- Create an item in the source from an item in the view
                        (\(ASInstanceType vId vTypeCPUs vTypeRAM vTypeCost) -> InstanceType vId vTypeCPUs vTypeRAM vTypeCost)
                        -- How to delete an item from the source list
                        (\_ -> Nothing)
