{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Concerns.Cost(costUpdate) where

import GHC.Generics
import Data.List
import Data.Maybe

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import Generics.BiGUL.Lib.List

import Concerns.SourceModel
import Concerns.Cost.Model

costUpdate :: BiGUL Source CView
costUpdate = $(rearrS [| \(Source x y z) -> (x,z,y) |])$
                      $(rearrV [| \(CView x y) -> (x,y,()) |])$
                      $(update [p| (x,y,z) |] [p| (x,y,z) |] [d| x = alignInstances; y = alignInstanceTypes; z = Skip (const ()) |])

alignInstances :: BiGUL [Instance] [CInstance]
alignInstances = align
                    -- identify not deleted item
                    (\(Instance _ _ _ state status _ _) -> (state == 16 && status /= 2) || (state /= 16 && status == 1))
                    -- match between source and view
                    (\(Instance s _ _ _ _ _ _) (CInstance v _ _) -> s == v)
                    -- update function
                    $(update [p| Instance identifier instType _ _ _ _ instLoad |] [p| CInstance identifier instType instLoad |] [d| identifier = Replace; instType = Replace; instLoad = Replace |])
                    -- create an item in the source from an item in the view
                    (\(CInstance vId vType vLoad) -> Instance vId vType "" 0 1 "" vLoad)
                    -- how to delete an item from the source list
                    (\(Instance sId sType sAmi sState _ sSecurityGroupRef sLoad) -> Just (Instance sId sType sAmi sState 2 sSecurityGroupRef sLoad))

alignInstanceTypes :: BiGUL [InstanceType] [CInstanceType]
alignInstanceTypes = align
                        -- identify not deleted item
                        (\_ -> True)
                        -- match between source and view
                        (\(InstanceType s _ _ _) (CInstanceType v _) -> s == v)
                        -- update function
                        $(update [p| InstanceType identifier _ _ typeCost |] [p| CInstanceType identifier typeCost |] [d| identifier = Replace; typeCost = Replace |])
                        -- create an item in the source from an item in the view
                        (\(CInstanceType vId vTypeCost) -> InstanceType vId 0 0 vTypeCost)
                        -- how to delete an item from the source list
                        (\_ -> Nothing)
