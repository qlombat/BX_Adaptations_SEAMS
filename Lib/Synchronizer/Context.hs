{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings, DeriveAnyClass, StandaloneDeriving,DeriveGeneric #-}
module Lib.Synchronizer.Context(CtxValue(..), CtxKey, Context) where

import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics
import Control.DeepSeq


data CtxValue = I Int | D Double | S String | B Bool deriving (Eq, Ord, Generic)
deriving instance NFData CtxValue

instance Show CtxValue where
    show (I x) = show x
    show (D x) = show x
    show (S x) = show x
    show (B x) = show x

type CtxKey = String
type Context = Map CtxKey CtxValue
