{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings, ExistentialQuantification, StandaloneDeriving, DeriveGeneric, DeriveAnyClass #-}

module Lib.Synchronizer(executeSeq,Concern(..), MasterView(..), orderConcern,execBranch) where

import Data.List
import GHC.Generics (Generic, Generic1)
import Control.DeepSeq
import Control.Monad
import Data.Maybe
import Data.Aeson
import Data.Aeson.TH

import Network.HTTP.Simple
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as Char8

import Generics.BiGUL
import Generics.BiGUL.Interpreter

import Lib.Synchronizer.Context
import Lib.Synchronizer.RuleBase

data MasterView = forall v. (ToJSON v, Show v, NFData v) => MasterView v
deriving instance Show MasterView
instance NFData MasterView where
    rnf (MasterView v) = rnf v

type ConcernName = String
type Url = String
type Port = Int
type Secured = Bool
type Serializer v = (v -> String)
type Deserializer v = (String -> v)

data Concern s  = forall v. (ToJSON v, Show v, NFData v) => Concern ConcernName (BiGUL s v) (v -> v)
                | forall v. (ToJSON v, Show v, NFData v) => ConcernRemote ConcernName (BiGUL s v) Url Port Secured (Serializer v) (Deserializer v)
instance Eq (Concern s) where
    (Concern name1 _ _) == (Concern name2 _ _) = name1 == name2
    (ConcernRemote name1 _ _ _ _ _ _) == (ConcernRemote name2 _ _ _ _ _ _) = name1 == name2
instance Show (Concern s) where
    show (Concern name _ _) = show name
    show (ConcernRemote name _ _ _ _ _ _) = show name
getConcernName :: Concern s -> String
getConcernName (Concern name _ _) = name
getConcernName (ConcernRemote name _ _ _ _ _ _) = name

execBranch :: s -> Concern s -> IO (s,MasterView)
execBranch source (Concern _ bx analysisAndPlan) = do
        let view = get bx source
        viewUpdated <- return (case view of
            Just x -> analysisAndPlan x
            otherwise -> error "Impossible to generate the view with the bidirectional tranformation given")
        sourceUpdated <- return (case put bx source viewUpdated of
            Just x  -> x
            otherwise -> error "Impossible to generate the updated source with the bidirectional tranformation given")
        return (sourceUpdated, MasterView viewUpdated)

execBranch source (ConcernRemote _ bx url port secured serialize unserialize) = do
        let view = get bx source
        viewUpdatedStr <- case view of
            Just x -> postRequest url port secured (serialize x)
            otherwise -> error "Impossible to generate the view with the bidirectional tranformation given"
        viewUpdated <- return (unserialize viewUpdatedStr)
        sourceUpdated <- return (case put bx source viewUpdated of
            Just x  -> x
            otherwise -> error "Impossible to generate the updated source with the bidirectional tranformation given")
        return (sourceUpdated, MasterView viewUpdated)


orderConcern :: [String] -> [Concern s] -> [Concern s]
orderConcern [] [] = []
orderConcern (x:xs) (concern:ys)    | x == (getConcernName concern) = concern:(orderConcern xs ys)
                                    | otherwise = case find (\c -> (getConcernName c) == x) ys of
                                        Just x -> x:(orderConcern xs (concern:(delete x ys)))
                                        Nothing -> error ("The concern " ++ (getConcernName concern) ++ " is not implemented")
orderConcern x y = error ("orderConcern x y : impossible case " ++ show x ++ " " ++ show (map getConcernName y))

executeSeq :: Context -> Rules -> PriorityPolicy -> s -> [Concern s] -> IO (s, [MasterView])
executeSeq ctx rules priorityPolicy source concerns = do
    let concernNames = map getConcernName concerns
    let order = determineOrder ctx rules concernNames priorityPolicy
    let concernsOrdered = reverse (orderConcern order concerns)
    res <- foldM fn (source, []) concernsOrdered
    return res
    where
        fn (accS, accV) concern = do
            branchResult <- execBranch accS concern
            return (fst branchResult, accV ++ [(snd branchResult)])

postRequest :: String -> Int -> Bool -> String -> IO String
postRequest url port secured body = do
    request' <- parseRequest ("POST " ++ url)
    let request
            = setRequestMethod "POST"
            $ setRequestBody (RequestBodyLBS (Char8.pack body))
            $ setRequestSecure secured
            $ setRequestPort port
            $ request'

    mgr <- newManager tlsManagerSettings
    let req = request { responseTimeout = responseTimeoutNone }
    response <- Network.HTTP.Conduit.httpLbs req mgr
    return $ Char8.unpack (getResponseBody response)
