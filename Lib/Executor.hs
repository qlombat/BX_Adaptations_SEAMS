{-# LANGUAGE FlexibleContexts,  TemplateHaskell,  TypeFamilies, OverloadedStrings #-}

module Lib.Executor(executeAWS) where

import Data.Text as T
import qualified Data.Map
import Data.List as L
import Data.Maybe
import System.Process
import System.IO
import System.IO.Temp
import Data.Char (toLower)


import Concerns.SourceModel

splitLines :: String -> [String]
splitLines str = L.reverse (L.foldl (\(x:xs) s -> if s == '\n' then []:x:xs else (x ++ [s]):xs) [[]] str)

indent :: String -> String
indent str =  L.concat (L.map (\x -> "  " ++ x ++ "\n") (splitLines str))

indentN :: Int -> String -> String
indentN 0 str = str
indentN x str = indentN (x-1) (indent str)


sourceToAnsibleTasks :: Source -> String
sourceToAnsibleTasks (Source instances sgs _) =
    securityGroupsStr ++
    instancesToRunStr ++
    newWebInstancesStr ++
    newOtherInstancesStr ++
    idsInstancesToStopStr
    where
        newInstances = L.filter isNewInstance instances

        groupBySecurityGroupAndInstanceType :: [Instance] -> [(String, String, [Instance])]
        groupBySecurityGroupAndInstanceType [] = []
        groupBySecurityGroupAndInstanceType (x@(Instance _ instType _ _ _ sg _):xs) = (sg,instType, L.filter (\(Instance _ instType2 _ _ _ sg2 _) -> sg == sg2 && instType == instType2) (x:xs)):(groupBySecurityGroupAndInstanceType (L.filter (\(Instance _ instType2 _ _ _ sg2 _) -> not (sg == sg2 && instType == instType2))  xs))

        webNewInstances = L.filter (\(Instance _ _ _ _ _ sg _) -> L.isInfixOf "web" (Prelude.map Data.Char.toLower sg)) newInstances
        otherNewInstance = L.filter (\(Instance _ _ _ _ _ sg _) -> not (L.isInfixOf "web" (Prelude.map Data.Char.toLower sg))) newInstances
        existingInstances = L.filter (\x -> not (isNewInstance x)) instances
        idsInstancesToRun = L.map getInstanceId (L.filter isInstanceToRun instances)
        idsInstancesToStop = L.map getInstanceId (L.filter isInstanceToSop instances)

        securityGroupsStr = L.concat (
            L.map (\sg ->
                (indentN 2 ("- name: Security group\n" ++
                    (indent ("ec2_group:\n" ++
                        (indent (securityGroupToAnsibleTask sg))))))) sgs)

        newWebInstancesStr = L.concat (
            L.map (\(sg,instType,insts) ->
                (indentN 2 ("- name: Create new instance\n" ++
                    (indent ("ec2:\n" ++
                        (indent (instanceToAnsibleTask (sg,instType,L.length insts)))++
                    "register: ec2\n")) ++
                    registerInstancesToLoadBalancerToAnsibleTask))) (groupBySecurityGroupAndInstanceType webNewInstances))

        newOtherInstancesStr = L.concat (
            L.map (\(sg,instType,insts) ->
                (indentN 2 ("- name: Create new instance\n" ++
                    (indent ("ec2:\n" ++
                        (indent (instanceToAnsibleTask (sg,instType,L.length insts)))))))) (groupBySecurityGroupAndInstanceType otherNewInstance))

        instancesToRunStr = if idsInstancesToRun /= [] then
            (indentN 2 ("- name: run instances\n" ++
                (indent ("ec2:\n" ++
                    (indent (existingInstanceToAnsibleTask idsInstancesToRun "running"))))))
            else ""

        idsInstancesToStopStr = if idsInstancesToStop /= [] then
            (indentN 2 ("- name: stop instances\n" ++
                (indent ("ec2:\n" ++
                    (indent (existingInstanceToAnsibleTask idsInstancesToStop "stopped"))))))
            else ""


getInstanceId :: Instance -> String
getInstanceId (Instance identifier _ _ _ _ _ _) = identifier

isNewInstance :: Instance -> Bool
isNewInstance (Instance "" _ _ _ 1 _ _) = True
isNewInstance _ = False

isInstanceToRun :: Instance -> Bool
isInstanceToRun (Instance "" _ _ _ 1 _ _) = False
isInstanceToRun (Instance _ _ _ _ 1 _ _) = True
isInstanceToRun _ = False

isInstanceToSop :: Instance -> Bool
isInstanceToSop (Instance "" _ _ _ 2 _ _) = False
isInstanceToSop (Instance _ _ _ _ 2 _ _) = True
isInstanceToSop _ = False

instanceToAnsibleTask :: (String, String, Int) -> String
instanceToAnsibleTask (sg, instType, nb) =
    createAnsibleTask ++
    "instance_type: " ++ instType ++ "\n" ++
    "count: " ++ show nb ++ "\n" ++
    "instance_tags: {\"BiGUL\":\"CloudBx\"}\n" ++
    "group: " ++ sg ++ "\n" ++
    "image: " ++ "\"{{image}}\"" ++ "\n" ++
    "monitoring: yes\n" ++
    "key_name: \"{{keypair}}\"\n" ++
    "wait: yes\n"

registerInstancesToLoadBalancerToAnsibleTask :: String
registerInstancesToLoadBalancerToAnsibleTask =
    "- name: Add each EC2 instance to the ELB\n" ++
    indent("ec2_elb:\n" ++
        indent(createAnsibleTask ++
        "state: present\n" ++
        "ec2_elbs: \"{{ aws_load_balancer }}\"\n" ++
        "wait: no\n" ++
        "instance_id: \"{{ item.id }}\"\n") ++
    "with_items: \"{{ ec2.instances }}\"")

securityGroupToAnsibleTask :: SecurityGroup -> String
securityGroupToAnsibleTask (SecurityGroup name description instsRef fwRules) =
    "name: " ++ name ++ "\n" ++
    "description: "++ description ++"\n" ++
    "tags: {\"BiGUL\":\"CloudBx\"}\n" ++
    "aws_access_key: \"{{aws_access_key}}\"\n" ++
    "aws_secret_key: \"{{aws_secret_key}}\"\n" ++
    "region: \"{{region}}\"\n" ++
    "rules: \n" ++
    indent(L.concat (L.map rulesToAnsibleTask (fst (rules fwRules)))) ++
    "rules_egress: \n" ++
    indent(L.concat (L.map rulesToAnsibleTask (snd (rules fwRules))))
    where
        rules :: [FirewallRule] -> ([FirewallRule],[FirewallRule])
        rules = L.foldl (\(inbound, outbound) rule@(FirewallRule x _ _ _ _) -> if x then (inbound, rule:outbound) else (rule:inbound, outbound)) ([],[])
        rulesToAnsibleTask ::FirewallRule -> String
        rulesToAnsibleTask (FirewallRule _ fromPort toPort ip protocol) =
            "- proto: " ++ protocol ++"\n" ++
            case fromPort of
                Just p -> "  from_port: "++ show p ++"\n"
                Nothing -> ""
            ++
            case toPort of
                Just p -> "  to_port: "++ show p ++"\n"
                Nothing -> ""
            ++
            "  cidr_ip: "++ ip ++"\n"

existingInstanceToAnsibleTask :: [String]-> String -> String
existingInstanceToAnsibleTask ids state = createAnsibleTask ++
    "instance_ids: [" ++ (L.intercalate "," ids) ++ "]\n" ++
    "state: " ++ state ++ "\n" ++
    "wait: yes\n" ++
    "monitoring: yes\n"

createAnsibleTask :: String
createAnsibleTask =
    "aws_access_key: \"{{aws_access_key}}\"\n" ++
    "aws_secret_key: \"{{aws_secret_key}}\"\n" ++
    "region: \"{{region}}\"\n"

generateMainTask :: String -> String -> String -> String -> String -> String -> String
generateMainTask access secret region keypair image lb = "- name: Main task\n" ++
 indent ("gather_facts: False\nhosts: localhost\nvars:\n" ++
    indent ("keypair: "++ keypair ++
        "\nimage: "++ image ++
        "\nregion: "++ region ++
        "\naws_access_key: "++ access ++
        "\naws_secret_key: "++ secret ++
        "\naws_load_balancer: "++ lb ++"\n") ++
    "tasks:\n")


executeAWS :: String -> String -> String -> String -> String -> String -> Source -> IO ()
executeAWS access secret region keypair image lb src = do
    template <- return (generateMainTask access secret region keypair image lb)
    tmpFilePath <- writeSystemTempFile "ansible.yaml" (template ++ (sourceToAnsibleTasks src))
    _ <- readProcess "ansible-playbook" [tmpFilePath] []
    return ()
