# Report
## Source Instances (total: 5, running: 3 )
```
Instance "i-0991eb164305b1acf" "t2.nano" "ami-1955cc7f" 48 0 "" 0.0
Instance "i-00028def2128ad361" "t2.medium" "ami-1955cc7f" 16 0 "CloudBx-Web" 0.8380158377327029
Instance "i-06bd6e8a33d4b3492" "t2.xlarge" "ami-1955cc7f" 16 0 "CloudBx-Web" 0.5416111246179497
Instance "i-0cb9a14f8d2665361" "t2.micro" "ami-bec974d8" 16 0 "CloudBx-Database" 0.6883333333333335
Instance "i-05ed6e5c18e4ffbcd" "t2.nano" "ami-4ebe2228" 48 0 "" 0.0
```

## Source firewall (2)
```
SecurityGroup "CloudBx-Database" "This security group is used  for experimentations" ["i-0cb9a14f8d2665361"] [FirewallRule False (Just 22) (Just 22) "0.0.0.0/0" "tcp",FirewallRule False (Just 3306) (Just 3306) "0.0.0.0/0" "tcp",FirewallRule True Nothing Nothing "0.0.0.0/0" "-1",FirewallRule True (Just 22) (Just 22) "0.0.0.0/0" "tcp"]
SecurityGroup "CloudBx-Web" "This security group is used  for experimentations" ["i-06bd6e8a33d4b3492","i-00028def2128ad361"] [FirewallRule False (Just 80) (Just 80) "0.0.0.0/0" "tcp",FirewallRule False (Just 0) (Just 65535) "0.0.0.0/0" "tcp",FirewallRule False (Just 22) (Just 22) "0.0.0.0/0" "tcp",FirewallRule True Nothing Nothing "0.0.0.0/0" "-1",FirewallRule True (Just 22) (Just 22) "0.0.0.0/0" "tcp"]
```


## New source Instances (total: 6, running: 3 )
```
Instance "i-05ed6e5c18e4ffbcd" "t2.nano" "ami-4ebe2228" 48 0 "" 0.0
Instance "i-0991eb164305b1acf" "t2.nano" "ami-1955cc7f" 48 0 "" 0.0
Instance "i-06bd6e8a33d4b3492" "t2.xlarge" "ami-1955cc7f" 16 0 "CloudBx-Web" 0.5416111246179497
Instance "i-00028def2128ad361" "t2.medium" "ami-1955cc7f" 16 0 "CloudBx-Web" 0.8380158377327029
Instance "" "t2.2xlarge" "" 0 2 "CloudBx-Web" 0.0
Instance "i-0cb9a14f8d2665361" "t2.micro" "ami-bec974d8" 16 0 "CloudBx-Database" 0.6883333333333335
```

## New source firewall (2)
```
SecurityGroup "CloudBx-Database" "This security group is used  for experimentations" ["i-0cb9a14f8d2665361"] [FirewallRule False (Just 22) (Just 22) "0.0.0.0/0" "tcp",FirewallRule False (Just 3306) (Just 3306) "0.0.0.0/0" "tcp",FirewallRule True Nothing Nothing "0.0.0.0/0" "-1",FirewallRule True (Just 22) (Just 22) "0.0.0.0/0" "tcp"]
SecurityGroup "CloudBx-Web" "This security group is used  for experimentations" ["i-06bd6e8a33d4b3492","i-00028def2128ad361"] [FirewallRule False (Just 80) (Just 80) "0.0.0.0/0" "tcp",FirewallRule False (Just 0) (Just 65535) "0.0.0.0/0" "tcp",FirewallRule False (Just 22) (Just 22) "0.0.0.0/0" "tcp",FirewallRule True Nothing Nothing "0.0.0.0/0" "-1",FirewallRule True (Just 22) (Just 22) "0.0.0.0/0" "tcp"]
```

## Rules triggered:
```
(AdvertisingRevenue == 0,["Cost","Redundancy","AutoScaling","Firewall"])
(AdvertisingRevenue <= 200,["Cost",*])
((HourOfDay > 6) and (HourOfDay < 20),["AutoScaling",*,"Cost"])
```

## Concerns execution order :
* Firewall
```
MasterView (FView [FSecurityGroup "CloudBx-Database" [FRule False (Just 22) (Just 22) "0.0.0.0/0" "tcp",FRule False (Just 3306) (Just 3306) "0.0.0.0/0" "tcp",FRule True Nothing Nothing "0.0.0.0/0" "-1",FRule True (Just 22) (Just 22) "0.0.0.0/0" "tcp"],FSecurityGroup "CloudBx-Web" [FRule False (Just 80) (Just 80) "0.0.0.0/0" "tcp",FRule False (Just 0) (Just 65535) "0.0.0.0/0" "tcp",FRule False (Just 22) (Just 22) "0.0.0.0/0" "tcp",FRule True Nothing Nothing "0.0.0.0/0" "-1",FRule True (Just 22) (Just 22) "0.0.0.0/0" "tcp"]])
```
* AutoScaling
```
MasterView (ASView [ASInstance "i-05ed6e5c18e4ffbcd" "t2.nano" 48 0 "" 0.0,ASInstance "i-0991eb164305b1acf" "t2.nano" 48 0 "" 0.0,ASInstance "" "t2.2xlarge" 0 1 "CloudBx-Web" 0.0,ASInstance "i-00028def2128ad361" "t2.medium" 16 0 "CloudBx-Web" 0.8380158377327029,ASInstance "i-06bd6e8a33d4b3492" "t2.xlarge" 16 0 "CloudBx-Web" 0.5416111246179497] [ASInstanceType "t2.nano" 1 0.5 5.8e-3,ASInstanceType "t2.micro" 1 1.0 1.16e-2,ASInstanceType "t2.small" 1 2.0 2.3e-2,ASInstanceType "t2.medium" 2 4.0 4.64e-2,ASInstanceType "t2.large" 2 8.0 9.28e-2,ASInstanceType "t2.xlarge" 4 16.0 0.1856,ASInstanceType "t2.2xlarge" 8 32.0 0.3712])
```
* Redundancy
```
MasterView (RView [RInstance "" "t2.2xlarge" 0 1 "CloudBx-Web",RInstance "i-00028def2128ad361" "t2.medium" 16 0 "CloudBx-Web",RInstance "i-06bd6e8a33d4b3492" "t2.xlarge" 16 0 "CloudBx-Web"] [RSecurityGroup "CloudBx-Web" ["i-06bd6e8a33d4b3492","i-00028def2128ad361"]] [RInstanceType "t2.nano",RInstanceType "t2.micro",RInstanceType "t2.small",RInstanceType "t2.medium",RInstanceType "t2.large",RInstanceType "t2.xlarge",RInstanceType "t2.2xlarge"])
```
* Cost
```
MasterView (CView [CInstance "i-06bd6e8a33d4b3492" "t2.xlarge" 0.5416111246179497,CInstance "i-00028def2128ad361" "t2.medium" 0.8380158377327029] [CInstanceType "t2.nano" 5.8e-3,CInstanceType "t2.micro" 1.16e-2,CInstanceType "t2.small" 2.3e-2,CInstanceType "t2.medium" 4.64e-2,CInstanceType "t2.large" 9.28e-2,CInstanceType "t2.xlarge" 0.1856,CInstanceType "t2.2xlarge" 0.3712])
```
