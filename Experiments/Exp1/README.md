# Experiment 1

## Getting Started

This experiment run on AWS with 5 server for the controller and concerns.
The web security group contains 2 instances (T2.Nano).
The database security group contains 1 instance (T2.micro)


Execution order of the concerns (From less important to more important)
* Cost (max 0,5/h)
* Firewall
* Autoscaling (avr load 40%)
* Redundancy

Backend architecture
* Each concern has its own server (4 x t2.micro)
* The controller/synchronizer has its own server (1 x t2.micro)
* 1 security group

Frontend architecture
* 2 security groups (for web and database)
* Wordpress (1 x t2.micro)
* MySQL (1 x t2.micro)
* Load balancer for the web security group

Application of the concerns
* All concerns are applied to the web security group
* Only security concern is applied to the database security group

Simulation of the load
* Using jMeter

Our software run every 15minutes
* All t2 instance types available


## Authors

* **Jérémy Duchesne** - [JeremyD11](https://github.com/JeremyD11)
* **Quentin Lombat** - [qlombat](https://github.com/qlombat)
