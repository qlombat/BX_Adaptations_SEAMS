# Self-Prioritized Modular Adaptations using Bidirectional Transformations

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Prerequisites

To running our project you need to have installed Haskell, Cabal and BiGUL.
```
sudo apt update
sudo apt install ghc
sudo apt install cabal-install
```

BiGUL works with GHC 7.10 and above, and is released to Hackage, so the installation of the latest release of BiGUL is as simple as executing
```
sudo cabal update
sudo cabal install BiGUL --allow-newer base
```
in the command line (i.e., the standard way of installing Haskell packages). [\[PRL - Bidirectional Programming - BiGUL\]](https://bitbucket.org/prl_tokyo/bigul/overview)

The project needs some Haskell packages to work

Required :
```
sudo cabal install http-conduit
sudo cabal install temporary
sudo apt install awscli
```
Optional (to run benchmark) :
```
sudo cabal install criterion
sudo cabal install deepseq-generics
```

if you have any problem with zlib library you can run the following line
```
sudo sudo apt install zlib1g-dev
```


### Installing
To install our project you have to clone the Git repository as follow

```
git clone https://github.com/qlombat/duduloma.git
```

The useful code to run example is in the following directory
```
cd duduloma/Conflict/AWS/
```

#### Ansible
First, we will install the repository of Ansible

```
sudo apt-get install software-properties-common
sudo apt-add-repository ppa:ansible/ansible
sudo apt-get update
```

Then, we will install the Ansible package
```
sudo apt-get install ansible
```

Ansible needs also Boto3. We have to install pip and Boto.
```
sudo apt-get install python-pip
sudo pip install --upgrade pip
sudo pip install boto3
```
### Using
#### Simple example without AWS
You can run an example to experiment our solution. It is as simple as executing
```
ghci Examples/Example.hs
```
in the command line

to see the result of the synchronization between some concerns depending on the context, please run the following command.
```
*Main> main1
```

The following example, use a web service to execute each concern. These services provide the analysis and planning steps.
For the example these web services will just retrieve the view sent by the synchronizer without any change.
```
*Main> main2
```
#### Simple example with AWS
![#f03c15](https://placehold.it/15/f03c15/000000?text=+) `Be careful the following lines execute some changes on AWS EC2. Please execute them on a test infrastructure`
You can run an example to experiment our solution using AWS. It is as simple as executing
```
ghci Examples/ExampleAws.hs
```
in the command line

to see the result of the synchronization between some concerns depending on the context, please run the following command.
```
*Main> main1
```
The software will ask you some information about you aws configuration. Be sure that put the correct information
It will execute some action to change you architecture according to our configuration. For example, if you have some security groups without running instance, our software will start two instances.

The following example, use a web service to execute each concern. These services provide the analysis and planning steps.
For the example these web services will just retrieve the view sent by the synchronizer without any change.
```
*Main> main2
```

## Authors

* **Jérémy Duchesne** - [JeremyD11](https://github.com/JeremyD11)
* **Quentin Lombat** - [qlombat](https://github.com/qlombat)
