#!/bin/bash

cd /var/www/html/duduloma/Conflict/AWS/
git pull
ghc -o AutoScaling Concerns/AutoScaling/main.hs
ghc -o Cost Concerns/Cost/main.hs
ghc -o Firewall Concerns/Firewall/main.hs
ghc -o Redundancy Concerns/Redundancy/main.hs
ghc -o Controller main.hs
ghc -o Controller2 main2.hs
ghc -o Benchmarks benchmarks.hs
ghc -o Benchmarks2 benchmarks2.hs
