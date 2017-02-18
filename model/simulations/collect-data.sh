#!/bin/bash

rm final.txt
for i in *.txt; do 
    cat $i >> simulations.txt; 
done
