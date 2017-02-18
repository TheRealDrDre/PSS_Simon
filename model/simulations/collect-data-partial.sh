#!/bin/bash

for i in *.txt; do 
    n=`wc $i | awk '{print $1}'`; 
    m=$((n-10)); 
    head -${m} $i >> partial.txt; 
done
