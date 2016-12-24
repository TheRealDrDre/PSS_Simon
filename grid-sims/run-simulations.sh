#!/bin/bash

for script in grid-search*.lisp; do
    nice sbcl --load ${script} &
done
