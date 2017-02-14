#!/bin/bash
## -------------------------------------------------------------- ##
## Kills all the sbcl processes
## -------------------------------------------------------------- ##

for process in `ps -A | grep sbcl | awk '{print $1}'`; do
    echo -n "Killing process ${process}..."
    kill ${process}
    echo "Done"
done
