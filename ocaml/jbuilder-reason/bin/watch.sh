#!/bin/sh

# Runs a commend ever time a file is edited
# Requires entr to be installed

while true
do
  ls -d src/* | entr -c -d $@
done
