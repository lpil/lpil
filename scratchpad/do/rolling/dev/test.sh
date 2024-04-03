#!/bin/bash

while true
do
  version=$(curl --max-time 1 --silent $1 --stderr -)

  if [ "$version" == "" ]; then
    echo "$(date -u) Service Unavailable"
  else
    echo "$(date -u) $version"
  fi

  sleep 0.1
done
