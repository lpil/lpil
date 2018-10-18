#!/usr/bin/env bash

mkdir -p "$HOME/.local/share/test-internet/"
LOG_PATH="$HOME/.local/share/test-internet/log.csv"

if [ ! -f $LOG_PATH ]; then
  speedtest --csv-header > $LOG_PATH
fi

if RESULT=$(speedtest --csv); then
  echo "$RESULT" >> $LOG_PATH
fi
