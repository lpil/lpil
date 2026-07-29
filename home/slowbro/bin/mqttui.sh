#!/bin/sh

#
# Run mqttui, a TUI for viewing mqtt traffic
#

set -eu

. ./src/secrets.env

export MQTTUI_PASSWORD="$MOSQUITTO_LOUIS_PASSWORD"
mqttui --broker mqtt://slowbro --username louis
