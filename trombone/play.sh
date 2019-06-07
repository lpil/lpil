#!/bin/sh

cargo run | aplay -c 2 -f S16_LE -r 44100
