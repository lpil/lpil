#!/bin/sh

# View the logs using the journalctl program on the VM

echo "Starting"

function retry {
  local n=1
  local max=5

  while true; do
    "$@" && break || {
      if [[ $n -lt $max ]]; then
	((n++))
	echo "Command failed. Attempt $n/$max:"
      else
	fail "The command has failed after $n attempts."
      fi
    }
  done
}

retry docker run \
  -d \
  -p 80:80 \
  ${DOCKER_TAG}

echo "Done"
