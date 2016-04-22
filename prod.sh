#!/usr/bin/env bash
set -euo pipefail

export MIX_ENV=prod
export NODE_ENV=production

npm run build
mix compile
mix phoenix.digest
mix phoenix.server --port 80
