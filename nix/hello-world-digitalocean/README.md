# DigitalOcean Nix Hello World

```sh
export DIGITAL_OCEAN_AUTH_TOKEN=token-goes-here

# Create "network"
nixops create droplet.nix -d hello-do

# Create DO resources
nixops deploy -d hello-do

# Check resources
nixops info -d hello-do

# Teardown resources
nixops destroy -d hello-do

# Delete "network"
nixops delete -d hello-do
```

So how do we share state files? :/
