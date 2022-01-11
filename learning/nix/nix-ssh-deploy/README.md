# A Nix project

I'm not a fan of NixOS' lack of state management, so let's just use a shell
script and SSH to push new config to a server.

Create a NixOS server (possibly using DigitalOcean and nix-infect) and then
run `bin/deploy.sh`.

Run `nix-channel --add https://nixos.org/channels/nixos-19.03 nixos` on the
remote.
