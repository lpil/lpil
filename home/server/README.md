# Home server

## Setup

1. Add ssh public key to `~/.ssh/authorized_keys` on the server.
2. Disable ssh password login.
3. [Install tailscale](https://tailscale.com/kb/1031/install-linux/). Set up as exit node.
4. [Install k3s](https://k3s.io/) without traefik. `curl ... | sh -s - --disable=traefik`.
   You may need to add some config to a boot file for this to work. Details are printed
   to the systemd journal.
5. Copy the config from `/etc/rancher/k3s/k3s.yaml` to your local machine, replacing
   usernames and addresses as appropriate.
