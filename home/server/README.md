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
6. Edit fstab to make storage auto-mount to `/mnt/usb0`.
7. Install syncthing.
   1. Install via the [official apt repo](https://apt.syncthing.net/).
   2. Edit the systemd service to remove the `-no-browser` option.
      ```sh
      sudo vim /lib/systemd/system/syncthing@.service
      ```
   3. Start the systemd service that comes with the official package.
      ```sh
      systemctl enable syncthing@louis.service
      systemctl start syncthing@louis.service
      systemctl status syncthing@louis.service
      ```
   4. Port forward so you can access the syncthing GUI locally.
      ```sh
      ssh -L 8000:localhost:8384 cubone
      ```
   5. Set a GUI password.
   6. Set the default folder path to `/mnt/usb0/syncthing`.
   7. Connect to the other devices. Set folders to sync.
8. Install podman.
   Note this requires the external storage to use a filesystem that support
   symlinks.
   1. `sudo apt install podman`.
   2. Copy `/etc/containers/storage.conf` into place.

