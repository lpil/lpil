# Home server

## Setup

5. Install syncthing.
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
