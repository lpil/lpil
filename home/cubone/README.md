# Home server

Run `bin/remotely-update.sh`.

You will need to edit the `REMOTE` variable the first time due to tailscale not
yet being installed.

## macos nfs mount

```
sudo mount -o resvport -o nolocks -t nfs cubone:/home/louis/media $HOME/media
```
