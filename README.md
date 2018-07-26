# Throne

## Notes

- https://github.com/yobert/alsa/blob/master/cmd/beep/main.go
- https://github.com/tomaka/cpal/blob/master/examples/beep.rs

## Build

For Ubuntu.

```sh
# Install ARM linker
sudo apt-get install gcc-arm-linux-gnueabihf

# Configure Rust build tool to cross compile
rustup target add armv7-unknown-linux-gnueabihf

# Compile binary for ARM
cargo build --target=armv7-unknown-linux-gnueabihf
```
