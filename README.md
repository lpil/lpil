# Throne

## Build

For Ubuntu.

```sh
# Install ARM linker
sudo apt-get install -qq gcc-arm-linux-gnueabihf

# Configure Rust build tool to cross compile
rustup target add armv7-unknown-linux-gnueabihf

# Compile binary for ARM
cargo build --target=armv7-unknown-linux-gnueabihf
```
