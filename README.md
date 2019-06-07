# Throne

## Rust build

For Ubuntu.

```sh
# Install ARM linker
sudo apt-get install gcc-arm-linux-gnueabihf

# Configure Rust build tool to cross compile
rustup target add armv7-unknown-linux-gnueabihf

# Compile binary for ARM
cargo build --target=armv7-unknown-linux-gnueabihf
```
