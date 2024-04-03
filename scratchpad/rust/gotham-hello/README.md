<p align="center">
  <img src="img/happylabs-logo.png" alt="HappyLabs logo">
</p>

This microservice written in Rust provides the REST API for HappyLabs!

It features:

- A super fast asynchronous HTTP2 API using Rust's Hyper and Warp web
  libraries.
- Tracing and logging using Rust's Tracing library.
- Compilation to a statically linked binary within a tiny scratch docker
  image.


## Quick reference

```sh
# Install the database migration tool
cargo install diesel_cli --no-default-features --features postgres

# Load the environment variables
source .env

# Create the database
diesel setup

# Run the tests
cargo test

# Run the application
cargo run
```
