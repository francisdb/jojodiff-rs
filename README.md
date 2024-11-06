# jojodiff-rs

Rust library for working with JojoDiff files

The original Jojos Binary Diff tool can be found at https://jojodiff.sourceforge.net/
and https://sourceforge.net/projects/jojodiff/

This library is a Rust implementation of the JojoDiff file format. It is intended to be used as a library for other Rust
projects that need to work with JojoDiff files.

Currently, it only supports applying JojoDiff patch files to binary files.

## Logging

This library uses the `log` crate for logging. To enable logging, set the `RUST_LOG` environment variable to `debug`.

## Running tests

```sh
cargo test
```

Running a single test with logging:

```sh
RUST_LOG=debug cargo test patch_copy_seq_3_times
```
