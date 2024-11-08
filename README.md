# jojodiff-rs

Rust library for working with JojoDiff files

The original Jojos Binary Diff tool can be found at https://jojodiff.sourceforge.net/
and https://sourceforge.net/projects/jojodiff/

This library is a Rust implementation of the JojoDiff file format. It is intended to be used as a library for other Rust
projects that need to work with JojoDiff files.

Currently, it only supports applying JojoDiff patch files to binary files.

## Usage

```
cargo add jojodiff
```

* Check the docs at https://docs.rs/jojodiff
* Check the unit tests in src/lib.rs for examples.

## Logging

This library uses the `log` crate for logging. See https://docs.rs/log/latest/log/ for more information on how to select
a logger implementation and configuring it.

## Running tests

```sh
cargo test
```

Running a single test with logging:

```sh
RUST_LOG=debug cargo test patch_copy_seq_3_times
```

## JojoDiff patch file format

The JojoDiff patch file format is a binary stream containing operands that describe how to transform an original file
into a destination file.

```
The stream consists of a series of
  <op> (<data> || <len>)
where
  <op>   = <ESC> (<MOD>||<INS>||<DEL>||<EQL>||<BKT>)
  <data> = <chr>||<ESC><ESC>
  <chr>  = any byte different from <ESC><MOD><INS><DEL><EQL> or <BKT>
  <ESC><ESC> yields one <ESC> byte
```

Where:

* `<ESC>` (`0xA7`) escape: Indicates that the next byte is an operand.
* `<MOD>` (`0xA6`) modify: Replaces bytes from the original file with bytes from the patch file.
* `<INS>` (`0xA5`) insert: Inserts bytes from the patch file into the destination file.
* `<DEL>` (`0xA4`) delete: Skips bytes from the original file.
* `<EQL>` (`0xA3`) equal: Copies bytes from the original file to the destination file.
* `<BKT>` (`0xA2`) backtrack: Seeks backwards in the original file.
