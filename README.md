# The Pyro programming language

This repo contains a compiler for the Pyro programming language.
It is probably best described as a C-like language, but that may change as I am designing the language on the fly while writing it.

At the present moment, the compiler is neither mature nor well documented.

# Why a new programming language?
I am writing this compiler mainly to get better at writing Rust and as an effort to learn LLVM as an IR.

It is not intended to be a replacement for any "real" programming language.


---

# If you happen to want to use this repo

As stated above, this is not exactly intended to be a production level language but if you find that you want to fool around with `Pyro` for some reason, follow these steps:

## Prerequisites
The code in this Repo depends on having LLVM 14.0 installed on your system and a nightly version of rust.

It also contains pre-commit hooks created using [pre-commit](https://pre-commit.com) which is therefore required to run the pre-commit hooks.


## Building the compiler
To build the repo, run `cargo build` to create the binaries.

It is not recommended to actually install the compiler to your system at this time as it is not stable.

## Running the compiler
After building, run the compiler using `./target/debug/pyro <pyro-program.pyro>`.

There are a few example programs available in the `test_programs` folder that can be used as inspiration.
