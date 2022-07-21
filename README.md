# Skeletal Semantics for Message-Passing Concurrency: From Actors to Channels and Back

Bachelor's Project for the University of Groningen by Miguel Bartelsman.

A detailed description of the development process and underlying theory can be found in the [project thesis document](project-thesis.pdf).

## Contents

This repository contains a Dune project consisting of a library and an executable, and the semantics for the two languge interpreters and the two translators.

### Library

The library is composed of 5 modules: `Actors`, `Channels`, `Act2Ch`, `Ch2Act`, and `Common`. The first four modules expose the functions and types required to make use of the interpreter and translator utilities of the project.

#### `Actors` and `Channels`

- `execute: string -> string`: parses and evaluates a program, producing a string displaying the results of the computation.
- `parse: string -> expr_t`: parses a string into a syntax tree of the langauge
- `evaluate: ?(print:bool) -> expr_t -> (string * expr_t) list`: evaluates a syntax tree, producing a list of pairs containing process IDs and their respective results.
- `serialize: expr_t -> string`: Serializes an expression into a string

#### `Act2Ch` and `Ch2Act`

- `translate: string -> string`: parses and and translates a program from one language into the other.
- `parse: string -> expr_t`: parses a string into a syntax tree of the langauge
- `evaluate: ?(print:bool) -> expr_t -> (string * expr_t) list`: translates a syntax tree of one language into the other
- `serialize: expr_t -> string`: Serializes an expression into a string

#### `Common`

`Common` provides utilities used by the previous four modules, it is not intended for external use.

### Executable

The executable contains a sample program showcasing the capabilties of the library. It can be run with the command `dune exec example`.

### Semantics

The semantics are located in the `semantics/` folder and are organized according to the respective module they were used for.
