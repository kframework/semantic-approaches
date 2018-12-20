# Introduction

This directory contains both code and tests for the big-step semantics written in Racket.

It includes a parser and interpreter for imp++ programs.

Currently the search functionality is not implemented. This will require some non-trivial refactoring of the current code.

Tests with non-determinism will randomly choose a path when it needs to make a choice. Running the tests repeatedly will show different results.

Additionally, thread joining is not implemented. Because of how big-step works, joining threads will requires new rules that "look ahead" whether there's a join in the future or not. If there is, then the whole spawn block will have to wait after the join.

# Installation

If you already have Racket, make sure you have the **brag** package installed. It's used for generating the parser.

These instructions are tested on Ubuntu.

1. Download and install racket

   ```
   sudo aptitude install racket
   ```

2. Install the **brag** package

   ```
   raco pkg install brag
   ```

If you're using another type of OS, the instructions [here](https://docs.racket-lang.org/getting-started/index.html) are easy to follow.

You would just install DrRacket and install **brag** through its user interface.

# Testing

There are 5 tests now
* sum.rkt -- tests while loops and increment
* spawn.rkt -- tests spawning, output, and halting
* non-deterministic-add.rkt -- tests non-determinism of `+` and division by 0
* non-deterministic-div.rkt -- tests non-determinism of `/` and input
* local-var.rkt -- tests variable shadowing

To run a test, use the command

```
racket [filename]
```

Otherwise, load the file with DrRacket and click the **Run** button.
