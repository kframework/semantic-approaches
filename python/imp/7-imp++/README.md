
# Introduction
This directory contains implementations of IMP++, with 5 features added using big step semantics in Python. It could deal with non-determinism and output all the possible evaluation results.

# Directory Structure
- `big_step.py` includes the implementation of the Big-Step semantics.
- `main.py` includes test cases and output the evaluation results of the tests.
- `util.py` includes some helper functions.


# How to run
- Install `Python3`.
- Run `python3 main.py`, which contains all the test cases.


# Notes on the Implementation (To Be Extended...)
1. Because type is not that clear for python. So it is hard to constrain the type of the input to each class.
2. Python paramter passing is not by copying. So need to explictly deep copy the state.

# TODO
- Implement the semantic of 'join'.
- Implement parser.
- Refine the testing (now checking manually, without writing down the oracle).