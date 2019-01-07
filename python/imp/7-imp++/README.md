
# Introduction
This directory contains implementations of IMP++, with 5 features added using big step semantics in Python. It could deal with non-determinism and output all the possible evaluation results.

# Directory Structure
- `big_step.py` includes the implementation of the Big-Step semantics.
- `main.py` includes test cases and output the evaluation results of the tests.
- `util.py` includes some helper functions.


# How to run
- Install `Python3`.
- Run `python3 main.py`, which contains all the test cases.


# Thoughts on the Implementation in Python
1. Types. Because type is not that clear for python. This would cause two problems. First, it loses the benefit that types could serve as a mini-document. It may not be straightforward when looking into the parameters and return values. Second, when refactoring the code, programmer need to keep track of the types in their heads, because there would be any compilation errors. 

2. Parameter passing. Python parameter passing is not by copying but reference. So need to explictly deep copy the state, which increase the burden of the programmer.

3. OOP v.s. FP. Need to define classes to represent the AST nodes, whereas in funciontal program one could use algebraic data types. Hence one need to define functions in classes in Python, while one would do pattern matchings in functional languages. Furthermore, functional languages has more support of higher-order functions.

4. Testing. Property-based testings such as QuickCheck are commonly used in functional languages such as Haskell. Python has alternatives like Hypothesis, but not quite clear about the performance. 

# TODO
- Implement the semantic of 'join'.
- Implement parser.
- Refine the testing (now checking manually, without writing down the oracle).
- Better way of doing deep copy on state. 