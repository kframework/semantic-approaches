# Approaches to program semantics

This project offers a comprehensive survey on the 10 most common-used approaches to give programs formal semantics. 

# Languages

* Ocaml
* Haskell
* Maude
* Racket
* Python
* Prolog
* Java 
* C/C++

# Semantic approaches

* Big-step SOS
* Small-step SOS
* Denotaional semantics
* MSOS
* Reduction semantics with evaluation contexts
* CK
* CEK
* SECD
* SCC
* CHAM

# Experiment plan

## An imperative language

1. Start with IMP without `while`. Ask vonlunteers to add the semantics of `while`. When everybody is done, freeze a canonical version of the semantics and call the language IMP for all.
2. Add incremental statement `++x` to IMP.
3. Add I/O with `read()` and `print(e)` to IMP (at 1, not 2).
4. Add abrupt termination with division-by-zero `error` and `halt;` to IMP.
5. Add threads with `spawn(code)` and `join;` to IMP.
6. Add local variable declarations to IMP.
7. Combine 2-6 all together, and call the language IMP++.

## A functional language

1. Start with LAMBDA with basic constructs, including `\lambda` and `\mu` binders, application, substitution, and builtins.
2. Add concurrency with `spawn` and `join` to LAMBDA.
3. Add `call/cc` to LAMBDA.
4. Combine 2-3 all together, and call the language LAMBDA++.

## Remarks

* Tests are very important. We will provide a comprehensive set of programs that test every corner cases of the semantics.
* Syntax of the languages are very important. We will provide it for every language construct.
* Nondeterminism behavior is very important. The ability to search for all possible execution results is required. For some semantic approaches such as big-step SOS and denotational semantics, ~full nondeterminism~ is almost impossible in practice, but at least the ~choice nondeterminism~ should be handled properly.
* We require two artifacts:
  1. Semantics implementation should pass all provided tests.
  2. An essay that describes and scores every semantic approach with explanation and justification.

# For those who take CS 522 Fall 2018

To fulfill the requirement, you should
* pick an implementation language
* pick a semantic style from either big-step or small-step SOS
* implement the semantics of IMP++ directly.

The following is a list of students who participate in the project. If you are taking the project but your name is not in the following list, please add it yourself.

* Armrin: big-step in Racket.
* Binzhe: TBD in Python.
* Nick: TBC in Haskell.
* Siwakorn: small-step in Racket.
* Xiaohong: big-step in OCaml.

Please put your code in folders. Every implementation language should have its own folder. 
