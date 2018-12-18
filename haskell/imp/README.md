# Table Of Contents

* [Big-Step Semantics](#big-step-semantics)
	* [File Info](#file-info)
	* [How to Run the Tests](#how-to-run-the-tests)
	* [Syntax Difference](#syntax-difference)
	* [Thoughts on the Implementation](#thoughts-on-the-implementation)

# Big-Step Semantics

This directory contains implementations of IMP, with 5 features added individually and then all combined at once using big step semantics in Haskell.

## File Info

In each folder there are certain files:
* `Imp_State.hs` 		defines convenience functions for a state, or a key value mapping.
* `Imp_Buffer.hs` 	(in 2 and 6) defines convenience functions for a buffer, or a list.
* `Program.hs` 		contains test programs.
* `Parser.hs` 		defines a parser that can parse the defined language.
* `Main.hs` 			loads the test programs, parses them, runs them, and prints the resulting configuration.
* `Imp_Syntax.hs` 	defines the syntax of programs, which is the end result of parsing and used to execute the program.
* `Imp_Bigstep.hs` 	defines big-step semantics of the language, allowing programs to be run.

Each file contains comments about what is going on.

## How to Run the Tests

In order to run the programs:
* Install Haskell. The code has been tested and works on Haskell Platform 8.6.3.
* Run `Main.hs`. This opens up a GHCi prompt.
* Type `:main` in the prompt and press enter.

In short, execute the main function in the Main module.

## Syntax Difference

A difference between the imp defined in class and the imp defined here is that all statements must be separated with a ; and there should not be a ; after the last statement in a list of statements.
This is a limitation caused by the way the parser parses programs, but the parsed programs should be equivalent to the imp defined in class. More info can be found in the Parser.hs in part 0.

## Thoughts on the Implementation:

* The parser doesn't come for free like in Maude (granted you need to write the precedence rules which we did not need to do in class), so I had to create one. Luckily there was code in the standard library that helped, but it took some time to figure out how it worked.
* Code in Maude represents the same ideas in a much more compact manner. My definitions are much larger than the equivalent ones in Maude, although I believe this is in part due to me being new to Haskell.
* The implementation is really similar to that of Maude.
    * Sorts correspond to data types and case statements replace rules and if statements. The Haskell definition can be understood if one understands the Maude definition, without knowing Haskell, more or less.
* Nondeterminism (i.e. finding all possible outcomes) doesn't come for free like in Maude, so I had to modify the semantics to handle it. This made it way more complicated than it needed to be and one has to be mindful of the number of potential states or the code would never finish running.

Overall, I don't think there are many benefits of the Haskell implementation over the Maude implementation. Rewrite engines are much better suited to the task of language semantics because they can very compactly describe rules.
I think small-step semantics would probably be the same amount of work for the same benefit as big-step in Haskell, but denotational semantics would be very tricky to do. It's already so verbose in Maude - it would become even more verbose in Haskell.
However, one benefit I did like was the Haskell debugger. I could set breakpoints wherever I wanted and run until I hit them, which was a better debugging experience than Maude.