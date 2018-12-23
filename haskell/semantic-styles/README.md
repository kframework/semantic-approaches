# semantic-styles

`semantic-styles` is a library of comprised of many different languages implemented in many different semantic frameworks using Haskell as a meta-language.

So far we have:

#### Languages
- [x] IMP
- [x] IMP++
- [ ] LAMBDA
- [ ] LAMBDA++

#### Semantics

- [x] Small-Step SOS (Transition Semantics)
- [ ] Big-Step SOS (Natural Semantics)
- [ ] Denotational Semantics
- [ ] Modular SOS
- [ ] RSEC - Reduction Semantics with Evaluation Contexts
- [ ] CK Machine
- [ ] SECD Machine
- [ ] SCC Machine
- [ ] CHAM - Chemical Abstract Machine


## Getting Started

This library is built using the Haskell `stack` build tool. Instructions to install `stack` are [here](https://docs.haskellstack.org/en/stable/README/#how-to-install)

Once `stack` is installed use 

```bash
stack install semantic-styles\:exe\:semantic-run 
```

To install the main executable of the library.
Once installed,
```bash
semantic-run -h
```
will give you the details of what flags and options to use to run programs.
Some example runs are included in the next section.

## Examples

To execute the program `collatz.imp` as an IMP program using the Small-Step SOS (Transition) semantic framework:

```bash
semantic-run --imp --transition res/imp/collatz.imp
```

To execute the IMP++ program `incUp.imp` with input from the file `oneUpTo5.in`:

```
semantic-run --imp++ -t res/imp++/incUp.imp --input res/imp/oneUpTo5.in
```

## Language Interpretters

Under Construction
