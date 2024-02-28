# TFSI

## Build and Running

To build the project, run

```
stack run tfsi
```

If the project is built correctly the program should print

```
TFSI: Build Success. Please run me in GHCi!
```

To actually use the interpreter, run

```
stack repl
```

For the recognized instructions, please refer to the next section.

To test the program, run

```
stack test
```

and it will go through the two test cases the project currently has.

## Instructions

In the core semantics, there are 4 integer operations, namely `int, add, neg, mul` and 4 general operations, namely `z, s, l, a`. The meanings of the integer operations are obvious. For general operations,

- `z`: zero-indexed variables in the local environment
- `s`: succ-indexed variables in the local environment
- `l`: lambda abstractions
- `a`: lambda applications

In the extended semantics, there are operations on boolean values and their meanings are also obvious. `ExtFix` just extends the operation of taking fix points of a function.

For more concrete examples, please refer to the test cases.

The core semantics also provides 4 ways to interprete the program:

- `eval` just evaluates the program to its actual value
- `view` pretty prints the program
- `toHsk` converts the current program into a standard Haskell program
- `Len` measures the length of the program, i.e. counting the number of nodes in the AST