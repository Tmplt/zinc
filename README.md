# Cimp, compiler harness for D7011E, Compiler Construction

This (new) version includes span information in the 'T_Imp.ml' AST, along with a simle type checker.

## Todo:

The examples are not updated, you need to add type information, e.g.

```
(* tc_1.imp passes type checking *)
a : SINT;   (* types must be declared *)
b : UINT32;
a := a + 1;
b := b +u 1
```

and
```
(* tc_2.imp fails type checking *)
a : SINT;   (* types must be declared *)
b : UINT32;
a := a + 1;
b := a +u 1

Type error: Expected UINT32 got SINT in:<105..106> a
in command: <100..111> b := a +u 1
```

## Requirements:

Dependencies:

- OCaml (tested with 4.06 and 4.07)
- menhir (tested with menhir, version 20180703, install through opam)
- code (tested with Visual Studio Code version 1.26)

Optional dependencies:

- ocamlmerlin (install merlin through opam)

The `.merlin` configuration file, gives information to merlin on how your project is setup. This allows for cross referencing, completion, and appropriate compilation error message processing. Reflect any changes to your build structure in the `.merlin` file.

```
S extract
S examples
B _build
B _build/extract
B _build/examples
PKG zarith
```

- vscode-reasonml (tested with 1.0.38)

The `tasks.json` configuration file allows you to setup build targets, and automate the building process. We have prepared a few build targets for Your convenience, e.g.:

```
        ...
        {
            "label": "Cimp",
            "type": "shell",
            "command": "ocamlbuild -r -use-menhir -I extract -pkg zarith Cimp.native",
            "problemMatcher": [
                "$ocamlc"
            ],
            "group": {
                "kind": "build",
                "isDefault": true
            }
        }
        ...
```

## The Cimp compiler harness:

### Building

Use the build target from within (Visual Studio Code) `code`, or the terminal command:
```
> ocamlbuild -r -use-menhir -I extract -pkg zarith Cimp.native
```

The harness should build without errors.

If interested you may look into `jbuilder/dune` as an alternative build system (seems the way to go for larger project developments, but for the purpose of the course the `ocamlbuild` system suffices).

### Testing

```
> ./Cimp.native --help
The cimp v1.0 compiler, Per Lindgren (c) 2018
Options summary:
  -i 		: infile (-i optional)
  -o 		: outfile (default infile.s)
  -v 		: verbose mode (default disable)
  -D 		: debug mode (default disable)
  -d_ast 	: dump AST
  -d_past 	: dump pretty AST
  -d_code 	: dump code
  -d_pcode 	: dump pretty code
  -imp_ex 	: imp_ex evaluation
  -vm_ex 	: vm_ex virtual machine execution
  -help  Display this list of options
  --help  Display this list of options
```

Or you may try with an example file:

```
> ./Cimp.native imp_programs/t0_BV32_Test.imp -d_ast
outfile :imp_programs/t0_BV32_Test.s
cimp options:
infile       : imp_programs/t0_BV32_Test.imp
outfile      : imp_programs/t0_BV32_Test.s
verbose      : false
debug        : false
d_ast        : true
d_past       : false
d_code       : false
d_pcode      : false
imp_ex       : false
vm_ex        : false

Raw AST :
Cassign 1 Aaddu (Anum 2147483647) (Anum 1)

Done!
```

Later in the course, you will extend the harness with further options.

## File structure

```
Files:
├── Cimp.ml                 Compiler harness for "imp"
├── Cimp.native ->          Link to executable
├── Cmd.ml                  Command line parser
├── CommentingExample.ml    (* Cmmenting guidelines *)
├── Common.ml               A few helper definitions
├── Dump.ml                 Printing of AST and other data structures
├── Env.ml                  Storage for the compilation environment
├── Error.ml                Error processing (from lexer and parser)
├── Lexer.mll               Lexer rules
├── Options.ml              Command line options (used by Cmd)
├── Parser.mly              Parser rules
├── T_Check.ml              Type checker
├── T_Dump.ml               Printing of T_AST (the input language AST)
├── T_Imp.ml                The input language AST
├── README.md               This file
├── _build                  Build directory for OCaml
├── examples                Some examples
│   ├── BV32_Test.ml        Testing of wrapping arithemtics
│   ├── Hello.ml            Hello World
│   └── Imp_AST_test.ml     Building an AST from OCaml
├── extract                 Extracted files from the Why3 development
|   imp_Imp.ml              The input program AST
|   ...
└── imp_programs            A set of "imp" program examples
|   ...
```

OCaml typically compiles into the `_build` directory (and its sub-folders).

## Extracted files

The `extract` folder contains type definitions and functions extracted from a Why3 development (giving the specification and implementation of a "certified"/proven compiler for the "imp" language). The `imp_Imp.ml`is of particular interest as defining the Abstract Syntax Tree (`AST`) for the "imp" language. We can either construct programs in "imp" by directly using the AST constructors (as seen in the examples `Imp_AST_test` and `BV32_Test` below), or by using the compiler harness to parse a text (program) with concrete syntax for "imp" (which we will discuss at the end).

Besides the "imp" AST, the extracted files provides:

- An execution engine "imp_ex" for "imp"

- A specification of a stack machine "vm"

- A compiler (with proof of correctness) from "imp" to "vm"

- An execution engine "vm_ex" for "vm"

For now, the extracted files are given to you, later You will generate them yourself on basis of Your specifications and implementations developed in Why3.

## Examples

Let us start by studying a few examples:

### Example Imp_AST_test:

> ocamlbuild -r -I extract -pkg zarith examples/Imp_AST_test.native --

Builds and executes the `Imp_AST_test` example, that constructs a program in "imp" directly in terms of an AST.

In `Env.ml` we define a bi-directional key (integer) value (string) store for our variable identifiers. A failing lookup of a variable identifier (string) creates a new allocation and increments the key (integer). (The implementation of `Env` uses list, more efficient data structures can be thought of, e.g., hash maps, but efficiency of the compiler is not a concern for now). For this example:

- Key integer 1, has the associated value string "v1"

- Key integer 2, has the associated value string "v2"

It first constructs and prints the AST for an arithmetic expression

```
a : Asub (Aadd (Anum 1) (Anum 2)) (Aadd (Avar Id #1) (Avar Id #2))
```

... and from there constructs a simple program, consisting of the following commands:

```
commands:
v1 := 1;
v2 := 2;
v2 := ((1 + 2) - (v1 + v2))
```

(by reusing the definition of the arithmetic expression)

It then creates and prints the initial memory state:

```
st_0
State:
Id #1 "v1" = 0
Id #2 "v2" = 0
```

(the memory state handling is part of the extracted code)

It then evaluates the input program, and prints the new (final) memory state.

```
ceval_ex
State:
Id #1 "v1" = 1
Id #2 "v2" = 0
```

(the evaluation is done by "imp_ex")

"v1" remains its assigned value 1, while "v2" is re-assigned to ((1 + 2) - (v1 + v2)) = 0

It then compiles the input program to an output program for the virtual (stack machine) language.

```
instructions:
                - code for v1 := 1;
Iconst 1;       - pushes 1 on the evaluation stack
Isetvar v1;     - pops the evaluation stack (1) and stores the value in variable v1

                - code for v2 := 2;
Iconst 2;       - pushes 2 on the evaluation stack
Isetvar v2;     - pops the evaluation stack (2) and stores the value in variable v2

                - v2 := ((1 + 2) - (v1 + v2))
Iconst 1;       - pushes 1 on the evaluation stack
Iconst 2;       - pushes 2 on the evaluation stack
Iadd;           - pops two values n1 (2) n2 (1) from the evaluation stack, computes n1 (2) + n2 (1), and pushes the result 3
Ivar v1;        - pushes the content of variable v1 (1) on the evalution stack
Ivar v2;        - pushes the content of variable v2 (2) on the evalution stack
Iadd;           - pops two values n1 (2) n2 (1) from the evaluation stack, computes n1 (2) + n2 (1), and pushes the result 3
Isub;           - pops two values n1 (3) n2 (3) from the evaluation stack, computes n1 (3) - n2 (3), and pushes the result 0
Isetvar v2;     - pops the evaluation stack (0) and stores the value in variable v2
Ihalt;          - terminates execution
```

(the actual compilation is done by the extracted code)

And finally, it executes the generated stack machine code using "vm_ex" (from the initial state st_0):

```
execution halted
instr_iter_ex
State:
Id #1 "v1" = 1
Id #2 "v2" = 0
```

(Here we see that the final memory states are consistent, essentially that is what the proof of correctness actually is all about.)

----

Warmup Exercise:
Copy the code (`Imp_AST_test.ml`) into `Imp_AST_ex1.ml`.
Make the necessary changes to represent, compile and execute the following program:

```
v1 := 1;
v2 := 2;
v3 := (v2 + 1);
v2 := ((1 + 2) - (v1 + v2 + v3))
```

You may find constructing programs from the AST to be boring and error prone. That is exactly why we have compiler frontends, giving a textual concrete syntax to programs;)

To confirm your result, run.

> ./Cimp.native imp_programs/t0_AST_ex1.imp  -d_ast -d_past -d_code -d_pcode -imp_ex -vm_ex


---


### Example BV32_Test:
```
> ocamlbuild -r -I extract -pkg zarith examples/BV32_Test.native --
```

Compiles and executes the example.

```
BV32_test
a : -2147483648
```

This is the result of:

```
     0x7fff_ffff
    +0x0000_0001
----------------
    -0x8000_0000
(printed as a base 10 integer)
```

The point here is to observe the wrapping behavior of `BV32.bv_add`. So why are we interested in such details? Well, we are studying a compiler, and in the end of the course we will generate machine executable assembly. At that point we need to select instructions correctly and discriminate between signed arithmetic operations (which may cause overflow exceptions), and unsigned operations (which may wrap).

The counterpart as a program in "imp" can be parsed and executed.
> ./Cimp.native imp_programs/t0_BV32_Test.imp  -d_ast -d_past -d_code -d_pcode -imp_ex -vm_ex

Looking at the "imp" code:

```
(* example corresponding to BV32_Test *)
v := 2147483647 +u 1
```

Here we see that the specific arithmetic operation (in this case unsigned addition), was given special syntax `+u`. A realistic language, would instead chose the specific operation on basis of the operand type(s). Later in the course, You will design a type system (and corresponding type checker). The `Cimp` harness given, parses directly to the (untyped) "imp" AST. Later you will either make the "imp" AST typed, or introduce intermediate representation(s) holding type information.

---

Warmup exercise:

Some languages require you to declare all variables (and their types) before use (e.g., C). Some require you to give an initial value at declaration (e.g., Rust). Languages (like OCaml and Rust) may in cases figure out the types depending on context (how they are used), this process is called type inference. Some languages allows for overloading operators, others have typeclasses (Haskell), Traits (Rust) or Interfaces (Java, C++). All of them have different pros- and cons-. What is your favourite? (Later, You have to decide on something sufficiently simple, in order to come up with a specification, rules for well-formedness and a corresponding implementation (type checker), with optional proofs of correctness)

---

