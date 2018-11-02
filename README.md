ZINC - ZINC Is Not Correct
===

This repository constitutes the IMP compiler written for the course D7011E ---
*Compiler Construction* --- taken at Luleå Technical University.
The GitHub repo for all why3 code (under `why3/`) is [available here](https://github.com/Tmplt/zinc-why3).

Aside from the mandatory assignments, the following extra changes has been done:
1. Syntax has been overhauled to be more C/Rust-like.
  * Use curly braces instead of `BEGIN`, `END`, etc.
  * Make so that all statements must end with semi-colon.
  * Make so that no semi-colon is required after loops, if-else, etc.
2. An extra rule has been added for casting (`Tsint -> Tuint32` considered illegal)
3. Another implementation of `imp_ex`, `imp_exn` that counts the number of evaluation steps.
  * Possible to give a limit to ZINC via `-n <max-steps>`.
  * Proof in why3.
4. Proof that AST optimization is not worse than unoptimized AST.
  * Count evaluation steps for `aexpr`, `bexpr` and `com`, except for `Cwhile`s which has an abstract cost instead.
5. Write a small test bench for the compiler harness 
  * Tests containing `// MUSTFAIL` must, as the comment implies, fail to compile.

---

1. See `imp_programs/*.imp`
2. Executing the following code
```
a : SINT;
b : UINT32;
b := 5;
a := ((UINT32) b) + a;
```
leads to
```
Type error: UINT32 in: Line 4: Position 16
  a := ((UINT32) b) + a;
                       ^
is an illegal cast
```
3. See <https://github.com/Tmplt/zinc-why3/blob/github/imp_ex_assignment.mlw#L59-L156>
Example execution:
```
$ ./Zinc.native -imp_exn imp_programs/if9.imp
Execute : imp_exn
ceval_exn in 5 steps
State:
Id #1 "a" = 20
```
Or with a limit:
```
$ ./Zinc.native -imp_exn imp_programs/if9.imp -n 3
Execute : imp_exn
Interpreter error: maximum steps reached!
State:
Id #1 "a" = 20
```
4. See <https://github.com/Tmplt/zinc-why3/blob/github/imp_ex_assignment.mlw#L159-L248>
5. All tests (`imp_programs/*.imp`) can be run via `make test`.


Extra
---

The file `shell.nix` contains the Nix expression used for the development environment used.
