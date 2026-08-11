# calc

Example Common Lisp project -- RPN calculator. Serves as a reference for
[cl-toolkit](https://github.com/turtle-bazon/cl-toolkit) development and as a
template for building executable CL applications with ASDF, Quicklisp, Make,
and GitHub Actions.

## Features

- Arithmetic: `+ - * / ^ mod min max gcd lcm pow`
- Trig / math: `sin cos tan asin acos atan atan2 log log10 exp sqrt abs round floor ceil hypot`
- Comparisons: `> < = >= <= !=`
- Stack ops: `dup swap drop over rot nip clear depth pick tuck`
- Bitwise / logic: `logand logior logxor logeqv and or xor nand nor not bitnot shl shr`
- Min/Max variants: `min3 max3 clamp` (3-argument versions)
- Base conversion: `hex bin dec`
- Factorial: `!`
- Memory: `M+ M- MR MC` (memory register operations)
- Arrays: `[ 1 2 3 ]` create, `get set len push pop append` operations
- Strings: `"hello"` literals, `strlen strcat substr upper lower trim` operations
- Macros: `defmacro double(x) x x +` (inline expansion)
- Lambdas: `( x ) x x +` define anonymous functions, `5 call` invoke
- Functional ops: `MAP FILTER REDUCE` (apply functions to arrays)
- Constants: `PI E TRUE FALSE NIL`
- Variables: `X = 42`
- User functions: `defun square(x) x x *`
- Ternary: `condition true-val false-val ?`
- Control flow: `if condition then true-val else false-val then`
- Loops: `begin condition while body repeat` or `begin body condition until`
- FOR loop: `start end FOR I body NEXT` (inline loop with counter I)
- Semicolon-separated expressions: `1 2 +; 3 4 +`
- Script execution: `calc script.calc` (execute .calc files)
- REPL commands: `help`, `variables`, `quit`
- Error handling with descriptive messages

## Build

Requires SBCL with [Quicklisp](https://www.quicklisp.org/) installed.

```sh
make build           # produces build/calc
make test            # FiveAM unit tests
make test-integration  # bash integration tests
make test-all        # both
make clean           # removes build/
```

## Usage

```
calc
Calculator (type 'quit' to exit)
2 3 +
= 5
X = 42
= 42
X
= 42
10 M+
= NIL
MR
= 10
MC
1 2 3 depth
= 3
10 20 0 pick
= 10
1 if 10 else 20 then
= 10
0 if 10 else 20 then
= 20
begin 5 1 - dup while dup 0 > repeat
= 0
( x ) x x + 5 call
= 10
[ 1 2 3 4 ] "+" 0 REDUCE
= 10
quit
```

## Project layout

```
calc.asd              ASDF system definition
calc-tests.asd        Test system
Makefile              build / test / clean targets
src/
  package.lisp        Package definition
  tokenizer.lisp      Expression tokenizer
  resolver.lisp       Token resolver (numbers, constants, variables)
  evaluator.lisp      RPN evaluator + lambdas + functional ops
  processor.lisp      Expression processor (assign, defun, calls)
  main.lisp           Entry point
tests/
  calc-test.lisp      FiveAM unit tests
  integration.sh      Bash integration tests
.github/workflows/
  build.yml           CI: Linux / macOS / Windows + releases on tags
```

## Template for executable CL projects

This repository demonstrates a standard layout for Common Lisp command-line
tools:

- **ASDF system** (`calc.asd`) with `:build-operation "program-op"` and
  `:entry-point`
- **Separate test system** (`calc-tests.asd`) with `:perform test-op`
- **Makefile** wrapping SBCL invocations
- **GitHub Actions** building on Linux (SBCL bullseye container), macOS, and
  Windows, with artifact upload and tag-triggered releases

To use as a template, search-and-replace `calc` with your project name and
update the source files.

## License

GPL-3.0. See [LICENSE](LICENSE).
