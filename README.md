# calc

Example Common Lisp project -- RPN calculator. Serves as a reference for
[cl-toolkit](https://github.com/turtle-bazon/cl-toolkit) development and as a
template for building executable CL applications with ASDF, Quicklisp, Make,
and GitHub Actions.

## Features

- Arithmetic: `+ - * / ^ mod`
- Trig / math: `sin cos tan asin acos atan log log10 exp sqrt abs round floor ceil`
- Comparisons: `> < = >= <= !=`
- Stack ops: `dup swap drop over rot nip`
- Bitwise / logic: `logand logior logxor and or not bitnot shl shr`
- Base conversion: `hex bin dec`
- Factorial: `!`
- Constants: `PI E TRUE FALSE NIL`
- Variables: `X = 42`
- User functions: `defun square(x) x x *`
- Ternary: `condition true-val false-val ?`
- Semicolon-separated expressions: `1 2 +; 3 4 +`

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
quit
```

## Project layout

```
calc.asd              ASDF system definition
calc-tests.asd        Test system
build.lisp            Quicklisp + asdf:make driver
Makefile              build / test / clean targets
src/
  package.lisp        Package definition
  tokenizer.lisp      Expression tokenizer
  resolver.lisp       Token resolver (numbers, constants, variables)
  evaluator.lisp      RPN evaluator + stack/comparison/logic ops
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
- **build.lisp** that quickloads the system and calls `asdf:make`
- **Makefile** wrapping SBCL invocations
- **GitHub Actions** building on Linux (SBCL bullseye container), macOS, and
  Windows, with artifact upload and tag-triggered releases

To use as a template, search-and-replace `calc` with your project name and
update the source files.

## License

GPL-3.0. See [LICENSE](LICENSE).
