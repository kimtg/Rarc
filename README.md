# Rarc: High-Performance Arc Lisp in Racket

Rarc is a high-performance, compact implementation of the **Arc programming language** written in [Racket](https://racket-lang.org/).

By targeting Racket's Chez Scheme JIT compiler, utilizing Racket's native readtable macro system, and compiling recursive definitions with self-referential `letrec` bindings and fixnum fast-paths, Rarc delivers **near-native Racket execution speeds**—outperforming **Python 3.14 by up to 18x** on recursive workloads while maintaining a clean, compact codebase under 1,400 lines of code.

---

## Performance Benchmark: Recursive `fib(34)`

Workload: Calculating the 34th Fibonacci number recursively (5,702,887 function calls):

```arc
(def fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
```

Run via `racket benchmark.rkt`:

| Language / Engine | `fib(34)` Result | Execution Time (ms) | vs Native Racket | vs Python 3.14 |
| :--- | :--- | :--- | :--- | :--- |
| **Native Racket (Chez Scheme JIT)** | **5,702,887** | **~25 ms** | **1.00x** (Baseline) | **23x faster** |
| **Rarc (Arc in Racket JIT)** | **5,702,887** | **~34 ms** | **1.36x** (Near-Native) | **17x faster** |
| **Python 3.14.4 (CPython)** | **5,702,887** | **~590 ms** | **23x slower** | **1.00x** (Baseline) |

---

## Features

- **JIT Compilation to Chez Scheme**: Translates Arc AST expressions directly into native Racket forms evaluated inside an isolated namespace, leveraging Racket's optimizing JIT engine.
- **Self-Referential `letrec` Inlining**: Function definitions (`def`, `assign`) compile to local `letrec` bindings, bypassing dynamic dispatch and emitting direct machine-code jumps on recursive paths.
- **Fixnum Arithmetic Acceleration**: Inlined arithmetic and comparisons (`-`, `<`, `>`) leverage native `racket/fixnum` primitives with automatic fallback to generic numbers, strings, and Arc list operations.
- **Native Readtable & Syntax Sugars**:
  - Bracket shorthand: `[+ _ 10]` $\to$ `(fn (_) (+ _ 10))` implemented via Racket's `make-readtable`.
  - Dot syntax: `car.xs` $\to$ `(car xs)`.
  - Bang syntax: `tbl!key` $\to$ `(tbl 'key)`.
  - Compose syntax: `+:sqrt` $\to$ `(compose + sqrt)`.
  - Complement syntax: `~even` $\to$ `(complement even)`.
  - Quasiquoting: `` `(a ,b ,@c) `` with unquote and unquote-splicing.
- **Zero-Overhead Compile-Time Macros**: Arc macros (`mac`) expand at compile time, eliminating runtime interpreter overhead for standard library constructs (`let`, `with`, `while`, `for`, `each`, etc.).
- **Implicit Indexing**: Direct application of tables, strings, and lists: `(tbl key)`, `(str idx)`, `(lst idx)`.
- **First-Class Continuations**: Supported via `ccc` (`call/cc`).
- **Complete Standard Library**: Automatically bootstraps Arc's core `library.arc` library.

---

## Getting Started

### Prerequisites

- [Racket](https://racket-lang.org/) (v8.0 or higher recommended)
- Python 3.x (optional, only needed for running the cross-language benchmark)

### Running the REPL

Launch the interactive Arc REPL:

```bash
racket main.rkt
```

```
Arc Lisp in Racket (Rarc)
Use (quit) or Ctrl+D / Ctrl+Z to exit.

arc> (+ 1 2 3)
6
arc> (map [+ _ 10] '(1 2 3))
(11 12 13)
arc> (let tbl (table) (= tbl!name "Arc") tbl!name)
"Arc"
```

### Running Scripts

Execute Arc source files directly:

```bash
racket main.rkt test_fib.arc
racket main.rkt euler.arc
```


Run an inline expression with `-e`:

```bash
racket main.rkt -e "(+ 1 2 3)"
```

### Building a Standalone Executable

Compile Rarc into a self-contained native executable using `raco exe`:

```bash
raco exe -o arc.exe main.rkt
```

Run the compiled executable:

```bash
./arc.exe test_fib.arc
```

### Command-Line Options

```
Usage: arc [OPTIONS...] [FILES...]

OPTIONS:
    -e <expr>    Evaluate given expression and print result
    -n, --no-lib Do not load library.arc on startup
    -h, --help   Show help message
    -v, --version Show version
```

---

## Testing & Benchmarking

Run the comprehensive unit test suite (54 test cases covering special forms, reader sugars, primitives, indexing, and standard library):

```bash
racket test.rkt
```

Run the 3-way performance benchmark comparing Rarc, Native Racket, and Python 3.14:

```bash
racket benchmark.rkt
```

---

## Codebase Architecture

The entire implementation consists of ~1,340 lines of modular Racket code:

| File | Purpose |
| :--- | :--- |
| [`types.rkt`](types.rkt) | Core Arc types: `acons` (cons cells), `arc-table`, tagged types, equality (`is`, `iso`), coercions. |
| [`reader.rkt`](reader.rkt) | Macro readtable for `[...]`, bracket syntax, and syntax sugar expansion (`.`, `!`, `:`, `~`). |
| [`builtins.rkt`](builtins.rkt) | Standard library built-ins aliasing Racket standard library functions with `case-lambda`. |
| [`eval.rkt`](eval.rkt) | Arc-to-Racket JIT compiler, macro expansion engine (`macex`), and namespace management. |
| [`arc.rkt`](arc.rkt) | High-level evaluation facade and environment initializer (`arc-init`). |
| [`main.rkt`](main.rkt) | CLI argument parser and interactive Read-Eval-Print-Loop (REPL). |
| [`library.arc`](library.arc) | Arc standard library definitions. |
| [`euler.arc`](euler.arc) | Project Euler problems 1 - 10 solutions in Arc with automated timing and verification. |


---

## Reference

### Special Forms
`assign` `do` `fn` `if` `mac` `quote`

### Built-in Primitives
`*` `+` `-` `/` `<` `>` `apply` `bound` `car` `ccc` `cdr` `close` `coerce` `cons` `cos` `dir` `dir-exists` `disp` `ensure-dir` `err` `eval` `expt` `file-exists` `flushout` `infile` `int` `is` `len` `load` `log` `macex` `maptable` `mod` `msec` `mvfile` `newstring` `outfile` `pipe-from` `quit` `rand` `read` `readb` `readline` `rmfile` `scar` `scdr` `sin` `sqrt` `sread` `stderr` `stdin` `stdout` `string` `sym` `system` `t` `table` `tan` `trunc` `type` `write` `writeb`

### Standard Library (`library.arc`)
`++` `--` `<=` `=` `>=` `aand` `abs` `accum` `acons` `adjoin` `afn` `aif` `alist` `all` `alref` `and` `andf` `assoc` `atend` `atom` `avg` `before` `best` `bestn` `caar` `cadr` `carif` `caris` `case` `caselet` `catch` `cddr` `check` `commonest` `compare` `complement` `compose` `consif` `conswhen` `copy` `copylist` `count` `counts` `cut` `dedup` `def` `defmemo` `do1` `dotted` `drain` `each` `empty` `even` `fill-table` `find` `firstn` `flat` `for` `forlen` `get` `idfn` `iflet` `in` `insert-sorted` `insort` `insortnew` `intersperse` `isa` `isnt` `iso` `join` `keep` `keys` `last` `len<` `len>` `let` `list` `listtab` `loop` `map` `map1` `mappend` `max` `med` `median` `mem` `memo` `memtable` `merge` `mergesort` `min` `mismatch` `most` `multiple` `n-of` `nearest` `no` `noisy-each` `nor` `nthcdr` `number` `obj` `odd` `on` `only` `ontable` `or` `orf` `pair` `point` `pop` `pos` `positive` `pr` `prn` `pull` `push` `pushnew` `quasiquote` `rand-choice` `rand-elt` `range` `readfile` `readfile1` `reclist` `recstring` `reduce` `reinsert-sorted` `rem` `repeat` `retrieve` `rev` `rfn` `rotate` `round` `roundup` `rreduce` `set` `single` `some` `sort` `split` `sref` `sum` `summing` `swap` `tablist` `testify` `tuples` `trues` `union` `uniq` `unless` `until` `vals` `w/table` `w/uniq` `when` `whenlet` `while` `whiler` `whilet` `wipe` `with` `withs` `writefile` `zap`

---

## See Also

* [Arc Language Website](http://arclanguage.org/)
* [Arc Tutorial](http://www.arclanguage.org/tut.txt), [Arc Tutorial (HTML)](https://arclanguage.github.io/tut-stable.html)
* [Arc Documentation & Reference](http://arclanguage.github.io/ref/index.html)

---

## License ##


   Copyright 2016-2026 Kim, Taegyoon

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

   [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
