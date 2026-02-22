# Ada Compiler → MIPS Assembly

A compiler for a subset of Ada, written in Haskell, that targets MIPS32 assembly. Built with Alex (lexer) and Happy (parser), it performs semantic analysis, IR generation, liveness-based dead code elimination, register allocation, and final MIPS code generation.

---

## Features

- **Case-insensitive lexing** — keywords are matched regardless of casing
- **Static type checking** — integers, floats, booleans, and strings, with a full symbol table
- **Scoped declarations** — nested `declare ... begin ... end` blocks with proper scope management
- **Control flow** — `if/then/else` and `while/loop` constructs
- **String operations** — concatenation (`&`), conversion from other types (`str(...)`), and I/O via `put_line` / `get_line`
- **Arithmetic** — `+`, `-`, `*`, `/`, `**` (power) for integers and floats
- **Boolean logic** — `and`, `or`, `xor`, `not`, with short-circuit evaluation
- **Liveness analysis** — dataflow-based dead code elimination on the IR
- **Register allocation** — variables and temporaries are assigned to registers or spilled to the stack per scope
- **MIPS runtime** — includes built-in routines for integer/float/boolean-to-string conversion, string concatenation, integer parsing, and overflow detection

---

## Supported Ada Subset

```ada
procedure main is
    x, y : integer := 0;
    s    : string  := "hello";
begin
    x := 10 ** 2;
    y := x + 5;
    s := str(y) & " world";
    put_line(s);

    while x > 0 loop
        x := x - 1;
    end loop;

    if x = 0 then
        put_line("done");
    else
        put_line("not done");
    end if;
end main;
```

**Supported types:** `integer`, `float`, `boolean`, `string`  
**Supported I/O:** `put_line(expr)`, `get_line(str_var, int_var)`  
**Supported conversions:** `str(expr)` — converts any type to string

---

## Project Structure

| File | Description |
|------|-------------|
| `Lexer.x` | Alex lexer specification |
| `Parser.y` | Happy parser, produces an AST |
| `SymbolTable.hs` | Symbol table construction and type checking |
| `PrintAST.hs` | Pretty-printer for the AST (debugging) |
| `IR.hs` | IR data types and AST → IR translation |
| `AnalyzeIR.hs` | IR pass: collects variables, literals, and scope info |
| `LivenessAnalysis.hs` | Liveness analysis and dead code elimination on the IR |
| `MemoryAllocator.hs` | Register and stack allocation per scope |
| `CodeGen.hs` | IR → MIPS assembly translation |
| `Main.hs` | Compiler driver |
| `Makefile` | Build system |

---

## Requirements

- [GHC](https://www.haskell.org/ghc/) (≥ 9.x recommended)
- [Alex](https://haskell-alex.readthedocs.io/) — lexer generator
- [Happy](https://www.haskell.org/happy/) — parser generator

Install via [GHCup](https://www.haskell.org/ghcup/):

```bash
ghcup install ghc
cabal install alex happy
```

---

## Building

```bash
make
```

This will:
1. Run `alex` on `Lexer.x` to generate `Lexer.hs`
2. Run `happy` on `Parser.y` to generate `Parser.hs`
3. Compile all modules with GHC into `bin/ada`

To clean build artifacts:

```bash
make clean
```

---

## Usage

```bash
./bin/ada <source_file.ada>
```

This produces several output files in the same directory as the source:

| Output | Description |
|--------|-------------|
| `<name>.mips` | Final MIPS assembly |
| `<name>AST.debugging` | Parsed AST |
| `<name>Table.debugging` | Symbol table and scope info |
| `<name>IR.debugging` | Raw intermediate representation |
| `<name>IROptimized.debugging` | IR after dead code elimination |
| `<name>Allocation.debugging` | Register allocation map |

---

## Compilation Pipeline

```
Source (.ada)
    │
    ▼
Lexer (Alex)          — tokenization, case-insensitive
    │
    ▼
Parser (Happy)        — produces AST
    │
    ▼
Symbol Table          — type checking, scope resolution
    │
    ▼
IR Generation         — AST → three-address IR
    │
    ▼
Liveness Analysis     — compute gen/kill/in/out sets
    │
    ▼
Dead Code Elimination — remove unused assignments
    │
    ▼
IR Analysis           — collect variable/literal info per scope
    │
    ▼
Memory Allocation     — assign registers or stack slots
    │
    ▼
Code Generation       — emit MIPS32 assembly
```

---

## MIPS Runtime

The generated assembly includes the following built-in subroutines:

| Routine | Description |
|---------|-------------|
| `itos` | Integer → string |
| `ftos` | Float → string |
| `btos` | Boolean → string (`TRUE`/`FALSE`) |
| `stoi` | String → integer (with overflow detection) |
| `concat` | Dynamic string concatenation |
| `read` | Read a line from stdin |
| `put_line` | Print a newline |
| `pow_int` | Integer exponentiation (with overflow detection) |
| `pow_float` | Float exponentiation (with overflow detection) |
| `static_str_compare_eq` | Static string equality |
| `dynamic_str_compare_eq` | Dynamic string equality |

---

## Known Limitations

- No support for functions or procedures beyond `main`
- No pointer or array types
- Integer and float types are 32-bit only
- Liveness analysis operates on a single flat block (no inter-procedural analysis)
- Available expression analysis is present in the codebase but currently commented out

---
