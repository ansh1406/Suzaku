# Suzaku

Suzaku is a compiler for a high-level language **Asplund** which compiles the source code to binary for my self made 16-Bit CPU **Lancelot-m1** , featuring a complete toolchain from parsing to binary generation. The project is organized into modular components for lexing, parsing, semantic analysis, intermediate representation (IR) generation, and binary output.

## Components

- **Lexer**: Tokenizes source code ([`lexer.hpp`](includes/lexer.hpp), [`lexer.cpp`](src/lexer.cpp))
- **Parser**: Builds an abstract syntax tree (AST) from tokens ([`parser.hpp`](includes/parser.hpp), [`parser.cpp`](src/parser.cpp))
- **Semantic Checker**: Performs semantic analysis ([`semanticChecker.hpp`](includes/semanticChecker.hpp), [`semanticChecker.cpp`](src/semanticChecker.cpp))
- **IR Generator**: Converts AST to intermediate representation ([`IRGenerator.hpp`](includes/IRGenerator.hpp), [`IRGenerator.cpp`](src/IRGenerator.cpp))
- **Binary Generator**: Translates IR to binary code ([`binaryGenerator.hpp`](includes/binaryGenerator.hpp), [`binaryGenerator.cpp`](src/binaryGenerator.cpp))
- **Error Handler**: Handles compilation errors ([`errorHandler.hpp`](includes/errorHandler.hpp), [`errorHandler.cpp`](src/errorHandler.cpp))
- **Grammar**: Language grammar definition ([`grammar.hpp`](includes/grammar.hpp), [`grammar.ebnf`](documentation/grammar.ebnf))

## Building

This project uses CMake and MinGW for building on Windows.

```sh
mkdir build
cd build
cmake ..
mingw32-make 
```

## Usage

Run the compiler with:

```sh
./suzaku.exe <input-file> <output-file> [-i]
```

- `<input-file>`: Path to the source file (e.g., `examples/calculator.aspl`)
- `<output-file>`: Name for the output binary (the extension `.lm1` will be added automatically)
- `-i` (optional): Keep the intermediate representation (`.lasm` file) after compilation

**Example:**

```sh
./suzaku.exe examples/calculator.aspl calculator
```

This will produce `calculator.lm1` (binary) and, unless `-i` is specified, remove the intermediate `calculator.lasm`.

## Documentation

- `documentation/grammar.ebnf`: Language grammar in EBNF
- `documentation/algorithms.md`: Algorithms used to translate high-level language constructs into corresponding Lancelot-M1 machine instructions.
- `documentation/IntermediateCodeSnippets.md`: A detailed breakdown of how each high-level language operation is translated into corresponding Lancelot-M1 instructions. It serves as the official reference for the IR generation phase of the Suzaku compiler.

## Examples

Sample programs are available in the `examples/` directory:

- `*.aspl`: Source code examples
- `*.lm1`: Corresponding compiled/intermediate files
