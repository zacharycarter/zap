# Zap Programming Language Compiler

![build status](https://github.com/zacharycarter/zap/actions/workflows/haskell.yml/badge.svg)

## Overview
The Zap programming language is a general-purpose systems programming language.

### Why?
The vast majority of the compiler code is authored by LLMs. I've simply been guiding them and asking them to analyze their own, as well as my, decision making. The project is really an experiment to see if I can leverage LLMs to create a compiler for a useful programming language.

## Installation

### Prerequisites
- Haskell GHC for building the compiler.
- C compiler (e.g., GCC or Clang) for executing generated code.

### Steps
1. Clone the repository:
   ```bash
   git clone <repository_url>
   cd zap-compiler
   ```
2. Build the project:
   ```bash
   stack build
   ```
3. Run the tests:
   ```bash
   stack test
   ```

## Usage

### Compiling a Program
To compile a Zap source file:
```bash
stack run zap-compiler -- input.zap output.c
```
The output will be a C file named `output.c`.

### Running the Generated Code
Use a C compiler to compile and run the generated file:
```bash
gcc output.c -o program
./program
```

## Tests

### Running Tests
To execute the test suite:
```bash
stack test
```

## Contributing
Contributions are welcome! Please follow these steps:
1. Fork the repository.
2. Create a new branch for your feature or bug fix.
3. Commit your changes and open a pull request.

## License
This project is licensed under the MIT License. See the `LICENSE` file for details.
