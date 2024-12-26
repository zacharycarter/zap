# Zap Programming Language Compiler

## Overview
The Zap programming language is a general-purpose systems language optimized for game development. This repository contains the source code for the Zap compiler, capable of parsing, analyzing, and generating code for the Zap programming language. The compiler generates C code as an intermediate representation, with plans to extend to other targets in the future.

## Features
### Language Features
- **Automatic Memory Management**: Zap uses mutable value semantics and reference counting for efficient memory management.
- **Allocator Control**: Provides fine-grained control over memory allocation via:
  - Allocator parameters in type definitions.
  - Scoped allocator context system.
  - Direct allocator specification for individual allocations.
- **Syntax**: Utilizes significant whitespace and allows parentheses omission for streamlined function calls, e.g., `print "Hello, World!"`.
- **Data Types**:
  - Numeric types (Int32, Int64, Float32, Float64).
  - Vectors (Vec2, Vec3, Vec4).
  - Strings, Booleans, Structs, and Arrays.
- **Control Structures**: Includes if-else, blocks, and loops with scoped variable control.

### Compiler Features
- **Compilation Stages**:
  1. Lexical Analysis.
  2. Parsing and AST generation.
  3. Semantic Analysis for type checking and validation.
  4. Intermediate Representation (IR) generation.
  5. C code generation.
- **Optimization**: Supports customizable optimization levels.
- **Error Handling**: Clear reporting for lexical, syntax, semantic, and generation errors.

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

### Test Directory Structure
Tests for the Zap language and compiler are located in the `tests/Zap` folder. These include:
- **Syntax Tests**: Validate correct parsing of language constructs.
- **Semantic Tests**: Verify type checking and semantic rules.
- **Code Generation Tests**: Ensure proper translation to C code.

### Running Tests
To execute the test suite:
```bash
stack test
```

## Example Programs

### Hello World
```zap
print "Hello, World!"
```

``` shell
Hello, World!
```

### Binary Operations
```zap
print 1 + 1
print 2 * 2
print 4 / 2
```

``` shell
2
4
2
```

### Vector Operations
```zap
let vec1 = Vec3(1.0, 2.0, 3.0)
let vec2 = Vec3 4.0, 5.0, 6.0
print (vec1 + vec2)
```

``` shell
(5.000000, 7.000000, 9.000000)
```

### Struct Definitions
```zap
type
  Point = struct
    x: f32
    y: f32

let origin = Point 5.0, 2.5
print origin.x
```

``` shell
5.000000
```

### While Loops

``` zap
var n = 0
while n < 3:
  print n
  n += 1
```

``` shell
0
1
2
```

### Functions With Implicit Result

``` zap
fn add(x, y: i32): i32 =
  x + y

print add(1, 2)
```

``` shell
3
```

### Functions With Local Variables

``` zap
fn sumSquares(x, y: i32): i32 =
  var sum = x * x
  sum = sum + y * y
  sum

print sumSquares(2, 2)
``` 

``` shell
4
```

## Contributing
Contributions are welcome! Please follow these steps:
1. Fork the repository.
2. Create a new branch for your feature or bug fix.
3. Commit your changes and open a pull request.

## License
This project is licensed under the MIT License. See the `LICENSE` file for details.
