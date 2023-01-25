# Dromedar Programming Language

Dromedar is a statically and strongly typed _safe_ programming language.

As of right now, the language includes primitive types, strings and arrays, fully connected to the garbage collector.
Because I am currently working on the language, it is likely that programs that work on day X stop working or change their behaviour due to a compiler update.

The [Documentation](Doc/Doc.pdf) contains extensive documentation, including a formal grammar and typing specification. The [Examples](Droml/ExamplePrograms/) folder contains some example programs with functionality that is already implemented.

## Using The Compiler

In order to compile the compiler, you will need `clang`, `make` and `ocaml` installed.

You can then compile the compiler and all the resources it requires with `make all`.

Then, you can compile Dromedar source files using `./droml <FILENAME>`. You can specify the **output file** with `-o <FILENAME>`. `-l` prints the lexer output, `-p` the parser output and `-ll` prints the generated LLVM IR code. `-s` generates Assembly output, `-c` generates LLVM output and writes it to the output file (instead of a fully compiled and linked executable).

Thus, e.g. `./droml ExamplePrograms/Primes.drm -l -o Primes.s -s` writes the `clang`-generated Assembly code to `Primes.s`, while printing the output of the lexer to the console.

## Expanding The Standard Library

It is possible to write extensions to the Dromedar standard library - both in Dromedar and in C (the language in which most of the standard library is implemented) - or even in C++ (which is a slightly more involved process).

### Expanding it in Dromedar

In order to expand the standard library with functions written in pure Dromedar, you will either need to create a new `.drm` file in which to create your functions, or append them to the [existing standard libary file](Droml/Drmstdlib.drm). If you create a new file, you need to add it to the [library list](Droml/LibLists/drmlibs.txt).

You can then use all the functions you created in your own Dromedar programs.

### Expanding it using C

Writing native functions requires the following steps:

1. Write the function signatures in a `.drm` file (as if you were to expand the standard library in Dromedar - see the section above). For this, you need to create `native` functions, types and values. Native types are used by the Dromedar runtime system to hide functionality of other languages behind them (e.g. a `std::regex` object as used by the `Regex` module of the standard library). They are treated like blackbox types by Dromedar programs.
2. You can then take a look at the LLVM output of any Dromedar program. Note that attempting to compile it to an executable now will most likely lead to a linker error since it cannot find the functions referred to by your signatures. The LLVM output will contain function declarations for your newly created functions. These are quite similar to how you will have to implement them in C. `#include "cutils/common.h"` will give you access to some typedefs that make your code look similar to the LLVM declarations.
3. You now have to compile your new file(s) - either compile them by hand by following the `stdlib` rule in the [makefile](Droml/Makefile) or by adding them to the task list in said `stdlib` rule.

You must make sure that the compiled `.o` file is located in the [obj](Droml/obj/) folder. Then you can use your new C functions in Dromedar using your native function signatures.

Note that if you return reference objects (including blackbox native types), you need to allocate them using `_allocate(int64_t)` - the function defined in the [garbage collection header file](Droml/cutils/gc.h).

### Expanding it using C++

Sometimes, writing functions in C++ is a considerable reduction in complexity compared to C. If you want to write a C++ extension to the standard library, you will still have to complete the steps for a Dromedar extension and the first two steps for a C extension.

Then, you can create a C++ header and source file (let's call them `l.h` and `l.cpp`) in which you will implement the core functionality of your new functions. You have to call the C++ functions from the C file which contains your functions that match the LLVM signatures.

In order to make the C++ functions interoperable with the C native functions, you need to make your files look as follows:

**l.h**

    #ifndef __L_H__
    #define __L_H__

    #ifdef __cplusplus
        extern "C" {
    #endif

            /*
             * your C++ function headers
             */

    #ifdef __cplusplus
        }
    #endif

    #endif

**l.cpp**

    #include "l.h"

    /*
     * static C++ functions
     */
    
    extern "C" {

        /*
         * implementations of your C++ headers from l.h
         */
    }

A good example of how your C++ and C files must look is the Regex module of the standard library: [Regex.c](Droml/cutils/drmstdlib/Regex.c), [Regex.h](Droml/cutils/drmstdlib/cpputils/Regex.h), and [Regex.cpp](Droml/cutils/drmstdlib/cpputils/Regex.cpp).

You can then either compile your C++ file to a statically linkable library `.so` file by following the execution steps of the `stdlib` rule in the [Makefile](Droml/Makefile), or add it to the taks list in the rule, following the outline given by the rule in the makefile.

Following that, make sure your `.so` file is located in the [obj](Droml/obj/) folder. Then, you must the path to your library in the [C++ Library List](Droml/LibLists/cpplibs.txt), following the example of the libraries which are already there.

After that, you can freely use your new functions in Dromedar programs.

## Quick Introduction

Everything described in this section is described in more detail in the [documentation](Doc/Doc.pdf). This text simply serves as a quick introduction into the language and its features.

### Hello, World!

    fn main -> void
        IO.print_str("Hello, World!\n")

### Program Buildup

Each Dromedar program consists of a series of global function and variable declarations. Function bodies contain statements (assignments, variable declarations, if-statements, etc.).

The language uses significant whitespace. Two neighboring instructions in the same block require the same indentation string, whereas a deeper instruction requires a longer indentation string. Two blocks with the same indentation level do not necessarily need the same indentation string, but they do need to match their respective environments.

#### Function Declarations

Function declarations are created using the `fn` keyword, followed by a name, an argument list and a return type. For example, the function header `fn f : x:int, y:int -> int` declares an `int`-function, taking two `int` arguments.

Functions are accessible in the entire program, meaning that mutual recursion is possible without forward declarations.

### Typing System

Dromedar knows four primitive types: `bool`, `char`, `int` and `flt`.

The language uses a safe typing system. This means that `null` pointer errors are impossible: dereferencing a reference object is only possible if it is of a strictly non-`null` type. Reference types come in non-`null`, and maybe-`null` types, which are denoted by a `?` suffix.

#### Lists

There are three ways to write down list literals:

* As an actual list: `[1,2,3,4,5]` is the list containing the numbers from 1 to 5.
* As a bounded list: `[1...5]` is the same list. The delimiter `..|` doesn't include the last element, `|..` doesn't include the first, and `|.|` doesn't include either. Thus, `[1...5]` and `[0|.|6]` are equivalent. Bounded lists can only return `int` lists as of now, but I plan on extending them to `char` lists as well.
* As a comprehension list: `[exp : decls : condition]`, e.g. as follows: `[2*x : x in [0...10] : x > 5]`, which is equivalent to `[12,14,16,18,20]`.

### Statements

#### Declarations

The following are simple local declarations:

    let a := 3
    let b := 2.0
    let c:int := 7
    let d:flt := 6
    let e:string? := "hi!"
    let f:[[int]] := [[1,2],[3],[4,5,6]]

The type specification is optional, but it allows for some bending of the rules: Declaring a variable as `flt` and assigning it an `int` value (and vice versa), is possible - as well as assigning a variable of type `t?` a value of type `t`, as in the string declaration.

Variables are immutable, unless declared with the `mut` keyword, in which they may change the value assigned to them.

#### Control Flow

There are five control flow constructs implemented as of today: `if-elif-else`, `while`, `do-while`, `for` and `denull`. Here are semantically equivalent examples for `do-while` and `for`:

    mut i := 0
    do
        printf("{0}\n", i)
        i := i + 1
    while i < 10

    for i := 0 ..| 10
        printf("{0}\n", i)

Both these programs print the numbers from 0 to 9.

The `denull` statement checks whether an expression passed to it is `null`. If not, its result may be used as if it were of non-`null` type, as follows:

    denull x := getstring()
        IO.print_str(x)

A `denull` statement may be followed by an `else` block which gets executed if the expression is `null`.

