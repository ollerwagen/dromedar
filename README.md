# Dromedar Programming Language

Dromedar is a statically and strongly typed _safe_ programming language.

I have already included strings and arrays (without linking them to the garbage collector as of yet), and I plan on adding first-class function objects, list comprehensions, etc.

The garbage collector is already implemented, but it is not linked to most reference operations as of yet. It mixes the mark/sweep and reference counting approaches.

See the Doc folder for more extensive documentation on the language.

The folder Droml/ExamplePrograms contains some (surprise!) example programs.
    
It prints the number from `20` to `1` in reverse order, followed by the string `"Hello, World!"`.

Note that because I am currently working on the language, programs that work on day X may not work with an updated compiler or change their behaviour.

## Using The Compiler

In order to compile the compiler, you will need `clang`, `make` and `ocaml` installed. You can then compile it with `make all`.

Then, you may compile `.drm` files with `./droml <filename>`. This generates an `Out.ll` file that contains the generated LLVM IR code, an `Out.s` file containing the Assembly code generated by the clang compiler using the `Out.ll` file, and finally the file `a.out`, an executable for the given input program.