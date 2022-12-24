# Dromedar Programming Language

Dromedar is a statically and strongly typed _safe_ programming language.

I have already included strings and arrays (without linking it to the garbage collector as of yet), and I plan on adding first-class function objects, list comprehensions, etc.

The garbage collector is already implemented, but it is not linked to most reference operations as of yet. It mixes the mark/sweep and reference counting approaches.

See the Doc folder for documentation on the language.

The following is an example program that works on my machine:

    fn main -> void
        let x := [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
        for i := 20 |.. 0
            print_int(x[i])
        print_str("Hello, World!")
        return
    
It prints the number from `20` to `1` in reverse order, followed by the string `"Hello, World!"`.

Note that because I am currently working on the language, programs that work on day X may not work with an updated compiler or change their behaviour.