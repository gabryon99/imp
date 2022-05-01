## IMP - a simple imperative language

> **IMP** is a small language of while programs. IMP is called an "imperative" language because program execution involeves carrying out a series of explicit commands to change state.
> 
> *The formal semantics of programming languages* - *Glynn Winskel*

This OCaml project implements a simple interpreter of the IMP programming language described by Winskell. The language is extended with some tweaks which are not present in the book, such as the `print` and `read` functions. The interpreter makes use of `ocamllex`, `menhir` and `ppx` as dependencies. To build the executable run `dune run` and to execute type `dune exec imp`.

Be aware that this implementation is a toy, it surely has some not checked errors 🙂

### Grammar

The grammar is expressed using the BNF.

```
Arithmetic Expressions
a ::= n | X | a0 + a1 | a0 - a1 | a0 * a1 | read | ( a )

Boolean Expressions
b ::= true | false | a0 = a1 | a0 <= a1 | a0 < a1 | a0 >= a1 | a0 > a1 | !b | b0 || b1 | b0 && b1 | ( b )

Commands
c ::= skip | print a | c0; c1 | if b then c0 else c1 end | while b do c end
```

### A simple program

The program shown below implement the factorial function.

```
n := read;
f := 1;
while n > 0 do
    f := f * n;
    n := n - 1;
end
print f;
```

All the examples can be found inside the `playground` directory.

### Added Features

The features added to the language are a few:
* the `end` keyword has been introduced to mark the end of while/if block;
* the `read` expression is useful to read a number from the *standard input* (⚠️ No check is performed onto the typed input); 
* the `print` command is the most complex one: it prints a number onto the screen.