# rlox

An implementation of the Lox language from [Crafting Interpreters][book] in
Rust.

This is going to be a pretty literal conversion into Rust -- there's a _lot_ of
`unsafe`. That said, there were a few places where I made changes to try and
avoid things rust hates when it was clear enough that the deviations would be
worthwhile.

- We don't have a global `VM` instance.
- Allocation of objects go through a borrowed `Heap` owned by the `VM`
- We have a `Context` instead of a global scanner.
- I used a `Vec` instead of an internal linked list in a few places
  - For the `Compiler` stack, instead of `enclosing`.
  - In the `VM` to track open upvalues.
  - GC is paused when compiling. This is so we don't need to worry about the
    partially-compiled `ObjFunction`s being collected, without needing to feed
    the compiler state back into the heap on every allocation to mark those
    roots.

Output should be basically identical, so you can use the test suite from
Crafting Interpreters.

There's a `debug_trace` cargo feature you can build with to print some extra
info at runtime.

[book]: https://craftinginterpreters.com