# rlox

An implementation of the Lox language from [Crafting Interpreters][book] in
Rust, up to the end of Chapter 29.

[book]: https://craftinginterpreters.com

This is a _very_ literal conversion into Rust -- there's a _lot_ of avoidable
`unsafe`, and a lot of very unidiomatic code.

I have some _grand designs_ I may never get to for making this more idiomatic,
in another repo where the language might diverge from lox a fair bit.

Chapter 30 is on optimizations which didn't seem worth implementing if I'm going
to tear this up anyway.

## Building and running

I wrote this with Rust 1.55 on stable, and everything's handled by `cargo`.

I don't recommend using this for much of anything. While the book's tests pass,
I would be shocked if there weren't a lot of bugs still hiding in there.

## Changes from the book

There were a few places where I made changes to try and avoid things
Rust hates when it was clear enough that the deviations would be worthwhile and
likely wouldn't have unforeseen consequences for later chapters.

- We don't have a global `VM` instance.
- Allocation (and freeing) of objects is a bit different.
- We have a `Context` instead of a bunch of global state for the compiler.
- I used a `Vec` instead of an internal linked list in a few places
  - For the `Compiler` stack, instead of `enclosing`.
  - In the `VM` to track open upvalues.
  - GC is paused when compiling. This is so we don't need to worry about the
    partially-compiled objects being collected, without needing to feed the
    compiler state back into the heap on every allocation to mark those roots.

Output should be identical, so you can use the test suite from Crafting
Interpreters.

There are a cargo features you can build with to print some extra info at
runtime: `debug_trace`, `debug_log_gc`. There's a `debug_stress_gc` which does a
full collection on every allocation (when not paused) as well.
