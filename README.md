# Rusty Monkey

A development of the Monkey Programming Language from the book 
[Writing a Intrepreter in Go](https://interpreterbook.com/).

## Notes
- I wrote this code over the course of a week, to get a feel for implementing a 
 parser and evaluator in Rust, for another project.
- The code is probably far from perfect, and I use liberal use of clone where you
 really rather shouldn't. For example creating an inner environment clones the 
outer environment, when creating a function closure. This is obviously no-bueno.
- I did not copy out all the test code into Rust.

# Prior Art & Inspiration
- https://github.com/wadackel/rs-monkey-lang
- https://github.com/Rydgel/monkey-rust