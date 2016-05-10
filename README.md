# golfscript-rs

A simple Golfscript interpreter in Rust. Currently there are a few missing
features, but these should be added soon.

A sample interpreter can be used by running `cargo run`, but otherwise it is
largely library-driven.

Maybe this will have use for when one wishes to embed some Golfscript in Rust.

# Challenges

Since Rust lacks the dynamic nature of Ruby (the original interpreter language)
we require a lot more explicitness in terms of the underlying interpreter
structure. Further, we don't use regexes for parsing, but instead follow a more
traditional lex -> (no parse) -> execute phase.

# Future Additions

- [ ] Block Statements
- [ ] Final builtin functions
- [ ] Assignment functionality
- [ ] Full test suite
- [ ] Transactional execution (interpreter)
