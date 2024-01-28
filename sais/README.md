# Syntactic Abstractions in Scheme

See `main.rs` for example calls / tests.

You can call `cargo run` too see pre- and post-expanded code.

This expander works in the typical style. Macros are more like
`syntax-rules` in that they define hygenic transformers, and
lambda arguments are treated like sub-transformers that are
expanded in the extent of the macro body.