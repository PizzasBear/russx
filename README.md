# russx

[![Documentation](https://docs.rs/russx/badge.svg)](https://docs.rs/russx/)
[![Latest version](https://img.shields.io/crates/v/russx.svg)](https://crates.io/crates/russx)

Russx implements a template rendering engine based on [rstml](https://github.com/rs-tml/rstml).
It generates Rust code from your templates at compile time using a macro.
This crate is inpired by both [Askama](https://github.com/djc/askama) and [Leptos](https://github.com/leptos-rs/leptos).

## Features

- Template instantiation
- For loops, if-else statements, if-let statements, match statements
- Full rust variables
- Opt-out HTML escaping
- Optional built-in support for Actix-Web, Axum, Gotham, Rocket, Tide, and warp web frameworks.

## How to get started

First, add the russx dependancy to your crate's `Cargo.toml`:

```sh
cargo add russx
```

In any Rust file inside your crate, add the following:

```rust
use russx::Template;

russx::templates! {
    pub fn hello<'a>(name: &'a str) {
        <p>Hello, {name}!</p>
    }
}

fn main() {
    let html = hello("world").render().unwrap();
    println!("{html}");
}
```

You should be able to compile and run this code.
