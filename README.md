# russx

Russx implements a template rendering engine based on [rstml](https://github.com/rs-tml/rstml).
It generates Rust code from your templates at compile time using a macro.
This crate is inpired by both [Askama](https://github.com/djc/askama) and [Leptos](https://github.com/leptos-rs/leptos).

## Supported in templates

- Template instantiation
- For loops, if-else statements, if-let statements, match statements
- Full rust variables
- Opt-out HTML escaping

## How to get started

First, add the russx dependancy to your crate's `Cargo.toml`:

```sh
cargo add russx
```

In any Rust file inside your crate, add the following:

```rust
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
