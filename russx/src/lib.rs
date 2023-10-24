//! # Russx
//! Russx implements a template rendering engine based on [rstml](https://github.com/rs-tml/rstml).
//! It generates Rust code from your templates at compile time using a macro.
//! This crate is inpired by both [Askama](https://github.com/djc/askama) and [Leptos](https://github.com/leptos-rs/leptos).
//!
//! To create templates use the [`templates`](macro.templates.html) macro,
//! or the [`tmpl`](macro.tmpl.html) for dynamic templates.

use std::{fmt, io};

pub use russx_macros::{templates, tmpl};
#[doc(hidden)]
pub use typed_builder as __typed_builder;

// mod seal {
//     pub trait InterpolateSeal {}
// }
//
// pub trait Interpolate: seal::InterpolateSeal {
//     fn interpolate(&self, writer: &mut (impl fmt::Write + ?Sized)) -> fmt::Result;
// }
//
// impl seal::InterpolateSeal for () {}
// impl Interpolate for () {
//     fn interpolate(&self, _writer: &mut (impl fmt::Write + ?Sized)) -> fmt::Result {
//         Ok(())
//     }
// }
//
// #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
// pub struct Trust<T>(pub T);
// impl<T: fmt::Display> seal::InterpolateSeal for Trust<T> {}
// impl<T: fmt::Display> Interpolate for Trust<T> {
//     fn interpolate(&self, writer: &mut (impl fmt::Write + ?Sized)) -> fmt::Result {
//         write!(writer, "{}", self.0)
//     }
// }
//
// impl<T: fmt::Display> seal::InterpolateSeal for &'_ T {}
// impl<T: fmt::Display> Interpolate for &'_ T {
//     fn interpolate(&self, writer: &mut (impl fmt::Write + ?Sized)) -> fmt::Result {
//         pub struct EscapeWriter<'a, W: fmt::Write + ?Sized>(&'a mut W);
//
//         impl<W: fmt::Write + ?Sized> fmt::Write for EscapeWriter<'_, W> {
//             #[inline]
//             fn write_str(&mut self, s: &str) -> fmt::Result {
//                 use askama_escape::Escaper;
//
//                 askama_escape::Html.write_escaped(&mut *self.0, s)
//             }
//         }
//
//         use fmt::Write;
//         write!(EscapeWriter(writer), "{self}")
//     }
// }
//
// pub fn interpolate(
//     writer: &mut (impl fmt::Write + ?Sized),
//     value: &impl Interpolate,
// ) -> fmt::Result {
//     value.interpolate(writer)
// }

#[doc(hidden)]
/// Writes html-escaped `value` into `writer`.
pub fn write_escaped(
    writer: &mut (impl fmt::Write + ?Sized),
    value: impl fmt::Display,
) -> fmt::Result {
    use fmt::Write;

    pub struct EscapeWriter<'a, W: fmt::Write + ?Sized>(&'a mut W);

    impl<W: fmt::Write + ?Sized> fmt::Write for EscapeWriter<'_, W> {
        #[inline]
        fn write_str(&mut self, s: &str) -> fmt::Result {
            use askama_escape::Escaper;

            askama_escape::Html.write_escaped(&mut *self.0, s)
        }
    }

    write!(EscapeWriter(writer), "{value}")
}

/// Main Template trait.
/// Implementations can be generated using both the `tmpl` and `templates` macros.
pub trait Template: Sized {
    /// Provides a rough estimate of the expanded length of the rendered template.
    /// Larger values result in higher memory usage but fewer reallocations.
    /// Smaller values result in the opposite. This value only affects render.
    /// It does not take effect when calling `render_into`, `write_into`, the `fmt::Display`
    // implementation, or the blanket `ToString::to_string` implementation.
    const SIZE_HINT: usize;

    /// Provides a conservative estimate of the expanded length of the rendered template.
    /// See `Template::SIZE_HINT` for more information.
    fn size_hint(&self) -> usize {
        Self::SIZE_HINT
    }

    // Required method

    /// Renders the template to the given writer fmt buffer.
    fn render_into(self, writer: &mut dyn fmt::Write) -> fmt::Result;

    // Provided methods

    /// Helper method which allocates a new String and renders into it.
    fn render(self) -> Result<String, fmt::Error> {
        let mut buf = String::new();
        let _ = buf.try_reserve(self.size_hint());
        self.render_into(&mut buf)?;
        Ok(buf)
    }

    #[inline]
    fn write_into(self, writer: &mut dyn io::Write) -> io::Result<()> {
        // Create a shim which translates a Write to a fmt::Write and saves
        // off I/O errors. instead of discarding them
        struct Adapter<'a, T: ?Sized + 'a> {
            inner: &'a mut T,
            error: io::Result<()>,
        }

        impl<T: io::Write + ?Sized> fmt::Write for Adapter<'_, T> {
            fn write_str(&mut self, s: &str) -> fmt::Result {
                match self.inner.write_all(s.as_bytes()) {
                    Ok(()) => Ok(()),
                    Err(e) => {
                        self.error = Err(e);
                        Err(fmt::Error)
                    }
                }
            }
        }

        struct DisplayError;
        impl fmt::Display for DisplayError {
            fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
                Err(fmt::Error)
            }
        }

        let mut output = Adapter {
            inner: writer,
            error: Ok(()),
        };
        match self.render_into(&mut output) {
            Ok(()) => Ok(()),
            Err(fmt::Error) => {
                // check if the error came from the underlying `Write` or not
                if output.error.is_err() {
                    output.error
                } else {
                    Err(writer
                        .write_fmt(format_args!("{DisplayError}"))
                        .unwrap_err())
                }
            }
        }
    }
}

/// A dynamic template generated from a function.
/// This is the type of `<prop _ />` and `children` when instantiating a static template.
pub struct TemplateFn<'a> {
    size_hint: usize,
    render_into: Box<dyn 'a + FnOnce(&mut dyn fmt::Write) -> fmt::Result>,
}

impl<'a> Default for TemplateFn<'a> {
    /// Creates a new `TemplateFn` which does nothing
    #[inline]
    fn default() -> Self {
        Self::new(0, |_| Ok(()))
    }
}

impl<'a> TemplateFn<'a> {
    /// Creates a new `TemplateFn` from a size hint (see `Template::SIZE_HINT`) and a render_into
    /// function (see `Template::render_into`).
    pub fn new(
        size_hint: usize,
        render_into: impl 'a + FnOnce(&mut dyn fmt::Write) -> fmt::Result,
    ) -> Self {
        Self {
            size_hint,
            render_into: Box::new(render_into),
        }
    }

    /// Converts a template into `TemplateFn`, this cannot be done through the `Into` trait since
    /// `TemplateFn` also implements `Template`.
    pub fn from_template<T: Template + 'a>(template: T) -> Self {
        Self::new(template.size_hint(), |writer| template.render_into(writer))
    }
}

impl Template for TemplateFn<'_> {
    const SIZE_HINT: usize = 20;

    fn size_hint(&self) -> usize {
        self.size_hint
    }

    fn render_into(self, writer: &mut dyn fmt::Write) -> fmt::Result {
        (self.render_into)(writer)
    }
}

impl fmt::Debug for TemplateFn<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[derive(Debug)]
        struct RenderInto;

        f.debug_struct(std::any::type_name::<Self>())
            .field("size_hint", &self.size_hint)
            .field("render_into", &RenderInto)
            .finish()
    }
}
