//! # Russx
//! Russx implements a template rendering engine based on [rstml](https://github.com/rs-tml/rstml).
//! It generates Rust code from your templates at compile time using a macro.
//! This crate is inpired by both [Askama](https://github.com/djc/askama) and [Leptos](https://github.com/leptos-rs/leptos).
//!
//! To create templates use the [`templates`](macro.templates.html) macro,
//! or the [`tmpl`](macro.tmpl.html) for dynamic templates.

use std::{error::Error as StdError, fmt, io, ops};

pub use russx_macros::{templates, tmpl};

pub type Result<T, E = Error> = core::result::Result<T, E>;

/// Russx error type.
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum Error {
    /// Formatting error
    #[error("formatting error: {0}")]
    Fmt(#[from] fmt::Error),
    /// HTML attribute name error.
    /// Fired when an attribute name doesn't conform to the [HTML standard](https://html.spec.whatwg.org/#attributes-2).
    #[error("attribute doesn't conform to html standard: {0:?}")]
    AttributeError(String),
    /// Any other error that might be raised inside a template.
    /// use `result.map_err(Error::custom)?`
    #[error("custom error: {0}")]
    Custom(Box<dyn StdError + Send + Sync>),
}

impl Error {
    /// A utility function to create `Error::Custom`.
    pub fn custom(err: impl StdError + Send + Sync + 'static) -> Self {
        Self::Custom(err.into())
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Self::custom(err)
    }
}

#[doc(hidden)]
pub mod __typed_builder {
    pub use typed_builder::*;
}

#[doc(hidden)]
/// Writes html-escaped `value` into `writer`.
pub fn __write_escaped(
    writer: &mut (impl fmt::Write + ?Sized),
    value: &(impl fmt::Display + ?Sized),
) -> Result<()> {
    use fmt::Write;

    pub struct EscapeWriter<'a, W: fmt::Write + ?Sized>(&'a mut W);

    impl<W: fmt::Write + ?Sized> fmt::Write for EscapeWriter<'_, W> {
        #[inline]
        fn write_str(&mut self, s: &str) -> fmt::Result {
            use askama_escape::Escaper;

            askama_escape::Html.write_escaped(&mut *self.0, s)
        }
    }

    write!(EscapeWriter(writer), "{value}")?;

    Ok(())
}

/// Write an attribute and check its validity.
fn write_attribute(
    writer: &mut (impl fmt::Write + ?Sized),
    value: &(impl fmt::Display + ?Sized),
) -> Result<()> {
    use fmt::Write;

    pub struct AttributeWriter<'a, W: fmt::Write + ?Sized> {
        has_written: bool,
        writer: &'a mut W,
    }

    impl<W: fmt::Write + ?Sized> fmt::Write for AttributeWriter<'_, W> {
        fn write_str(&mut self, s: &str) -> fmt::Result {
            #[rustfmt::skip]
            fn is_invalid_attribute_char(ch: char) -> bool {
                matches!(
                    ch,
                    '\0'..='\x1F' | '\x7F'..='\u{9F}'
                    | ' ' | '"' | '\'' | '>' | '/' | '='
                    | '\u{FDD0}'..='\u{FDEF}'
                    | '\u{0FFFE}' | '\u{0FFFF}' | '\u{01FFFE}' | '\u{01FFFF}' | '\u{2FFFE}'
                    | '\u{2FFFF}' | '\u{3FFFE}' | '\u{03FFFF}' | '\u{04FFFE}' | '\u{4FFFF}'
                    | '\u{5FFFE}' | '\u{5FFFF}' | '\u{06FFFE}' | '\u{06FFFF}' | '\u{7FFFE}'
                    | '\u{7FFFF}' | '\u{8FFFE}' | '\u{08FFFF}' | '\u{09FFFE}' | '\u{9FFFF}'
                    | '\u{AFFFE}' | '\u{AFFFF}' | '\u{0BFFFE}' | '\u{0BFFFF}' | '\u{CFFFE}'
                    | '\u{CFFFF}' | '\u{DFFFE}' | '\u{0DFFFF}' | '\u{0EFFFE}' | '\u{EFFFF}'
                    | '\u{FFFFE}' | '\u{FFFFF}' | '\u{10FFFE}' | '\u{10FFFF}'
                )
            }

            self.has_written |= !s.is_empty();
            if s.contains(is_invalid_attribute_char) {
                return Err(fmt::Error);
            }
            self.writer.write_str(s)
        }
    }

    let mut attr_writer = AttributeWriter {
        has_written: false,
        writer,
    };

    write!(attr_writer, "{value}")?;

    if !attr_writer.has_written {
        return Err(Error::AttributeError(value.to_string()));
    }

    Ok(())
}

mod sealed {
    pub trait SealedAttribute {}
    pub trait SealedAttributes {}
}

/// The attribute trait, this will write a single attribute.
pub trait Attribute: sealed::SealedAttribute {
    /// Renders the attribute to the given fmt writer, the attribute will have a space prefixed.
    fn render_into(&self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()>;
}

impl<T: Attribute + ?Sized> sealed::SealedAttribute for &'_ T {}
impl<T: Attribute + ?Sized> Attribute for &'_ T {
    #[inline]
    fn render_into(&self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        T::render_into(self, writer)
    }
}

impl<T: Attribute + ?Sized> sealed::SealedAttribute for &'_ mut T {}
impl<T: Attribute + ?Sized> Attribute for &'_ mut T {
    #[inline]
    fn render_into(&self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        T::render_into(self, writer)
    }
}

impl sealed::SealedAttribute for String {}
impl Attribute for String {
    /// Writes a valueless attribute
    fn render_into(&self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        writer.write_char(' ')?;
        write_attribute(writer, self)?;

        Ok(())
    }
}

impl sealed::SealedAttribute for str {}
impl Attribute for str {
    /// Writes a valueless attribute
    fn render_into(&self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        writer.write_char(' ')?;
        write_attribute(writer, self)?;

        Ok(())
    }
}

impl<N: fmt::Display, T: fmt::Display> sealed::SealedAttribute for (N, T) {}
impl<N: fmt::Display, T: fmt::Display> Attribute for (N, T) {
    fn render_into(&self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        writer.write_char(' ')?;
        write_attribute(writer, &self.0)?;
        writer.write_str("=\"")?;
        __write_escaped(writer, &self.1)?;
        writer.write_char('"')?;

        Ok(())
    }
}

/// The attributes trait, this can write a variable amount of attributes.
/// You can use this within a template using a braced attribute, `{...}`.
///
/// ```rust
/// let style_attr = ("style", "border: 1px solid black");
/// let html = russx::tmpl! {
///     <div {style_attr}>
///         "hello world"
///     </div>
/// }.render()?;
/// ```
pub trait Attributes: sealed::SealedAttributes {
    /// Renders the attributes to the given fmt writer, the attributes will be separated by spaces
    /// and be prefixed with a space.
    fn render_into(self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()>;
}

impl<T: Attribute> sealed::SealedAttributes for T {}
impl<T: Attribute> Attributes for T {
    fn render_into(self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        Attribute::render_into(&self, writer)
    }
}

impl sealed::SealedAttributes for () {}
impl Attributes for () {
    /// Does nothing
    #[inline]
    fn render_into(self, _writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        Ok(())
    }
}

impl<I: Attribute, T: IntoIterator<Item = I>> sealed::SealedAttributes for ops::RangeTo<T> {}
impl<I: Attribute, T: IntoIterator<Item = I>> Attributes for ops::RangeTo<T> {
    fn render_into(self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        for attr in self.end {
            attr.render_into(writer)?;
        }

        Ok(())
    }
}

/// Main template trait.
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

    /// Renders the template to the given fmt writer.
    fn render_into(self, writer: &mut dyn fmt::Write) -> Result<()>;

    /// Helper method which allocates a new String and renders into it.
    fn render(self) -> Result<String> {
        let mut buf = String::new();
        let _ = buf.try_reserve(self.size_hint());
        self.render_into(&mut buf)?;
        Ok(buf)
    }

    /// Renders the template to the given IO writer.
    #[inline]
    fn write_into(self, writer: &mut (impl io::Write + ?Sized)) -> io::Result<()> {
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
            Err(err) => {
                // check if the error came from the underlying `Write` or not
                if output.error.is_err() {
                    output.error
                } else {
                    match err {
                        Error::Fmt(fmt::Error) => Err(writer
                            .write_fmt(format_args!("{DisplayError}"))
                            .unwrap_err()),
                        Error::AttributeError(_) => {
                            Err(io::Error::new(io::ErrorKind::InvalidData, err))
                        }
                        err => Err(io::Error::new(io::ErrorKind::Other, err)),
                    }
                }
            }
        }
    }
}

type DynRenderInto<'a> = dyn FnOnce(&mut dyn fmt::Write) -> Result<()> + Send + 'a;

/// A dynamic template generated from a function.
/// This is the type of `<prop _ />` and `children` when instantiating a static template.
pub struct TemplateFn<'a> {
    size_hint: usize,
    render_into: Box<DynRenderInto<'a>>,
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
        render_into: impl FnOnce(&mut dyn fmt::Write) -> Result<()> + Send + 'a,
    ) -> Self {
        Self {
            size_hint,
            render_into: Box::new(render_into),
        }
    }

    /// Converts a template into `TemplateFn`, this cannot be done through the `Into` trait since
    /// `TemplateFn` also implements `Template`.
    pub fn from_template<T: Template + Send + 'a>(template: T) -> Self {
        Self::new(template.size_hint(), |writer| template.render_into(writer))
    }
}

impl Template for TemplateFn<'_> {
    const SIZE_HINT: usize = 20;

    fn size_hint(&self) -> usize {
        self.size_hint
    }

    fn render_into(self, writer: &mut dyn fmt::Write) -> Result<()> {
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

#[allow(dead_code)]
const HTML_MIME_TYPE: &str = "text/html";

#[doc(hidden)]
#[cfg(feature = "axum")]
pub mod __axum {
    pub use axum_core::response::{IntoResponse, Response};
    use http::{header, HeaderValue, StatusCode};

    use super::*;

    pub fn into_response<T: Template>(t: T) -> Response {
        match t.render() {
            Ok(body) => IntoResponse::into_response((
                [(
                    header::CONTENT_TYPE,
                    HeaderValue::from_static(HTML_MIME_TYPE),
                )],
                body,
            )),
            Err(_) => StatusCode::INTERNAL_SERVER_ERROR.into_response(),
        }
    }

    impl<'a> IntoResponse for TemplateFn<'a> {
        fn into_response(self) -> Response {
            into_response(self)
        }
    }
}

#[doc(hidden)]
#[cfg(feature = "actix-web")]
pub mod __actix_web {
    pub use actix_web::{body::BoxBody, HttpRequest, HttpResponse, Responder};
    use actix_web::{
        http::{header::HeaderValue, StatusCode},
        HttpResponseBuilder, ResponseError,
    };

    use super::*;

    impl ResponseError for Error {}

    pub fn respond_to<T: Template>(t: T) -> HttpResponse {
        match t.render() {
            Ok(body) => HttpResponseBuilder::new(StatusCode::OK)
                .content_type(HeaderValue::from_static(HTML_MIME_TYPE))
                .body(body),
            Err(err) => HttpResponse::from_error(err),
        }
    }

    impl<'a> Responder for TemplateFn<'a> {
        type Body = BoxBody;

        fn respond_to(self, _req: &HttpRequest) -> HttpResponse {
            respond_to(self)
        }
    }
}

#[doc(hidden)]
#[cfg(feature = "hyper")]
pub mod __hyper {
    use hyper::{
        header::{self, HeaderValue},
        StatusCode,
    };

    use super::*;

    pub type Body = String;
    pub type Response<B = Body> = hyper::Response<B>;

    fn try_respond<T: Template>(t: T) -> Result<Response> {
        Ok(Response::builder()
            .status(StatusCode::OK)
            .header(
                header::CONTENT_TYPE,
                HeaderValue::from_static(HTML_MIME_TYPE),
            )
            .body(t.render()?.into())
            .unwrap())
    }

    pub fn respond<T: Template>(t: T) -> Response {
        try_respond(t).unwrap_or_else(|_| {
            Response::builder()
                .status(StatusCode::INTERNAL_SERVER_ERROR)
                .body(Default::default())
                .unwrap()
        })
    }

    impl<'a> From<TemplateFn<'a>> for Response {
        fn from(slf: TemplateFn<'a>) -> Self {
            respond(slf)
        }
    }

    // impl<'a> TryFrom<TemplateFn<'a>> for Body {
    //     type Error = Error;
    //     fn try_from(slf: TemplateFn<'a>) -> Result<Self> {
    //         slf.render().map(Into::into)
    //     }
    // }
}

#[doc(hidden)]
#[cfg(feature = "warp")]
pub mod __warp {
    pub use warp::reply::{Reply, Response};
    use warp::{
        http::{self, header, StatusCode},
        hyper::Body,
    };

    use super::*;

    pub fn reply<T: Template>(t: T) -> Response {
        match t.render() {
            Ok(body) => http::Response::builder()
                .status(StatusCode::OK)
                .header(header::CONTENT_TYPE, HTML_MIME_TYPE)
                .body(body.into()),
            Err(_) => http::Response::builder()
                .status(StatusCode::INTERNAL_SERVER_ERROR)
                .body(Body::empty()),
        }
        .unwrap()
    }

    impl<'a> Reply for TemplateFn<'a> {
        fn into_response(self) -> Response {
            reply(self)
        }
    }
}

#[doc(hidden)]
#[cfg(feature = "tide")]
pub mod __tide {
    pub use tide::{Body, Response};

    use super::*;

    pub fn try_into_body<T: Template>(t: T) -> Result<Body> {
        let mut body = Body::from_string(t.render()?);
        body.set_mime(HTML_MIME_TYPE);
        Ok(body)
    }

    pub fn into_response<T: Template>(t: T) -> Response {
        match try_into_body(t) {
            Ok(body) => {
                let mut response = Response::new(200);
                response.set_body(body);
                response
            }

            Err(error) => {
                let mut response = Response::new(500);
                response.set_error(error);
                response
            }
        }
    }

    impl<'a> TryFrom<TemplateFn<'a>> for Body {
        type Error = Error;

        fn try_from(slf: TemplateFn<'a>) -> Result<Self, Self::Error> {
            try_into_body(slf)
        }
    }

    impl<'a> From<TemplateFn<'a>> for Response {
        fn from(slf: TemplateFn<'a>) -> Self {
            into_response(slf)
        }
    }
}

#[doc(hidden)]
#[cfg(feature = "gotham")]
pub mod __gotham {
    use gotham::hyper::{
        self,
        header::{self, HeaderValue},
        StatusCode,
    };
    pub use gotham::{handler::IntoResponse, state::State};

    use super::*;

    pub type Response<B = hyper::Body> = hyper::Response<B>;

    pub fn respond<T: Template>(t: T) -> Response {
        match t.render() {
            Ok(body) => Response::builder()
                .status(StatusCode::OK)
                .header(
                    header::CONTENT_TYPE,
                    HeaderValue::from_static(HTML_MIME_TYPE),
                )
                .body(body.into())
                .unwrap(),
            Err(_) => Response::builder()
                .status(StatusCode::INTERNAL_SERVER_ERROR)
                .body(vec![].into())
                .unwrap(),
        }
    }

    impl<'a> IntoResponse for TemplateFn<'a> {
        fn into_response(self, _state: &State) -> Response {
            respond(self)
        }
    }
}

#[doc(hidden)]
#[cfg(feature = "rocket")]
pub mod __rocket {
    use std::io::Cursor;

    use rocket::{
        http::{Header, Status},
        response::Response,
    };
    pub use rocket::{
        response::{Responder, Result},
        Request,
    };

    use super::*;

    pub fn respond<T: Template>(t: T) -> Result<'static> {
        let rsp = t.render().map_err(|_| Status::InternalServerError)?;
        Response::build()
            .header(Header::new("content-type", HTML_MIME_TYPE))
            .sized_body(rsp.len(), Cursor::new(rsp))
            .ok()
    }

    impl<'a, 'r, 'o: 'r> Responder<'r, 'o> for TemplateFn<'a> {
        fn respond_to(self, _req: &'r Request<'_>) -> Result<'o> {
            respond(self)
        }
    }
}
