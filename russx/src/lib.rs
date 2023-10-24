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
pub mod __typed_builder {
    pub use typed_builder::*;
}

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
    render_into: Box<dyn FnOnce(&mut dyn fmt::Write) -> fmt::Result + Send + 'a>,
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
        render_into: impl FnOnce(&mut dyn fmt::Write) -> fmt::Result + Send + 'a,
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

    struct ErrorAdapter(fmt::Error);

    impl fmt::Debug for ErrorAdapter {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt::Debug::fmt(&self.0, f)
        }
    }

    impl fmt::Display for ErrorAdapter {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt::Display::fmt(&self.0, f)
        }
    }

    impl ResponseError for ErrorAdapter {}

    pub fn respond_to<T: Template>(t: T) -> HttpResponse {
        match t.render() {
            Ok(body) => HttpResponseBuilder::new(StatusCode::OK)
                .content_type(HeaderValue::from_static(HTML_MIME_TYPE))
                .body(body),
            Err(err) => HttpResponse::from_error(ErrorAdapter(err)),
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
    pub use hyper::Body;
    use hyper::{
        header::{self, HeaderValue},
        StatusCode,
    };

    use super::*;

    pub type Response<B = Body> = hyper::Response<B>;

    fn try_respond<T: Template>(t: T) -> Result<Response, fmt::Error> {
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
                .body(Body::empty())
                .unwrap()
        })
    }

    impl<'a> Into<Response> for TemplateFn<'a> {
        fn into(self) -> Response {
            respond(self)
        }
    }

    impl<'a> TryInto<Body> for TemplateFn<'a> {
        type Error = fmt::Error;
        fn try_into(self) -> Result<Body, Self::Error> {
            self.render().map(Into::into)
        }
    }
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

    pub fn try_into_body<T: Template>(t: T) -> Result<Body, fmt::Error> {
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

    impl<'a> TryInto<Body> for TemplateFn<'a> {
        type Error = fmt::Error;

        fn try_into(self) -> Result<Body, Self::Error> {
            try_into_body(self)
        }
    }

    impl<'a> Into<Response> for TemplateFn<'a> {
        fn into(self) -> Response {
            into_response(self)
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
