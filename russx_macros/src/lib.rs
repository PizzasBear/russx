use std::{
    collections::HashSet,
    fmt::{Display, Write},
    sync::OnceLock,
};

use askama_escape::Escaper;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use rstml::node::{
    KeyedAttribute, KeyedAttributeValue, Node as RstmlNode, NodeAttribute, NodeBlock, NodeName,
};
use syn::{
    braced, parenthesized,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    spanned::Spanned,
    token, Token,
};

mod nodes;

use nodes::*;

#[rustfmt::skip]
const HTML_ELEMENTS: [&str; 137] = [
    "a", "abbr", "acronym", "address", "area", "article",
    "aside", "audio", "b", "base", "bdi", "bdo", "big",
    "blockquote", "body", "br", "button", "canvas", "caption", "center",
    "cite", "code", "col", "colgroup", "data", "datalist", "dd",
    "del", "details", "dfn", "dialog", "dir", "div", "dl",
    "dt", "em", "embed", "fieldset", "figcaption", "figure", "font",
    "footer", "form", "frame", "frameset", "h1", "head", "header",
    "hgroup", "hr", "html", "i", "iframe", "image", "img",
    "input", "ins", "kbd", "label", "legend", "li", "link",
    "main", "map", "mark", "marquee", "menu", "menuitem", "meta",
    "meter", "nav", "nobr", "noembed", "noframes", "noscript", "object",
    "ol", "optgroup", "option", "output", "p", "param", "picture",
    "plaintext", "portal", "pre", "progress", "q", "rb", "rp",
    "rt", "rtc", "ruby", "s", "samp", "script", "search",
    "section", "select", "slot", "small", "source", "span", "strike",
    "strong", "style", "sub", "summary", "sup", "table", "tbody",
    "td", "template", "textarea", "tfoot", "th", "thead", "time",
    "title", "tr", "track", "tt", "u", "ul", "var",
    "video", "wbr", "xmp", "h2", "h3", "h4", "h5",
    "h6", "svg", "math", "content", "shadow",
];

fn fn_binding_to_pat(
    mut fn_binding: rstml::node::FnBinding,
    allow_types: bool,
) -> syn::Result<(token::Paren, syn::Pat)> {
    if fn_binding.inputs.len() == 1 && !fn_binding.inputs.trailing_punct() {
        let binding = fn_binding.inputs.pop().unwrap().into_value();
        if let (
            false,
            syn::Pat::Type(syn::PatType {
                colon_token, ty, ..
            }),
        ) = (allow_types, &binding)
        {
            Err(syn::Error::new_spanned(
                quote! { #colon_token #ty },
                "specifying the type of a pattern isn't supported here",
            ))
        } else {
            Ok((fn_binding.paren, binding))
        }
    } else if allow_types {
        Err(syn::Error::new_spanned(
            fn_binding,
            "Expected a single pattern with an optional type, e.g. `(pat: ty)` or `(mut pat)`",
        ))
    } else {
        Err(syn::Error::new_spanned(
            fn_binding,
            "Expected a single pattern type, e.g. `(mut pat)`",
        ))
    }
}

fn html_elements_set() -> &'static HashSet<&'static str> {
    static HTML_ELEMENTS_SET: OnceLock<HashSet<&str>> = OnceLock::new();

    HTML_ELEMENTS_SET.get_or_init(|| HTML_ELEMENTS.into_iter().collect())
}

fn path_to_ident(mut path: syn::Path) -> Result<syn::Ident, syn::Path> {
    if path.get_ident().is_some() {
        Ok(path.segments.pop().unwrap().into_value().ident)
    } else {
        Err(path)
    }
}

fn expr_path_to_ident(
    syn::ExprPath { attrs, qself, path }: syn::ExprPath,
) -> Result<syn::Ident, syn::ExprPath> {
    if attrs.is_empty() && qself.is_none() {
        match path_to_ident(path) {
            Ok(ident) => Ok(ident),
            Err(path) => Err(syn::ExprPath { attrs, qself, path }),
        }
    } else {
        Err(syn::ExprPath { attrs, qself, path })
    }
}

fn node_name_to_html_name(name: NodeName) -> syn::Result<HtmlNodeName> {
    match name {
        NodeName::Block(_block) => unreachable!(),
        NodeName::Path(path) => Ok(HtmlNodeName::Ident(expr_path_to_ident(path).map_err(
            |path| {
                syn::Error::new_spanned(
                    path,
                    "Expected either identifier or a punctuated name. \
                     NOTE: you can try to prefix this with `raw:`",
                )
            },
        )?)),
        NodeName::Punctuated(pname) => 'not_raw: {
            use rstml::node::NodeNameFragment;
            use syn::punctuated::Pair;
            'raw: {
                if pname.len() < 2 {
                    break 'raw;
                }

                let Some(Pair::Punctuated(NodeNameFragment::Ident(ident), punct)) =
                    pname.pairs().next()
                else {
                    break 'raw;
                };

                if ident != "raw" || punct.as_char() != ':' {
                    break 'raw;
                }

                let mut pairs = pname.into_pairs();

                let Pair::Punctuated(NodeNameFragment::Ident(raw), colon) = pairs.next().unwrap()
                else {
                    unreachable!()
                };
                break 'not_raw Ok(HtmlNodeName::Raw(RawHtmlNodeName {
                    raw_token: kw::raw(raw.span()),
                    colon_token: {
                        type Colon = Token![:];
                        Colon {
                            spans: [colon.span()],
                        }
                    },
                    punctuated: pairs.collect(),
                }));
            }

            Ok(HtmlNodeName::Punctuated(pname))
        }
    }
}

fn is_html_element(name: &NodeName) -> bool {
    match name {
        NodeName::Path(syn::ExprPath {
            qself: None,
            attrs,
            path,
        }) if attrs.is_empty() => {
            let Some(ident) = path.get_ident() else {
                return false;
            };
            html_elements_set().contains(&*ident.to_string())
        }
        NodeName::Path(_) => false,
        NodeName::Block(_) => false,
        NodeName::Punctuated(_) => true,
    }
}

fn is_name_ident(name: &NodeName, ident: &str) -> bool {
    match name {
        NodeName::Path(syn::ExprPath {
            qself: None,
            attrs,
            path,
        }) if attrs.is_empty() => {
            let Some(name_ident) = path.get_ident() else {
                return false;
            };
            name_ident == ident
        }
        _ => false,
    }
}

// fn write_escaped(
//     writer: &mut (impl fmt::Write + ?Sized),
//     value: &(impl fmt::Display + ?Sized),
// ) -> fmt::Result {
// }

fn crate_path() -> syn::Path {
    syn::parse_quote!(::russx)
}

fn parse_node(node: RstmlNode) -> syn::Result<Node> {
    match node {
        RstmlNode::Comment(comment) => Ok(Node::Comment(comment)),
        RstmlNode::Doctype(doctype) => Ok(Node::Doctype(doctype)),
        RstmlNode::Fragment(fragment) => Ok(Node::Fragment(NodeFragment {
            open_tag: fragment.tag_open,
            children: fragment
                .children
                .into_iter()
                .map(parse_node)
                .collect::<syn::Result<_>>()?,
            close_tag: fragment.tag_close,
        })),
        RstmlNode::Element(node) if is_name_ident(node.name(), "else") => {
            return Err(syn::Error::new_spanned(
                node,
                "unexpected `<else ...>` node",
            ));
        }
        RstmlNode::Element(node) if is_name_ident(node.name(), "prop") => {
            return Err(syn::Error::new_spanned(
                node,
                "unexpected `<prop ...></_>` node",
            ));
        }
        RstmlNode::Element(rstml::node::NodeElement {
            open_tag:
                rstml::node::atoms::OpenTag {
                    token_lt: open_tag_start,
                    name,
                    generics,
                    attributes,
                    end_tag: open_tag_end,
                },
            children,
            close_tag,
        }) if is_name_ident(&name, "_") => {
            if generics.lt_token.is_some() {
                return Err(syn::Error::new_spanned(generics, "unexpected generics"));
            }
            if !children.is_empty() {
                return Err(syn::Error::new_spanned(
                    quote! { #(#children)* },
                    "discard elements cannot have children",
                ));
            }
            if close_tag.is_some() {
                return Err(syn::Error::new_spanned(
                    close_tag,
                    "discard elements must self close `/>`, separate close tags aren't permitted",
                ));
            }

            let value = match <[_; 1]>::try_from(attributes) {
                Ok([NodeAttribute::Block(value)]) => value,
                Ok([attr]) => {
                    return Err(syn::Error::new_spanned(
                        quote! { #name #attr },
                        r#"expected a single attribute. e.g. `<_ {x = 4}>`"#,
                    ))
                }
                Err(attrs) => {
                    return Err(syn::Error::new_spanned(
                        quote! { #name #(#attrs)* },
                        r#"expected a single attribute. e.g. `<_ {x = 4}>`"#,
                    ))
                }
            };

            Ok(Node::DiscardElement(DiscardNodeElement {
                start: open_tag_start,
                discard_token: syn::parse2(name.to_token_stream())?,
                value,
                end: open_tag_end,
            }))
        }
        RstmlNode::Element(rstml::node::NodeElement {
            open_tag:
                rstml::node::atoms::OpenTag {
                    token_lt: open_tag_start,
                    name,
                    generics,
                    attributes,
                    end_tag: open_tag_end,
                },
            children,
            close_tag,
        }) if is_name_ident(&name, "trust") => {
            if generics.lt_token.is_some() {
                return Err(syn::Error::new_spanned(generics, "unexpected generics"));
            }
            if !children.is_empty() {
                return Err(syn::Error::new_spanned(
                    quote! { #(#children)* },
                    "trust elements cannot have children",
                ));
            }
            if close_tag.is_some() {
                return Err(syn::Error::new_spanned(
                    close_tag,
                    "trust elements must self close `/>`, separate close tags aren't permitted",
                ));
            }

            let value = match <[_; 1]>::try_from(attributes) {
                Ok([NodeAttribute::Block(value)]) => value,
                Ok([attr]) => {
                    return Err(syn::Error::new_spanned(
                        quote! { #name #attr },
                        r#"expected a single value. e.g. `<trust {"<p>Hello</p>"}>`"#,
                    ))
                }
                Err(attrs) => {
                    return Err(syn::Error::new_spanned(
                        quote! { #name #(#attrs)* },
                        r#"expected a single value. e.g. `<trust {"<p>Hello</p>"}>`"#,
                    ))
                }
            };

            Ok(Node::TrustElement(TrustNodeElement {
                start: open_tag_start,
                trust_token: { kw::trust(name.span()) },
                value,
                end: open_tag_end,
            }))
        }
        RstmlNode::Element(rstml::node::NodeElement {
            open_tag:
                rstml::node::atoms::OpenTag {
                    token_lt: open_tag_start,
                    name,
                    generics,
                    attributes,
                    end_tag: open_tag_end,
                },
            children,
            close_tag,
        }) if is_name_ident(&name, "let") => {
            if generics.lt_token.is_some() {
                return Err(syn::Error::new_spanned(generics, "unexpected generics"));
            }
            if !children.is_empty() {
                return Err(syn::Error::new_spanned(
                    quote! { #(#children)* },
                    "let elements cannot have children",
                ));
            }
            if close_tag.is_some() {
                return Err(syn::Error::new_spanned(
                    close_tag,
                    "let elements must self close `/>`, separate close tags aren't permitted",
                ));
            }

            let (var_token, (binding_paren, binding), value) =
                match <[_; 2]>::try_from(attributes) {
                    Ok(
                        [NodeAttribute::Attribute(KeyedAttribute {
                            key,
                            possible_value: KeyedAttributeValue::Binding(binding),
                        }), NodeAttribute::Block(value)],
                    ) if is_name_ident(&key, "var") => (
                        kw::var(key.span()),
                        fn_binding_to_pat(binding, true)?,
                        value,
                    ),
                    Ok([attr, attr2]) => return Err(syn::Error::new_spanned(
                        quote! { #name #attr # attr2 },
                        "expected a binding to var and a value. e.g. `<let var(pat: ty) {value}>`",
                    )),
                    Err(attrs) => return Err(syn::Error::new_spanned(
                        quote! { #name #(#attrs)* },
                        "expected a binding to var and a value. e.g. `<let var(pat: ty) {value}>`",
                    )),
                };

            Ok(Node::LetElement(LetNodeElement {
                start: open_tag_start,
                let_token: syn::parse2(name.to_token_stream())?,
                var_token,
                binding_paren,
                binding,
                value,
                end: open_tag_end,
            }))
        }
        RstmlNode::Element(rstml::node::NodeElement {
            open_tag:
                rstml::node::atoms::OpenTag {
                    token_lt: open_tag_start,
                    name,
                    generics,
                    attributes,
                    end_tag: open_tag_end,
                },
            children,
            close_tag,
        }) if is_name_ident(&name, "for") => {
            if generics.lt_token.is_some() {
                return Err(syn::Error::new_spanned(generics, "unexpected generics"));
            }

            let (each_token, (binding_paren, binding), in_token, iter) =
                match <[_; 3]>::try_from(attributes) {
                    Ok(
                        [NodeAttribute::Attribute(KeyedAttribute {
                            key: each_token,
                            possible_value: KeyedAttributeValue::Binding(binding),
                        }), NodeAttribute::Attribute(KeyedAttribute {
                            key: in_token,
                            possible_value: KeyedAttributeValue::None,
                        }), NodeAttribute::Block(value)],
                    ) if is_name_ident(&each_token, "each") && is_name_ident(&in_token, "in") => (
                        kw::each(each_token.span()),
                        fn_binding_to_pat(binding, false)?,
                        {
                            type In = Token![in];
                            In { span: name.span() }
                        },
                        value,
                    ),
                    Ok(attrs) => return Err(syn::Error::new_spanned(
                        quote! { #name #(#attrs)* },
                        "expected a binding to var and a value. e.g. `<for each(pat) in {value}>`",
                    )),
                    Err(attrs) => return Err(syn::Error::new_spanned(
                        quote! { #name #(#attrs)* },
                        "expected a binding to var and a value. e.g. `<for each(pat) in {value}>`",
                    )),
                };

            Ok(Node::ForElement(ForNodeElement {
                open_tag: ForOpenTag {
                    start: open_tag_start,
                    for_token: syn::parse2(name.to_token_stream())?,
                    each_token,
                    binding_paren,
                    binding,
                    in_token,
                    iter,
                    end: open_tag_end,
                },
                children: children
                    .into_iter()
                    .map(parse_node)
                    .collect::<syn::Result<_>>()?,
                close_tag: close_tag.map(KwCloseTag::from_rstml).transpose()?,
            }))
        }
        RstmlNode::Element(rstml::node::NodeElement {
            open_tag:
                rstml::node::atoms::OpenTag {
                    token_lt: open_tag_start,
                    name: match_token,
                    generics,
                    attributes,
                    end_tag: open_tag_end,
                },
            children,
            close_tag,
        }) if is_name_ident(&match_token, "match") => {
            if generics.lt_token.is_some() {
                return Err(syn::Error::new_spanned(generics, "unexpected generics"));
            }

            Ok(Node::MatchElement(MatchNodeElement {
                open_tag: MatchOpenTag {
                    start: open_tag_start,
                    match_token: syn::parse2(match_token.to_token_stream())?,
                    value: 'not_error: {
                        let attrs = {
                            match <[_; 1]>::try_from(attributes) {
                                Ok([NodeAttribute::Block(value)]) => break 'not_error value,
                                Ok(attrs) => attrs.into(),
                                Err(attrs) => attrs,
                            }
                        };
                        return Err(syn::Error::new_spanned(
                            quote! { #match_token #(#attrs)* },
                            "expected one of the following `<if {cond}>` or `<if let(pat) {cond}>`",
                        ));
                    },
                    end: open_tag_end,
                },
                arms: {
                    let mut arms = vec![];
                    for child in children {
                        match child {
                            RstmlNode::Element(rstml::node::NodeElement {
                                open_tag:
                                    rstml::node::atoms::OpenTag {
                                        token_lt: open_tag_start,
                                        name: on_token,
                                        generics,
                                        attributes,
                                        end_tag: open_tag_end,
                                    },
                                children,
                                close_tag,
                            }) if is_name_ident(&on_token, "on") => {
                                if generics.lt_token.is_some() {
                                    return Err(syn::Error::new_spanned(
                                        generics,
                                        "unexpected generics",
                                    ));
                                }
                                if !children.is_empty() {
                                    return Err(syn::Error::new_spanned(
                                        quote! { #(#children)* },
                                        "let elements cannot have children",
                                    ));
                                }
                                if close_tag.is_some() {
                                    return Err(syn::Error::new_spanned(
                                        close_tag,
                                        "let elements must self close `/>`, \
                                         separate close tags aren't permitted",
                                    ));
                                }

                                arms.push((
                                    'not_error: {
                                        let attrs = match attributes.len() {
                                            1 => match <[_; 1]>::try_from(attributes).unwrap() {
                                                [NodeAttribute::Attribute(KeyedAttribute {
                                                    key: case_token,
                                                    possible_value:
                                                        KeyedAttributeValue::Binding(binding),
                                                })] if is_name_ident(&case_token, "case") => {
                                                    let (binding_paren, binding) =
                                                        fn_binding_to_pat(binding, false)?;
                                                    break 'not_error MatchArmTag {
                                                        start: open_tag_start,
                                                        on_token: syn::parse2(
                                                            on_token.to_token_stream(),
                                                        )?,
                                                        case_token: syn::parse2(
                                                            case_token.to_token_stream(),
                                                        )?,
                                                        binding_paren,
                                                        binding,
                                                        guard: None,
                                                        end: open_tag_end,
                                                    };
                                                }
                                                attrs => attrs.into(),
                                            },
                                            3 => match <[_; 3]>::try_from(attributes).unwrap() {
                                                [NodeAttribute::Attribute(KeyedAttribute {
                                                    key: case_token,
                                                    possible_value:
                                                        KeyedAttributeValue::Binding(binding),
                                                }), NodeAttribute::Attribute(KeyedAttribute {
                                                    key: if_token,
                                                    possible_value: KeyedAttributeValue::None,
                                                }), NodeAttribute::Block(if_cond)]
                                                    if is_name_ident(&case_token, "case")
                                                        && is_name_ident(&if_token, "if") =>
                                                {
                                                    let (binding_paren, binding) =
                                                        fn_binding_to_pat(binding, false)?;
                                                    break 'not_error MatchArmTag {
                                                        start: open_tag_start,
                                                        on_token: syn::parse2(
                                                            on_token.to_token_stream(),
                                                        )?,
                                                        case_token: syn::parse2(
                                                            case_token.to_token_stream(),
                                                        )?,
                                                        binding_paren,
                                                        binding,
                                                        guard: Some((
                                                            syn::parse2(
                                                                if_token.to_token_stream(),
                                                            )?,
                                                            if_cond,
                                                        )),
                                                        end: open_tag_end,
                                                    };
                                                }
                                                attrs => attrs.into(),
                                            },
                                            _ => attributes,
                                        };
                                        return Err(syn::Error::new_spanned(
                                            quote! { #on_token #(#attrs)* },
                                            "expect an on-case node, \
                                         e.g. `<on case(pat)>` or `<on case(pat) if {cond}>`",
                                        ));
                                    },
                                    vec![],
                                ));
                            }
                            node => {
                                let Some((_, arm_children)) = arms.last_mut() else {
                                    return Err(syn::Error::new_spanned(
                                        node,
                                        "expect an on-case node, \
                                         e.g. `<on case(pat)>` or `<on case(pat) if {cond}>`",
                                    ));
                                };
                                arm_children.push(parse_node(node)?);
                            }
                        }
                    }

                    arms
                },
                close_tag: close_tag.map(KwCloseTag::from_rstml).transpose()?,
            }))
        }
        RstmlNode::Element(rstml::node::NodeElement {
            open_tag:
                rstml::node::atoms::OpenTag {
                    token_lt: open_tag_start,
                    name,
                    generics,
                    attributes,
                    end_tag: open_tag_end,
                },
            children,
            close_tag,
        }) if is_name_ident(&name, "if") => {
            if generics.lt_token.is_some() {
                return Err(syn::Error::new_spanned(generics, "unexpected generics"));
            }

            let (let_binding, value) = 'not_error: {
                let attrs = {
                    match <[_; 1]>::try_from(attributes) {
                        Ok([NodeAttribute::Block(value)]) => break 'not_error (None, value),
                        Ok(attrs) => attrs.into(),
                        Err(attrs) => match <[_; 2]>::try_from(attrs) {
                            Ok(
                                [NodeAttribute::Attribute(KeyedAttribute {
                                    key,
                                    possible_value: KeyedAttributeValue::Binding(binding),
                                }), NodeAttribute::Block(value)],
                            ) if is_name_ident(&key, "let") => {
                                let (binding_paren, binding) = fn_binding_to_pat(binding, false)?;
                                break 'not_error (
                                    Some(IfLetBinding {
                                        let_token: {
                                            type Let = Token![let];
                                            Let { span: key.span() }
                                        },
                                        binding_paren,
                                        binding,
                                    }),
                                    value,
                                );
                            }
                            Ok(attrs) => attrs.into(),
                            Err(attrs) => attrs,
                        },
                    }
                };
                return Err(syn::Error::new_spanned(
                    quote! { #name #(#attrs)* },
                    "expected one of the following `<if {cond}>` or `<if let(pat) {cond}>`",
                ));
            };

            let mut children = children.into_iter();

            let mut if_section = vec![];
            let mut sep = None;
            for child in &mut children {
                let RstmlNode::Element(child) = child else {
                    if_section.push(parse_node(child)?);
                    continue;
                };
                if !is_name_ident(child.name(), "else") {
                    if_section.push(parse_node(RstmlNode::Element(child))?);
                    continue;
                }
                sep = Some(child);
                break;
            }

            let mut else_if_sections = vec![];
            let mut else_section = None;
            while let Some(sep_el) = sep.take() {
                match <[_; 2]>::try_from(sep_el.open_tag.attributes) {
                    Ok(
                        [NodeAttribute::Attribute(KeyedAttribute {
                            key: if_token,
                            possible_value: KeyedAttributeValue::None,
                        }), NodeAttribute::Block(value)],
                    ) if is_name_ident(&if_token, "if") => {
                        let else_if_tag = ElseIfTag {
                            start: sep_el.open_tag.token_lt,
                            else_token: {
                                type Else = Token![else];
                                Else {
                                    span: sep_el.open_tag.name.span(),
                                }
                            },
                            if_token: {
                                type If = Token![if];
                                If {
                                    span: if_token.span(),
                                }
                            },
                            let_binding: None,
                            value,
                            end: sep_el.open_tag.end_tag,
                        };
                        let mut else_if_section = vec![];
                        for child in &mut children {
                            let RstmlNode::Element(child) = child else {
                                else_if_section.push(parse_node(child)?);
                                continue;
                            };
                            if !is_name_ident(child.name(), "else") {
                                else_if_section.push(parse_node(RstmlNode::Element(child))?);
                                continue;
                            }
                            sep = Some(child);
                            break;
                        }
                        else_if_sections.push((else_if_tag, else_if_section));
                    }
                    Ok([attr, _]) => {
                        return Err(syn::Error::new_spanned(
                            attr,
                            "expected either `<else if let(pat) {}>`, `<else if {}>` or `<else>`",
                        ));
                    }
                    Err(attrs) => match <[_; 3]>::try_from(attrs) {
                        Ok(
                            [NodeAttribute::Attribute(KeyedAttribute {
                                key: if_token,
                                possible_value: KeyedAttributeValue::None,
                            }), NodeAttribute::Attribute(KeyedAttribute {
                                key: let_token,
                                possible_value: KeyedAttributeValue::Binding(binding),
                            }), NodeAttribute::Block(value)],
                        ) if is_name_ident(&if_token, "if") && is_name_ident(&let_token, "let") => {
                            let (binding_paren, binding) = fn_binding_to_pat(binding, false)?;
                            let else_if_tag = ElseIfTag {
                                start: sep_el.open_tag.token_lt,
                                else_token: {
                                    type Else = Token![else];
                                    Else {
                                        span: sep_el.open_tag.name.span(),
                                    }
                                },
                                if_token: {
                                    type If = Token![if];
                                    If {
                                        span: if_token.span(),
                                    }
                                },
                                let_binding: Some(IfLetBinding {
                                    let_token: {
                                        type Let = syn::Token![let];
                                        Let {
                                            span: let_token.span(),
                                        }
                                    },
                                    binding_paren,
                                    binding,
                                }),
                                value,
                                end: sep_el.open_tag.end_tag,
                            };
                            let mut else_if_section = vec![];
                            for child in &mut children {
                                let RstmlNode::Element(child) = child else {
                                    else_if_section.push(parse_node(child)?);
                                    continue;
                                };
                                if !is_name_ident(child.name(), "else") {
                                    else_if_section.push(parse_node(RstmlNode::Element(child))?);
                                    continue;
                                }
                                sep = Some(child);
                                break;
                            }
                            else_if_sections.push((else_if_tag, else_if_section));
                        }
                        Ok([attr, _, _]) => {
                            return Err(syn::Error::new_spanned(
                            attr,
                            "expected either `<else if let(pat) {}>`, `<else if {}>` or `<else>`",
                        ));
                        }
                        Err(attrs) if attrs.is_empty() => {
                            else_section = Some((
                                ElseTag {
                                    start: sep_el.open_tag.token_lt,
                                    else_token: {
                                        type Else = Token![else];
                                        Else {
                                            span: sep_el.open_tag.name.span(),
                                        }
                                    },
                                    end: sep_el.open_tag.end_tag,
                                },
                                (&mut children)
                                    .map(parse_node)
                                    .collect::<syn::Result<_>>()?,
                            ));
                        }
                        Err(attrs) => {
                            return Err(syn::Error::new_spanned(
                            &attrs[0],
                            "expected either `<else if let(pat) {}>`, `<else if {}>` or `<else>`",
                        ));
                        }
                    },
                }
            }

            Ok(Node::IfElement(IfNodeElement {
                open_tag: IfOpenTag {
                    start: open_tag_start,
                    if_token: {
                        type If = Token![if];
                        If { span: name.span() }
                    },
                    let_binding,
                    value,
                    end: open_tag_end,
                },
                if_section,
                else_if_sections,
                else_section,
                close_tag: close_tag.map(KwCloseTag::from_rstml).transpose()?,
            }))
        }
        RstmlNode::Element(rstml::node::NodeElement {
            open_tag:
                rstml::node::atoms::OpenTag {
                    token_lt: open_tag_start,
                    name,
                    generics,
                    attributes,
                    end_tag: open_tag_end,
                },
            children,
            close_tag,
        }) if is_html_element(&name) && generics.lt_token.is_none() => {
            Ok(Node::HtmlElement(HtmlNodeElement {
                open_tag: HtmlOpenTag {
                    start: open_tag_start,
                    name: node_name_to_html_name(name)?,
                    attributes: attributes
                        .into_iter()
                        .map(|attr| match attr {
                            NodeAttribute::Block(block) => {
                                return Err(syn::Error::new_spanned(
                                    block,
                                    "unexpected block attribute, NOTE: HTML elements' keys cannot be blocks",
                                ));
                            }
                            NodeAttribute::Attribute(attr) => Ok(HtmlNodeAttribute {
                                key: node_name_to_html_name(attr.key)?,
                                value: match attr.possible_value {
                                    KeyedAttributeValue::None => None,
                                    KeyedAttributeValue::Binding(binding) => {
                                        return Err(syn::Error::new_spanned(
                                            binding,
                                            "unexpected binding attribute",
                                        ))
                                    }
                                    KeyedAttributeValue::Value(value) => Some(NodeAttributeValue {
                                        eq_token: value.token_eq,
                                        value: value.value,
                                    }),
                                },
                            }),
                        })
                        .collect::<syn::Result<_>>()?,
                    end: open_tag_end,
                },
                children: children
                    .into_iter()
                    .map(parse_node)
                    .collect::<syn::Result<_>>()?,
                close_tag: match close_tag {
                    Some(close_tag) => Some(HtmlCloseTag {
                        start: close_tag.start_tag,
                        name: node_name_to_html_name(close_tag.name)?,
                        end: close_tag.token_gt,
                    }),
                    None => None,
                },
            }))
        }
        RstmlNode::Element(rstml::node::NodeElement {
            open_tag:
                rstml::node::atoms::OpenTag {
                    token_lt: open_tag_start,
                    name: NodeName::Block(name_block),
                    generics,
                    attributes,
                    end_tag: open_tag_end,
                },
            children,
            close_tag,
        }) => {
            if generics.lt_token.is_some() {
                return Err(syn::Error::new_spanned(generics, "unexpected generics"));
            }
            if let Some(attr) = attributes.first() {
                return Err(syn::Error::new_spanned(
                    attr,
                    "dynamic templates cannot take any attributes",
                ));
            }
            if !children.is_empty() {
                return Err(syn::Error::new_spanned(
                    quote! { #(#children)* },
                    "dynamic templates cannot have children",
                ));
            }
            if close_tag.is_some() {
                return Err(syn::Error::new_spanned(
                    close_tag,
                    "dynamic templates must self close `/>`, separate close tags aren't permitted",
                ));
            }

            Ok(Node::DynTmplElement(DynTmplNodeElement {
                start: open_tag_start,
                name_block,
                end1: open_tag_end.token_solidus.unwrap(),
                end2: open_tag_end.token_gt,
            }))
        }
        RstmlNode::Element(rstml::node::NodeElement {
            open_tag:
                rstml::node::atoms::OpenTag {
                    token_lt: open_tag_start,
                    name,
                    generics,
                    attributes,
                    end_tag: open_tag_end,
                },
            children,
            close_tag,
        }) => {
            if generics.lt_token.is_some() {
                return Err(syn::Error::new_spanned(generics, "unexpected generics"));
            }
            let NodeName::Path(name) = name else {
                return Err(syn::Error::new_spanned(name, "expected a path name"));
            };

            let mut new_children = vec![];
            let mut prop_children = vec![];

            for child in children {
                match child {
                    RstmlNode::Element(rstml::node::NodeElement {
                        open_tag:
                            rstml::node::atoms::OpenTag {
                                token_lt: open_tag_start,
                                name,
                                generics,
                                attributes,
                                end_tag: open_tag_end,
                            },
                        children,
                        close_tag,
                    }) if is_name_ident(&name, "prop") => {
                        if generics.lt_token.is_some() {
                            return Err(syn::Error::new_spanned(generics, "unexpected generics"));
                        }
                        const EXPECTED_IDENT: &str =
                            "expected a single identifier, the property's name";
                        let attributes = <[_; 1]>::try_from(attributes).map_err(|attributes| {
                            syn::Error::new_spanned(
                                quote! { #name #(#attributes)* },
                                EXPECTED_IDENT,
                            )
                        })?;
                        let [NodeAttribute::Attribute(KeyedAttribute {
                            key: NodeName::Path(attr),
                            possible_value: KeyedAttributeValue::None,
                        })] = attributes
                        else {
                            return Err(syn::Error::new_spanned(
                                quote! { #name #(#attributes)* },
                                EXPECTED_IDENT,
                            ));
                        };
                        let ident = expr_path_to_ident(attr)
                            .map_err(|attr| syn::Error::new_spanned(attr, EXPECTED_IDENT))?;
                        prop_children.push(PropNodeElement {
                            open_tag: PropOpenTag {
                                start: open_tag_start,
                                prop_token: kw::prop(name.span()),
                                name: ident,
                                end: open_tag_end,
                            },
                            children: children
                                .into_iter()
                                .map(parse_node)
                                .collect::<syn::Result<_>>()?,
                            close_tag: close_tag.map(KwCloseTag::from_rstml).transpose()?,
                        })
                    }
                    _ => new_children.push(parse_node(child)?),
                }
            }

            Ok(Node::StaticTmplElement(StaticTmplNodeElement {
                open_tag: StaticTmplOpenTag {
                    start: open_tag_start,
                    name,
                    attributes: attributes
                        .into_iter()
                        .map(|attr| match attr {
                            NodeAttribute::Block(block) => {
                                return Err(syn::Error::new_spanned(block, "unexpected block attr"))
                            }
                            NodeAttribute::Attribute(attr) => {
                                Ok(StaticTmplNodeAttribute {
                                    key: match attr.key {
                                        NodeName::Path(path) => {
                                            expr_path_to_ident(path).map_err(|path| {
                                                syn::Error::new_spanned(
                                                    path,
                                                    "template argument name must be a single identifier",
                                                )
                                            })?
                                        }
                                        name => return Err(syn::Error::new_spanned(
                                            name,
                                            "template argument name must be a single identifier",
                                        )),
                                    },
                                    value: match attr.possible_value {
                                        KeyedAttributeValue::None => None,
                                        KeyedAttributeValue::Value(value) => {
                                            Some(NodeAttributeValue {
                                                eq_token: value.token_eq,
                                                value: value.value,
                                            })
                                        }
                                        KeyedAttributeValue::Binding(binding) => {
                                            return Err(syn::Error::new_spanned(
                                                binding,
                                                "unexpected binding attribute",
                                            ))
                                        }
                                    },
                                })
                            }
                        })
                        .collect::<syn::Result<_>>()?,
                    end: open_tag_end,
                },
                children: new_children,
                prop_children,
                close_tag: match close_tag {
                    Some(close_tag) => Some(StaticTmplCloseTag {
                        start: close_tag.start_tag,
                        name: {
                            let NodeName::Path(name) = close_tag.name else {
                                unreachable!();
                            };
                            name
                        },
                        end: close_tag.token_gt,
                    }),
                    None => None,
                },
            }))
        }
        RstmlNode::Block(block) => Ok(Node::Block(block)),
        RstmlNode::Text(text) => Ok(Node::Text(text)),
        RstmlNode::RawText(text) => Ok(Node::RawText(text)),
    }
}

struct NodeBody(pub Vec<Node>);

impl Parse for NodeBody {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.is_empty() {
            Ok(Self(vec![]))
        } else {
            let nodes = rstml::Parser::new(
                rstml::ParserConfig::new()
                    .recover_block(true)
                    .raw_text_elements(["style", "script"].into())
                    .always_self_closed_elements(
                        [
                            "arena", "base", "br", "col", "embed", "hr", "img", "input", "link",
                            "meta", "param", "source", "track", "wbr", // html
                            "else", "on", "let", "_", "trust", // control-flow
                        ]
                        .into(),
                    )
                    .element_close_use_default_wildcard_ident(false),
            )
            .parse_syn_stream(&input)
            .into_result()?;

            Ok(Self(
                nodes
                    .into_iter()
                    .map(parse_node)
                    .collect::<syn::Result<_>>()?,
            ))
        }
    }
}

#[allow(dead_code)]
enum TmplPropMeta {
    Into {
        into_token: kw::into,
    },
    Default {
        default_token: Token![default],
    },
    DefaultValue {
        default_token: Token![default],
        eq_token: Token![=],
        value: syn::Expr,
    },
    Optional {
        optional_token: kw::optional,
    },
    Toggle {
        toggle_token: kw::toggle,
    },
}

impl Parse for TmplPropMeta {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(kw::into) {
            Ok(Self::Into {
                into_token: input.parse()?,
            })
        } else if input.peek(Token![default]) {
            let default_token = input.parse()?;
            if input.peek(Token![=]) {
                Ok(Self::DefaultValue {
                    default_token,
                    eq_token: input.parse()?,
                    value: input.parse()?,
                })
            } else {
                Ok(Self::Default { default_token })
            }
        } else if input.peek(kw::optional) {
            Ok(Self::Optional {
                optional_token: input.parse()?,
            })
        } else if input.peek(kw::toggle) {
            Ok(Self::Toggle {
                toggle_token: input.parse()?,
            })
        } else {
            Err(syn::Error::new(
                input.span(),
                "unexpected token, see documentation",
            ))
        }
    }
}

#[allow(dead_code)]
struct TmplProp {
    pound_token: Token![#],
    style: syn::AttrStyle,
    bracket_token: token::Bracket,
    prop_token: kw::prop,
    delimiter: syn::MacroDelimiter,
    meta_inner: Punctuated<TmplPropMeta, Token![,]>,
}

struct TmplArg {
    pub prop_attrs: Vec<TmplProp>,
    pub attrs: Vec<syn::Attribute>,
    pub pat: syn::PatIdent,
    pub colon_token: Token![:],
    pub ty: Box<syn::Type>,
}

impl Parse for TmplArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let arg: syn::FnArg = input.parse()?;

        let syn::FnArg::Typed(arg) = arg else {
            return Err(syn::Error::new_spanned(
                arg,
                "`self` parameter is not allowed in templates",
            ));
        };

        let (prop_attrs, attrs): (Vec<_>, _) = arg
            .attrs
            .into_iter()
            .partition(|attr| attr.path().is_ident("prop"));

        Ok(Self {
            attrs,
            prop_attrs: prop_attrs
                .into_iter()
                .map(|attr| -> syn::Result<_> {
                    let meta_inner = attr.parse_args_with(Punctuated::parse_terminated)?;
                    let syn::Meta::List(mut meta) = attr.meta else {
                        unreachable!()
                    };

                    Ok(TmplProp {
                        pound_token: attr.pound_token,
                        style: attr.style,
                        bracket_token: attr.bracket_token,
                        prop_token: kw::prop(
                            meta.path.segments.pop().unwrap().into_value().ident.span(),
                        ),
                        delimiter: meta.delimiter,
                        meta_inner,
                    })
                })
                .collect::<syn::Result<_>>()?,
            pat: match *arg.pat {
                syn::Pat::Ident(pat) => pat,
                pat => return Err(syn::Error::new_spanned(
                    pat,
                    "Only `prop: bool` or `prop @ (x, y): (u32, u32)` style properties are allowed",
                )),
            },
            colon_token: arg.colon_token,
            ty: arg.ty,
        })
    }
}

#[allow(dead_code)]
struct ItemTmpl {
    pub attrs: Vec<syn::Attribute>,
    pub vis: syn::Visibility,

    pub fn_token: Token![fn],
    pub ident: syn::Ident,
    pub generics: syn::Generics,
    pub paren_token: token::Paren,
    pub inputs: Punctuated<TmplArg, Token![,]>,

    pub brace_token: token::Brace,
    pub body: NodeBody,
}

impl Parse for ItemTmpl {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut generics: syn::Generics;
        let args_content;
        let body_content;

        Ok(Self {
            attrs: syn::Attribute::parse_outer(input)?,
            vis: input.parse()?,
            fn_token: input.parse()?,
            ident: input.parse()?,
            paren_token: {
                generics = input.parse()?;
                parenthesized!(args_content in input)
            },
            inputs: Punctuated::parse_terminated(&args_content)?,
            generics: {
                generics.where_clause = input.parse()?;
                generics
            },
            brace_token: braced!(body_content in input),
            body: body_content.parse()?,
        })
    }
}

const EST_EXPR_SIZE: usize = 10;

fn try_stringify_expr(expr: &syn::Expr) -> Option<String> {
    match expr {
        syn::Expr::Lit(syn::ExprLit { attrs, lit }) if attrs.is_empty() => match lit {
            syn::Lit::Str(s) => Some(s.value()),
            syn::Lit::Byte(byte) => Some(byte.value().to_string()),
            syn::Lit::Char(ch) => Some(ch.value().to_string()),
            syn::Lit::Int(int) => Some(int.to_string()),
            syn::Lit::Float(float) => Some(float.to_string()),
            syn::Lit::Bool(bool) => Some(bool.value().to_string()),
            _ => None,
        },
        syn::Expr::Paren(syn::ExprParen { expr, attrs, .. }) if attrs.is_empty() => {
            try_stringify_expr(expr)
        }
        syn::Expr::Block(syn::ExprBlock { block, attrs, .. }) if attrs.is_empty() => {
            try_stringify_block(block)
        }
        _ => None,
    }
}

fn try_stringify_block(block: &syn::Block) -> Option<String> {
    let Some(syn::Stmt::Expr(expr, None)) = block.stmts.iter().rfind(|stmt| {
        matches!(
            stmt,
            syn::Stmt::Expr(_, _) | syn::Stmt::Macro(_) | syn::Stmt::Local(_),
        )
    }) else {
        return None;
    };
    try_stringify_expr(expr)
}

fn flush_buffer(tokens: &mut TokenStream, buf: &mut String) {
    if !buf.is_empty() {
        tokens.append_all(quote! {
            ::core::fmt::Write::write_str(__writer, #buf)?;
        });
        buf.clear();
    }
}

/// Try to isolate `tokens` from `break`.
fn isolate_block(tokens: impl ToTokens) -> TokenStream {
    // might be improved to isolate `return` or `__writer` in the future.

    // // this breaks rust analyzer
    // quote! {
    //     match (|| -> ::core::result::Result<_, ::core::fmt::Error> {
    //         Ok({ #tokens })
    //     })() {
    //         Ok(__x) => __x,
    //         Err(__err) => return Err(__err),
    //     }
    // }
    quote! { loop { break { #tokens } } }
}

struct TmplBodyNode<'a> {
    size: &'a mut usize,
    buf: &'a mut String,
    node: &'a Node,
}

impl<'a> TmplBodyNode<'a> {
    pub fn new(node: &'a Node, size: &'a mut usize, buf: &'a mut String) -> Self {
        Self { node, size, buf }
    }

    fn write_escaped_str(&mut self, value: impl Display) {
        pub struct EscapeWriter<'a>(&'a mut String);

        impl Write for EscapeWriter<'_> {
            #[inline]
            fn write_str(&mut self, s: &str) -> std::fmt::Result {
                askama_escape::Html.write_escaped(&mut *self.0, s)
            }
        }

        write!(EscapeWriter(&mut self.buf), "{value}").unwrap();
    }

    #[inline]
    fn flush_buffer(&mut self, tokens: &mut TokenStream) {
        *self.size += self.buf.len();
        flush_buffer(tokens, &mut *self.buf)
    }

    fn write_displayable(&mut self, tokens: &mut TokenStream, displayable: &impl ToTokens) {
        let crate_path = crate_path();

        self.flush_buffer(tokens);
        *self.size += EST_EXPR_SIZE;

        let displayable = isolate_block(displayable);
        tokens.append_all(quote! {
            #crate_path::write_escaped(
                __writer,
                &#displayable,
            )?;
        });
    }

    fn write_block(&mut self, tokens: &mut TokenStream, block: &syn::Block) {
        match try_stringify_block(block) {
            Some(s) => {
                tokens.append_all(isolate_block(quote! {
                    #block;
                }));
                tokens.append_all(quote! { ; });
                self.write_escaped_str(&s);
            }
            None => {
                match block.stmts.iter().rposition(|stmt| {
                    matches!(
                        stmt,
                        syn::Stmt::Expr(_, _) | syn::Stmt::Macro(_) | syn::Stmt::Local(_),
                    )
                }) {
                    Some(i) => {
                        let (before, after) = block.stmts.split_at(i);
                        let (stmt, after) = after.split_first().unwrap();

                        let crate_path = crate_path();

                        self.flush_buffer(tokens);
                        *self.size += EST_EXPR_SIZE;
                        tokens.append_all(isolate_block(quote! {
                            #(#before)*
                            #crate_path::write_escaped(__writer, &{#stmt})?;
                            #(#after)*
                        }));
                        tokens.append_all(quote! { ; });
                    }
                    None => {
                        self.write_displayable(tokens, block);
                    }
                }
            }
        }
    }

    fn write_node_block(&mut self, tokens: &mut TokenStream, block: &rstml::node::NodeBlock) {
        match block {
            rstml::node::NodeBlock::ValidBlock(block) => self.write_block(tokens, block),
            rstml::node::NodeBlock::Invalid { .. } => self.write_displayable(tokens, block),
        }
    }

    fn write_expr(&mut self, tokens: &mut TokenStream, expr: &syn::Expr) {
        match try_stringify_expr(expr) {
            Some(s) => {
                tokens.append_all(isolate_block(quote! {
                    #expr;
                }));
                tokens.append_all(quote! { ; });
                self.write_escaped_str(&s);
            }
            None => self.write_displayable(tokens, expr),
        }
    }

    // fn write_node_name(&mut self, tokens: &mut TokenStream, name: &NodeName) {
    //     match name {
    //         NodeName::Block(block) => self.write_block(tokens, block),
    //         NodeName::Punctuated(pname) => {
    //             use rstml::node::NodeNameFragment;
    //             use syn::punctuated::Pair;
    //             'raw: {
    //                 if pname.len() < 2 {
    //                     break 'raw;
    //                 }

    //                 let mut pairs = pname.pairs();

    //                 let Some(Pair::Punctuated(NodeNameFragment::Ident(ident), punct)) =
    //                     pairs.next()
    //                 else {
    //                     break 'raw;
    //                 };

    //                 if ident != "raw" || punct.as_char() != ':' {
    //                     break 'raw;
    //                 }

    //                 for pair in pairs {
    //                     self.write_escaped_str(pair.value());
    //                     if let Some(&punct) = pair.punct() {
    //                         self.write_escaped_str(punct);
    //                     }
    //                 }
    //                 return;
    //             }

    //             self.write_escaped_str(name);
    //         }
    //         NodeName::Path(_) => {
    //             self.write_escaped_str(name);
    //         }
    //     }
    // }

    fn generate(&mut self, tokens: &mut TokenStream) {
        match self.node {
            Node::Comment(comment) => {
                self.buf.push_str("<!-- ");
                self.write_escaped_str(comment.value.value());
                self.buf.push_str(" -->");
            }
            Node::Doctype(doctype) => {
                write!(
                    self.buf,
                    "<!{} {}>",
                    doctype.token_doctype,
                    doctype.value.to_string_best(),
                )
                .unwrap();
            }
            Node::Fragment(fragment) => {
                let mut block_tokens = TokenStream::new();
                for child in &fragment.children {
                    TmplBodyNode::new(child, &mut *self.size, &mut *self.buf)
                        .generate(&mut block_tokens);
                }
                tokens.append_all(quote! { { #block_tokens } });
            }
            Node::DiscardElement(DiscardNodeElement { value, .. }) => {
                tokens.append_all(isolate_block(quote! { #value; }));
                tokens.append_all(quote! { ; });
            }
            Node::TrustElement(TrustNodeElement { value, .. }) => {
                fn write_block(value: impl ToTokens) -> TokenStream {
                    quote! {
                        // #[allow(unused_braces)]
                        match #value {
                            __value => {
                                use ::core::fmt::Write;
                                ::core::write!(__writer, "{}", __value)?;
                            }
                        }
                    }
                }
                match value {
                    NodeBlock::ValidBlock(value) => match try_stringify_block(value) {
                        Some(s) => {
                            tokens.append_all(isolate_block(quote! { #value; }));
                            tokens.append_all(quote! { ; });
                            self.buf.push_str(&s);
                        }
                        None => {
                            match value.stmts.iter().rposition(|stmt| {
                                matches!(
                                    stmt,
                                    syn::Stmt::Expr(_, _)
                                        | syn::Stmt::Macro(_)
                                        | syn::Stmt::Local(_),
                                )
                            }) {
                                Some(i) => {
                                    let (before, after) = value.stmts.split_at(i);
                                    let (stmt, after) = after.split_first().unwrap();

                                    self.flush_buffer(tokens);
                                    *self.size += EST_EXPR_SIZE;

                                    let show_stmt = write_block(quote! { &{#stmt} });
                                    tokens.append_all(isolate_block(quote! {
                                        #(#before)*
                                        #show_stmt
                                        #(#after)*
                                    }));
                                    tokens.append_all(quote! { ; });
                                }
                                None => tokens.append_all(write_block(value)),
                            }
                        }
                    },
                    NodeBlock::Invalid { .. } => tokens.append_all(write_block(value)),
                }
            }
            Node::LetElement(LetNodeElement {
                let_token,
                binding,
                value,
                ..
            }) => tokens.append_all(quote! {
                #let_token #binding = #value;
            }),
            Node::ForElement(ForNodeElement {
                open_tag:
                    ForOpenTag {
                        for_token,
                        binding,
                        in_token,
                        iter,
                        ..
                    },
                children,
                ..
            }) => {
                self.flush_buffer(tokens);
                let mut block_tokens = TokenStream::new();
                let init_size = *self.size;
                for child in children {
                    TmplBodyNode::new(child, &mut *self.size, &mut *self.buf)
                        .generate(&mut block_tokens);
                }
                self.buf.push(' ');
                self.flush_buffer(&mut block_tokens);
                *self.size = 6 * *self.size - 5 * init_size;

                let iter = isolate_block(iter);
                tokens.append_all(quote! {
                    // #[allow(unused_braces)]
                    #for_token #binding #in_token #iter {
                        #block_tokens
                    }
                });
            }
            Node::MatchElement(MatchNodeElement {
                open_tag:
                    MatchOpenTag {
                        match_token, value, ..
                    },
                arms,
                close_tag: _,
            }) => {
                self.flush_buffer(tokens);
                let mut block_tokens = TokenStream::new();
                let init_size = *self.size;

                self.flush_buffer(&mut block_tokens);

                let mut max_arm_size = init_size;
                for (MatchArmTag { binding, guard, .. }, children) in arms {
                    let mut subblock_tokens = TokenStream::new();

                    for child in children {
                        TmplBodyNode::new(child, &mut *self.size, &mut *self.buf)
                            .generate(&mut subblock_tokens);
                    }
                    self.flush_buffer(&mut subblock_tokens);

                    let guard = guard.as_ref().map(|(if_token, cond)| {
                        let cond = isolate_block(cond);
                        quote! { #if_token #cond }
                    });

                    block_tokens.append_all(quote! {
                        #binding #guard => { #subblock_tokens }
                    });

                    max_arm_size = max_arm_size.max(*self.size);
                    *self.size = init_size;
                }
                *self.size = max_arm_size;

                let value = isolate_block(value);
                tokens.append_all(quote! {
                    // #[allow(unused_braces)]
                    #match_token #value {
                        #block_tokens
                    }
                });

                self.buf.push(' ');
            }
            Node::IfElement(IfNodeElement {
                open_tag:
                    IfOpenTag {
                        if_token,
                        let_binding,
                        value,
                        ..
                    },
                if_section,
                else_if_sections,
                else_section,
                close_tag: _,
            }) => {
                self.flush_buffer(tokens);
                let mut block_tokens = TokenStream::new();
                let init_size = *self.size;
                for child in if_section {
                    TmplBodyNode::new(child, &mut *self.size, &mut *self.buf)
                        .generate(&mut block_tokens);
                }

                let let_binding = let_binding.as_ref().map(
                    |IfLetBinding {
                         let_token, binding, ..
                     }| quote! { #let_token #binding = },
                );

                self.flush_buffer(&mut block_tokens);
                let value = isolate_block(value);
                tokens.append_all(quote! {
                    // #[allow(unused_braces)]
                    #if_token #let_binding #value { #block_tokens }
                });
                let mut new_size = *self.size;

                for (
                    ElseIfTag {
                        else_token,
                        if_token,
                        let_binding,
                        value,
                        ..
                    },
                    children,
                ) in else_if_sections
                {
                    *self.size = init_size;
                    let mut block_tokens = TokenStream::new();
                    for child in children {
                        TmplBodyNode::new(child, &mut *self.size, &mut *self.buf)
                            .generate(&mut block_tokens);
                    }

                    let let_binding = let_binding.as_ref().map(
                        |IfLetBinding {
                             let_token, binding, ..
                         }| quote! { #let_token #binding = },
                    );

                    self.flush_buffer(&mut block_tokens);
                    let value = isolate_block(value);
                    tokens.append_all(quote! {
                        #else_token #if_token #let_binding #value { #block_tokens }
                    });

                    new_size = new_size.max(*self.size);
                }

                if let Some((ElseTag { else_token, .. }, children)) = else_section {
                    *self.size = init_size;
                    let mut block_tokens = TokenStream::new();
                    for child in children {
                        TmplBodyNode::new(child, &mut *self.size, &mut *self.buf)
                            .generate(&mut block_tokens);
                    }
                    self.flush_buffer(&mut block_tokens);
                    tokens.append_all(quote! { #else_token { #block_tokens } });

                    new_size = new_size.max(*self.size);
                }
                *self.size = new_size;

                self.buf.push(' ');
            }
            Node::HtmlElement(el) => {
                self.buf.push('<');

                self.write_escaped_str(&el.open_tag.name);

                for attr in &el.open_tag.attributes {
                    self.buf.push(' ');
                    self.write_escaped_str(&attr.key);
                    if let Some(value) = &attr.value {
                        self.buf.push_str("=\"");
                        self.write_expr(tokens, &value.value);
                        self.buf.push('"');
                    }
                }

                if el.open_tag.end.token_solidus.is_some() {
                    self.buf.push_str(" />");
                } else {
                    self.buf.push('>');
                }
                // pub end: OpenTagEnd,

                let mut block_tokens = TokenStream::new();
                for child in &el.children {
                    TmplBodyNode::new(child, &mut *self.size, &mut *self.buf)
                        .generate(&mut block_tokens);
                }
                if !block_tokens.is_empty() {
                    tokens.append_all(quote! { { #block_tokens } });
                }

                if let Some(_close_tag) = &el.close_tag {
                    self.buf.push_str("</");

                    self.write_escaped_str(&el.open_tag.name);
                    self.buf.push('>');
                }
                self.buf.push(' ');
            }
            Node::DynTmplElement(DynTmplNodeElement { name_block, .. }) => {
                let crate_path = crate_path();

                self.flush_buffer(tokens);
                let name_block = isolate_block(name_block);
                tokens.append_all(quote! {
                    // #[allow(unused_braces)]
                    #crate_path::Template::render_into(
                        #name_block,
                        __writer,
                    )?;
                });
                self.buf.push(' ');
            }
            Node::StaticTmplElement(StaticTmplNodeElement {
                open_tag:
                    StaticTmplOpenTag {
                        name, attributes, ..
                    },
                children,
                prop_children,
                close_tag: _,
            }) => {
                let crate_path = crate_path();

                self.flush_buffer(tokens);

                let apply_children = {
                    assert!(self.buf.is_empty());
                    let init_size = *self.size;
                    let mut block_tokens = TokenStream::new();
                    for child in children {
                        TmplBodyNode::new(child, &mut *self.size, &mut *self.buf)
                            .generate(&mut block_tokens);
                    }
                    self.flush_buffer(&mut block_tokens);
                    let children_size = *self.size - init_size;

                    if block_tokens.is_empty() {
                        None
                    } else {
                        Some(quote! {
                            .children(#crate_path::TemplateFn::new(#children_size, |__writer| {
                                #block_tokens
                                ::core::fmt::Result::Ok(())
                            }))
                        })
                    }
                };

                let apply_attrs =
                    attributes
                        .iter()
                        .map(|StaticTmplNodeAttribute { key, value }| {
                            let value = value.as_ref().map(|value| isolate_block(&value.value));
                            quote! { #key(#value) }
                        });

                let apply_prop_children = prop_children.iter().map(
                    |PropNodeElement {
                         open_tag: PropOpenTag { name, .. },
                         children,
                         ..
                     }| {
                        assert!(self.buf.is_empty());
                        let init_size = *self.size;
                        let mut block_tokens = TokenStream::new();
                        for child in children {
                            TmplBodyNode::new(child, &mut *self.size, &mut *self.buf)
                                .generate(&mut block_tokens);
                        }
                        self.flush_buffer(&mut block_tokens);
                        let prop_size = *self.size - init_size;

                        quote! {
                            #name(#crate_path::TemplateFn::new(#prop_size, |__writer| {
                                #block_tokens
                                ::core::fmt::Result::Ok(())
                            }))
                        }
                    },
                );

                tokens.append_all(quote! {
                    #crate_path::Template::render_into(
                        #name::Props::builder()
                            #(.#apply_attrs)*
                            #(.#apply_prop_children)*
                            #apply_children
                            .build(),
                        __writer,
                    )?;
                });
                self.buf.push(' ');
            }
            Node::Block(block) => {
                self.write_node_block(tokens, block);
                self.buf.push(' ');
            }
            Node::Text(text) => {
                self.write_escaped_str(text.value_string());
                self.buf.push(' ');
            }
            Node::RawText(text) => {
                self.write_escaped_str(text.to_string_best());
                self.buf.push(' ');
            }
        }
    }
}

impl ToTokens for ItemTmpl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let crate_path = crate_path();

        let Self {
            attrs,
            vis,
            fn_token: _,
            ident,
            generics,
            paren_token: _,
            inputs,
            brace_token: _,
            body,
        } = self;

        // let body = TmplBody { nodes: &body.0 };
        let mut body_tokens = TokenStream::new();
        let mut size = 0;

        {
            let mut buf = String::new();
            for node in &body.0 {
                TmplBodyNode::new(node, &mut size, &mut buf).generate(&mut body_tokens);
            }
            size += buf.len();
            flush_buffer(&mut body_tokens, &mut buf);
        }

        let props_fields =
            inputs.iter().map(|arg| {
                let attrs = &arg.attrs;
                let ident = &arg.pat.ident;
                let colon_token = arg.colon_token;
                let ty = &arg.ty;
                let builders = arg.prop_attrs.iter().flat_map(|attr| &attr.meta_inner).map(
                    |prop| match &prop {
                        TmplPropMeta::Default { .. } => quote! { #[builder(default)] },
                        TmplPropMeta::Into { .. } => quote! { #[builder(setter(into))] },
                        TmplPropMeta::DefaultValue { value, .. } => quote! {
                            #[builder(default = #value)]
                        },
                        TmplPropMeta::Optional { .. } => quote! {
                            #[builder(default, setter(strip_option))]
                        },
                        TmplPropMeta::Toggle { .. } => quote! { #[builder(setter(strip_bool))] },
                    },
                );
                quote! {
                    #(#attrs)*
                    #(#builders)*
                    pub #ident #colon_token #ty,
                }
            });

        let fn_inputs = inputs.iter().map(
            |TmplArg {
                 prop_attrs: _,
                 attrs: _,
                 pat: syn::PatIdent { ident, .. },
                 colon_token,
                 ty,
             }| {
                quote! {
                    #ident #colon_token #ty
                }
            },
        );

        let generate_inputs = inputs.iter().map(
            |TmplArg {
                 prop_attrs: _,
                 attrs: _,
                 pat,
                 colon_token,
                 ty,
             }| {
                quote! {
                    #pat #colon_token #ty
                }
            },
        );

        let instance_fields = inputs.iter().map(
            |TmplArg {
                 pat: syn::PatIdent { ident, .. },
                 ..
             }| ident,
        );
        let call_generate_args = inputs.iter().map(
            |TmplArg {
                 pat: syn::PatIdent { ident, .. },
                 ..
             }| quote! { self.#ident },
        );

        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        let into_generics = syn::Generics {
            lt_token: generics
                .lt_token
                .clone()
                .or_else(|| Some(parse_quote! { < })),
            params: {
                let params = &generics.params;
                parse_quote! { '__self, #params }
            },
            gt_token: generics
                .gt_token
                .clone()
                .or_else(|| Some(parse_quote! { > })),
            where_clause: match &generics.where_clause {
                Some(syn::WhereClause {
                    where_token,
                    predicates,
                }) => {
                    parse_quote! { #where_token Self: '__self, #predicates }
                }
                None => Some(parse_quote! { where Self: '__self }),
            },
        };
        let (impl_into_generics, _, into_where_clause) = into_generics.split_for_impl();

        tokens.append_all(quote! {
            #(#attrs)*
            #vis mod #ident {
                use super::*;

                #(#attrs)*
                #[derive(#crate_path::__typed_builder::TypedBuilder)]
                #[builder(crate_module_path = #crate_path::__typed_builder)]
                // #[builder(doc)]
                pub struct Props #generics #where_clause {
                    #(#props_fields)*
                }

                impl #impl_generics #crate_path::Template for Props #ty_generics #where_clause {
                    const SIZE_HINT: usize = #size;

                    fn render_into(
                        self,
                        writer: &mut dyn ::core::fmt::Write,
                    ) -> ::core::fmt::Result {
                        #[inline]
                        #[doc(hidden)]
                        #[allow(unused_braces)]
                        fn __generate #generics (
                            __writer: &mut dyn ::core::fmt::Write,
                            #(#generate_inputs,)*
                        ) -> ::core::fmt::Result
                        #where_clause
                        {
                            #body_tokens
                            ::core::fmt::Result::Ok(())
                        }

                        __generate(writer, #(#call_generate_args),*)
                    }
                }

                impl #impl_into_generics Into<#crate_path::TemplateFn<'__self>> for Props #ty_generics
                #into_where_clause
                {
                    fn into(self) -> #crate_path::TemplateFn<'__self> {
                        #crate_path::TemplateFn::new(self.size_hint(), |writer| {
                            #crate_path::Template::render_into(self, writer)
                        })
                    }
                }
            }
            #(#attrs)*
            #vis fn #ident #generics (#(#fn_inputs),*) -> #ident::Props #ty_generics #where_clause {
                #ident::Props {
                    #(#instance_fields,)*
                }
            }
        });
    }
}

fn parse_templates(input: ParseStream) -> syn::Result<Vec<ItemTmpl>> {
    let mut items = vec![];
    while !input.is_empty() {
        items.push(input.parse()?);
    }
    Ok(items)
}

/// Define static templates.
///
/// Each static template is defined in a similar manner to a function definition.
/// The major differences are:
/// * Templates cannot be `const`, `async`, or `unsafe`.
/// * No return type is specified, if called as a function it will return `{name}::Props` struct that
///   implements `Template`.
/// * Implicit lifetimes aren't supported, as a template item's arguments are used to generate the
///   `{name}::Props` struct, and structs don't currently support them.
/// * All of a template item's arguments must be named, you can still destructure using the
///   `field_name @ pat` pattern.
/// * Finally the function body uses a special syntax instead of regular rust code, as is the
///   entire point.
/// Here's an example for how to make a simple template:
/// ```rust
/// templates! {
///     pub fn show<T: fmt::Display>(x: T) {
///         Here is: <span style="color: red">{x}</span>
///     }
///
///     pub fn greet<'a>(
///         name: &'a str,
///         age: u8,
///     ) {
///         <p>
///             "Your name is" {name} "and you are" {age} "years old."
///         </p>
///     }
/// }
///
/// println!("{}", greet("Jake", 40).render()?);
/// // something like: <p>Your name is Jake and you are 40 years old.</p>
/// ```
///
/// # Text
/// Text may be quoted or unquoted. Separate text blocks will have a space automatically inserted
/// between them. Unquoted text follows the [caveats specified here](https://docs.rs/rstml/latest/rstml/node/struct.RawText.html).
///
/// # HTML Element
/// A cannonical HTML element can be defined using its name.
/// Custom names are supported but must be prefixed with `raw:` to avoid confusion with static
/// templates. `raw:` will be strippd from the element's name and the element will be treated as an
/// HTML element.
///
/// HTML elements may have attributes which will be parsed as Rust expressions, though the macro
/// will optimize any literal attributes. Attribute names may also be prefixed with `raw:` and
/// follow the same rules as the element name.
///
/// Any close tag may be substituted for an underscore, `_`, and it will be automatically replaced
/// with the appropriate closing tag. Also some HTML elements such as `<input>` may be self closing
/// this library respects their descision and identity and will treat them as they desire.
///
/// ```rust
/// templates! {
///     pub fn greet<'a>(
///         id: &'a str,
///         name: &'a str,
///         age: u8,
///     ) {
///         <label for=id>"Name: "</label>
///         <input id=id type="string" name="name" value=name>
///     }
/// }
/// ```
///
/// # Substitutions
/// Braces, `{}`, will be interpreted as substitutions. These will write a rust block expression
/// into the file. Substitutions are HTML escaped so it is mostly safe to put user provided strings
/// there.
///
/// To disable escaping use the trust tag (`<trust {...}>`), this tag will display whatever it
/// consumes without escaping it.
///
/// You can also choose not to show a subsitution using the discard tag (`<_ {...}>`). This tag
/// allows you to write mutating operations in a nice manner.
///
/// ```rust
/// templates! {
///     pub fn safe<'a>(name: &'a str) {
///         <p>{name}</p>
///     }
///     pub fn naughty<'a>(name: &'a str) {
///         <trust {format!("<p>{name}</p>")}>
///     }
///
///     pub fn add2(mut n: f32) {
///         {n}
///         "+ 2 ="
///         <_ {n += 2}>
///         {n}
///     }
/// }
/// ```
///
/// # `<script>` & `<style>`
/// Everything inside these elements will be treated as raw text, even quotes. To get around this
/// you can prefix them with `raw:`, though this is not recommended and may lead to XSS if not done
/// carefully.
///
/// ```rust
/// templates! {
///     pub fn js() {
///         <script>
///             const names = ["Jeff", "Jake", "James", "Jonathan"];
///             for (const name of names) {
///                 console.log(name)
///             }
///         </script>
///     }
/// }
/// ```
///
/// # Control flow
/// There are multiple control-flow operations.
///
/// ## Let-Var
/// Let-var elements allow you to define a variable local to the current element.
/// `<let var(pattern: OptionalType) {value}>`
///
/// ```rust
/// templates! {
///     pub fn sum_up(n: u32, m: u32) {
///         <let var(sum) {n + m}>
///         {n} "and" {m} "sum up to" {sum}
///     }
/// }
/// ```
///
/// # If-Else
/// If-else statements are supported, and so are if-let statements.
/// Else elements are put inside an if element and are self-closed.
///
/// ```rust
/// templates! {
///     pub fn show_number(n: u32) {
///         <if {n == 0}>
///             nought
///         <else if {n == 1}>
///             singleton
///         <else if let(n @ 2..=9) {n}>
///             Single digit {n}
///         <else>
///             BIG {n}
///         </if>
///     }
/// }
/// ```
///
/// # For
/// For loops are present and function like they do in rust.
///
/// ```rust
/// templates! {
///     pub fn greet_everyone<'a>(names: &'a [&'a str]) {
///         <for each(name) in {names}>
///             "Hello" {name}
///         </for>
///     }
/// }
/// ```
///
/// # Match
/// To provide feature parity with Askama, it was decided to include a `match` element as well.
/// This element contains different cases for what to match against, these on-case elements are
/// self-closed and segment the `match` body similarly to if-else elements, `<on case(pattern)>`.
/// They also support guards `<on case(pattern) if {condition}>`.
///
/// ```rust
/// templates! {
///     pub fn greet<'a>(name_result: Result<&'a str, &'a str>) {
///         <match {name_result}>
///         <on case(Ok("President John"))>
///             "Oh, hello John, how are you?"
///         <on case(Ok(name))>
///             "Hello" {name}
///         <on case(Err(reason)) if {reason == "laziness"}>
///             "YOU ARE FIRED!"
///         <on case(Err("sleepiness"))>
///             "WAKE UP!"
///         <on case(Err(reason))>
///             "Getting name failed because" {reason}
///         </match>
///     }
/// }
/// ```
///
/// # Properties
/// You can tag properties to alter them during static instantiation of the template.
/// There are multiple different tags you can use:
/// * `#[prop(into)]` This will call `Into::into()` on any value passed into the property during
///    static instantiation.
/// * `#[prop(default)]` If the user does not specify this property when they statically
///   instantiate the template, it will be set to its default value.
/// * `#[prop(optional)]` Like `default`, but if the user does specify this property it will be set
///   to Some(value).
/// * `#[prop(default = value_expr)]` Like `default`, except instead of `Default::default()`
///   `value_expr` will be assigned.
/// * `#[prop(toggle)]` If not present it will be set to `false`. To turn it on, this property must
///   be present but not assign a value, then it will be set to `true`.
///
/// # Template Instantiation
/// There are two types of instantiation: static and dynamic.
///
/// ## Dynamic Instantiation
/// Dynamic instantiation is extremely simple. This just calls `Template::render_into()` on its
/// argument. You perform it by writing an element whose name is a block, `<{template} />`.
/// This element must self close, with `/>`, and therefore it cannot take children.
/// It also doesn't accept any attributes.
///
/// ```rust
/// templates! {
///     pub fn box<'a>(
///         #[prop(into, default)]
///         children: TemplateFn<'a>,
///     ) {
///         <div style="border: 1px solid black">
///             <{children} />
///         </div>
///     }
/// }
/// ```
///
/// ## Static Instantiation
/// Static instantiation is instantiation of templates defined using `templates! {}`. You do this by
/// defining an element whose name is the path to the template. If the path collides with an HTML
/// element you may prefix it with `self::` to disambiguate it. You can assign to properties using
/// attributes. You can toggle a property by putting an attribute without assigning it a value.
///
/// You can create and assign a dynamic template to a property by either putting a prop element
/// directly inside the instantiation (`<prop property_name>children</prop>`),
/// or by just putting children inside the instantiation and therefore implicitly assigning them to
/// the `children` property. Because `children` can be empty it's recommended to mark it as
/// `#[prop(into, default)]` to allow callers to not put any children inside.
///
/// ```rust
/// templates! {
///     pub fn greet<'a>(
///         name: &'a str,
///         #[prop(toggle)]
///         politely: bool,
///     ) {
///         <p>
///             <if {politely}>
///                 "Thank you for gracing us with your presense"
///                 {name}
///             <else>
///                 "Hi" {name}
///             </if>
///         </p>
///     }
///
///     pub fn greet_john() {
///         <greet politely name="Mr. John" />
///     }
///
///     pub fn base<'a>(
///         #[prop(into, default)]
///         head: TemplateFn<'a>,
///         #[prop(into, default)]
///         children: TemplateFn<'a>,
///     ) {
///         <!DOCTYPE html>
///         <html>
///             <head>
///                 <{head} />
///             </head>
///             <body>
///                 <{children} />
///             <body/>
///         </html>
///     }
///
///     pub fn home_page() {
///         <self::base>
///             <prop head>
///                 <title>"Awesome"</title>
///             </prop>
///
///             <greet politely name="Mr. John" />
///         </self::base>
///     }
/// }
/// ```
///
/// [`TemplateFn`]: struct.TemplateFn.html
#[proc_macro]
pub fn templates(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let items = parse_macro_input!(input with parse_templates);

    quote! { #(#items)* }.into()
}

/// Define dynamic templates, basically the content of a template funcation inside [`templates`]
/// This returns [`TemplateFn`]. To see the syntax and usage information check [`templates`].
///
/// ```rust
/// let name = "world";
/// let html = tmpl! {
///     <p>Hello, {name}!</p>
/// }.render()?;
/// println!("{html}");
/// ```
///
/// [`templates`]: macro.templates.html
/// [`TemplateFn`]: struct.TemplateFn.html
#[proc_macro]
pub fn tmpl(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let body = parse_macro_input!(input as NodeBody);

    let crate_path = crate_path();

    let mut buf = String::new();

    let mut size = 0;
    let mut block_tokens = TokenStream::new();
    for child in &body.0 {
        TmplBodyNode::new(&child, &mut size, &mut buf).generate(&mut block_tokens);
    }
    size += buf.len();
    flush_buffer(&mut block_tokens, &mut buf);

    quote! {
        #crate_path::TemplateFn::new(#size, |__writer| {
            #block_tokens
            ::core::fmt::Result::Ok(())
        })
    }
    .into()
}
