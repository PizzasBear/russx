use std::fmt;

use proc_macro2::{Ident, Punct, TokenStream};
use quote::ToTokens;
use rstml::{
    atoms::{CloseTagStart, FragmentClose, FragmentOpen, OpenTagEnd},
    node::{NodeBlock, NodeComment, NodeDoctype, NodeNameFragment, NodeText, RawText},
};
use syn::{
    ext::IdentExt,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token, Token,
};

pub mod kw {
    // prop
    syn::custom_keyword!(prop);
    syn::custom_keyword!(into);
    syn::custom_keyword!(optional);
    syn::custom_keyword!(toggle);
    // syn::custom_keyword!(children_vec);

    // nodes
    syn::custom_keyword!(var);
    syn::custom_keyword!(each);
    syn::custom_keyword!(raw);
    syn::custom_keyword!(trust);
    syn::custom_keyword!(on);
    syn::custom_keyword!(case);
}

#[derive(Debug, Clone)]
pub struct NodeFragment {
    pub open_tag: FragmentOpen,
    pub children: Vec<Node>,
    pub close_tag: Option<FragmentClose>,
}

#[derive(Debug, Clone)]
pub struct NodeAttributeValue {
    pub eq_token: Token![=],
    pub value: syn::Expr,
}

#[derive(Debug, Clone)]
pub enum HtmlNodeAttribute {
    Block(NodeBlock),
    Keyed(KeyedHtmlNodeAttribute),
}

#[derive(Debug, Clone)]
pub struct KeyedHtmlNodeAttribute {
    pub key: HtmlNodeName,
    pub value: Option<NodeAttributeValue>,
}

#[derive(Debug, Clone)]
pub struct RawHtmlNodeName {
    pub raw_token: kw::raw,
    pub colon_token: Token![:],
    pub punctuated: Punctuated<NodeNameFragment, Punct>,
}

#[derive(Debug, Clone)]
pub enum HtmlNodeName {
    Raw(RawHtmlNodeName),
    Punctuated(Punctuated<NodeNameFragment, Punct>),
    Ident(Ident),
}

impl fmt::Display for HtmlNodeName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident(ident) => fmt::Display::fmt(ident, f),
            Self::Raw(RawHtmlNodeName { punctuated, .. }) | Self::Punctuated(punctuated) => {
                for pair in punctuated.pairs() {
                    fmt::Display::fmt(pair.value(), f)?;
                    if let Some(&punct) = pair.punct() {
                        fmt::Display::fmt(punct, f)?;
                    }
                }

                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct HtmlOpenTag {
    pub start: Token![<],
    pub name: HtmlNodeName,
    pub attributes: Vec<HtmlNodeAttribute>,
    pub end: OpenTagEnd,
}

#[derive(Debug, Clone)]
pub struct HtmlCloseTag {
    pub start: CloseTagStart,
    pub name: HtmlNodeName,
    pub end: Token![>],
}

#[derive(Debug, Clone)]
pub struct HtmlNodeElement {
    pub open_tag: HtmlOpenTag,
    pub children: Vec<Node>,
    pub close_tag: Option<HtmlCloseTag>,
}

#[derive(Debug, Clone)]
pub struct StaticTmplNodeAttribute {
    pub key: Ident,
    pub value: Option<NodeAttributeValue>,
}

#[derive(Debug, Clone)]
pub struct StaticTmplOpenTag {
    pub start: Token![<],
    pub name: syn::ExprPath,
    pub attributes: Vec<StaticTmplNodeAttribute>,
    pub end: OpenTagEnd,
}

#[derive(Debug, Clone)]
pub struct StaticTmplCloseTag {
    pub start: CloseTagStart,
    pub name: syn::ExprPath,
    pub end: Token![>],
}

#[derive(Debug, Clone)]
pub struct PropOpenTag {
    pub start: Token![<],
    pub prop_token: kw::prop,
    pub name: Ident,
    pub end: OpenTagEnd,
}

#[derive(Debug, Clone)]
pub struct PropNodeElement {
    pub open_tag: PropOpenTag,
    pub children: Vec<Node>,
    pub close_tag: Option<KwCloseTag>,
}

#[derive(Debug, Clone)]
pub struct StaticTmplNodeElement {
    pub open_tag: StaticTmplOpenTag,
    pub prop_children: Vec<PropNodeElement>,
    pub children: Vec<Node>,
    pub close_tag: Option<StaticTmplCloseTag>,
}

#[derive(Debug, Clone)]
pub struct DynTmplNodeElement {
    pub start: Token![<],
    pub name_block: syn::Block,
    pub end1: Token![/],
    pub end2: Token![>],
}

#[derive(Debug, Clone)]
pub struct KwCloseTag {
    pub start: CloseTagStart,
    pub name: Ident,
    pub end: Token![>],
}

impl KwCloseTag {
    pub fn from_rstml(tag: rstml::atoms::CloseTag) -> syn::Result<Self> {
        struct AnyIdent(Ident);
        impl Parse for AnyIdent {
            fn parse(input: ParseStream) -> syn::Result<Self> {
                Ok(Self(Ident::parse_any(input)?))
            }
        }
        Ok(Self {
            start: tag.start_tag,
            name: syn::parse2::<AnyIdent>(tag.name.to_token_stream())?.0,
            end: tag.token_gt,
        })
    }
}

#[derive(Debug, Clone)]
pub struct IfLetBinding {
    pub let_token: Token![let],
    pub binding_paren: token::Paren,
    pub binding: syn::Pat,
}

#[derive(Debug, Clone)]
pub struct IfOpenTag {
    pub start: Token![<],
    pub if_token: Token![if],
    pub let_binding: Option<IfLetBinding>,
    pub value: NodeBlock,
    pub end: OpenTagEnd,
}

#[derive(Debug, Clone)]
pub struct ElseIfTag {
    pub start: Token![<],
    pub else_token: Token![else],
    pub if_token: Token![if],
    pub let_binding: Option<IfLetBinding>,
    pub value: NodeBlock,
    pub end: OpenTagEnd,
}

#[derive(Debug, Clone)]
pub struct ElseTag {
    pub start: Token![<],
    pub else_token: Token![else],
    pub end: OpenTagEnd,
}

#[derive(Debug, Clone)]
pub struct IfNodeElement {
    pub open_tag: IfOpenTag,
    pub if_section: Vec<Node>,
    pub else_if_sections: Vec<(ElseIfTag, Vec<Node>)>,
    pub else_section: Option<(ElseTag, Vec<Node>)>,
    pub close_tag: Option<KwCloseTag>,
}

#[derive(Debug, Clone)]
pub struct LetNodeElement {
    pub start: Token![<],
    pub let_token: Token![let],
    pub var_token: kw::var,
    pub binding_paren: token::Paren,
    pub binding: syn::Pat,
    pub value: NodeBlock,
    pub end: OpenTagEnd,
}

#[derive(Debug, Clone)]
pub struct TrustNodeElement {
    pub start: Token![<],
    pub trust_token: kw::trust,
    pub value: NodeBlock,
    pub end: OpenTagEnd,
}

#[derive(Debug, Clone)]
pub struct ForOpenTag {
    pub start: Token![<],
    pub for_token: Token![for],
    pub each_token: kw::each,
    pub binding_paren: token::Paren,
    pub binding: syn::Pat,
    pub in_token: Token![in],
    pub iter: NodeBlock,
    pub end: OpenTagEnd,
}

#[derive(Debug, Clone)]
pub struct ForNodeElement {
    pub open_tag: ForOpenTag,
    pub children: Vec<Node>,
    pub close_tag: Option<KwCloseTag>,
}

#[derive(Debug, Clone)]
pub struct MatchOpenTag {
    pub start: Token![<],
    pub match_token: Token![match],
    pub value: NodeBlock,
    pub end: OpenTagEnd,
}

#[derive(Debug, Clone)]
pub struct MatchArmTag {
    pub start: Token![<],
    pub on_token: kw::on,
    pub case_token: kw::case,
    pub binding_paren: token::Paren,
    pub binding: syn::Pat,
    pub guard: Option<(Token![if], NodeBlock)>,
    pub end: OpenTagEnd,
}

#[derive(Debug, Clone)]
pub struct MatchNodeElement {
    pub open_tag: MatchOpenTag,
    pub arms: Vec<(MatchArmTag, Vec<Node>)>,
    pub close_tag: Option<KwCloseTag>,
}

#[derive(Debug, Clone)]
pub struct DiscardNodeElement {
    pub start: Token![<],
    pub discard_token: Token![_],
    pub value: NodeBlock,
    pub end: OpenTagEnd,
}

#[derive(Debug, Clone)]
pub enum Node {
    Comment(NodeComment),
    Doctype(NodeDoctype),
    Fragment(NodeFragment),
    LetElement(LetNodeElement),
    TrustElement(TrustNodeElement),
    DiscardElement(DiscardNodeElement),
    IfElement(IfNodeElement),
    ForElement(ForNodeElement),
    MatchElement(MatchNodeElement),
    HtmlElement(HtmlNodeElement),
    StaticTmplElement(StaticTmplNodeElement),
    DynTmplElement(DynTmplNodeElement),
    Block(NodeBlock),
    Text(NodeText),
    RawText(RawText),
}

impl ToTokens for NodeFragment {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            open_tag,
            children,
            close_tag,
        } = self;
        open_tag.to_tokens(tokens);
        for child in children {
            child.to_tokens(tokens);
        }
        close_tag.to_tokens(tokens);
    }
}

impl ToTokens for LetNodeElement {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            start,
            let_token,
            var_token,
            binding_paren,
            binding,
            value,
            end,
        } = self;

        start.to_tokens(tokens);
        let_token.to_tokens(tokens);
        var_token.to_tokens(tokens);
        binding_paren.surround(tokens, |tokens| binding.to_tokens(tokens));
        value.to_tokens(tokens);
        end.to_tokens(tokens);
    }
}

impl ToTokens for TrustNodeElement {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            start,
            trust_token,
            value,
            end,
        } = self;

        start.to_tokens(tokens);
        trust_token.to_tokens(tokens);
        value.to_tokens(tokens);
        end.to_tokens(tokens);
    }
}

impl ToTokens for DiscardNodeElement {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            start,
            discard_token,
            value,
            end,
        } = self;

        start.to_tokens(tokens);
        discard_token.to_tokens(tokens);
        value.to_tokens(tokens);
        end.to_tokens(tokens);
    }
}

impl ToTokens for KwCloseTag {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { start, name, end } = self;
        start.to_tokens(tokens);
        name.to_tokens(tokens);
        end.to_tokens(tokens);
    }
}

impl ToTokens for IfLetBinding {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            let_token,
            binding_paren,
            binding,
        } = self;

        let_token.to_tokens(tokens);
        binding_paren.surround(tokens, |tokens| binding.to_tokens(tokens));
    }
}

impl ToTokens for IfOpenTag {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            start,
            if_token,
            let_binding,
            value,
            end,
        } = self;

        start.to_tokens(tokens);
        if_token.to_tokens(tokens);
        let_binding.to_tokens(tokens);
        value.to_tokens(tokens);
        end.to_tokens(tokens);
    }
}

impl ToTokens for ElseIfTag {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            start,
            else_token,
            if_token,
            let_binding,
            value,
            end,
        } = self;

        start.to_tokens(tokens);
        else_token.to_tokens(tokens);
        if_token.to_tokens(tokens);
        let_binding.to_tokens(tokens);
        value.to_tokens(tokens);
        end.to_tokens(tokens);
    }
}

impl ToTokens for ElseTag {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            start,
            else_token,
            end,
        } = self;

        start.to_tokens(tokens);
        else_token.to_tokens(tokens);
        end.to_tokens(tokens);
    }
}

impl ToTokens for IfNodeElement {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            open_tag,
            if_section,
            else_if_sections,
            else_section,
            close_tag,
        } = self;

        open_tag.to_tokens(tokens);
        for node in if_section {
            node.to_tokens(tokens);
        }
        for (tag, nodes) in else_if_sections {
            tag.to_tokens(tokens);
            for node in nodes {
                node.to_tokens(tokens);
            }
        }
        if let Some((tag, nodes)) = else_section {
            tag.to_tokens(tokens);
            for node in nodes {
                node.to_tokens(tokens);
            }
        }
        close_tag.to_tokens(tokens);
    }
}

impl ToTokens for ForOpenTag {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            start,
            for_token,
            each_token,
            binding_paren,
            binding,
            in_token,
            iter,
            end,
        } = self;

        start.to_tokens(tokens);
        for_token.to_tokens(tokens);
        each_token.to_tokens(tokens);
        binding_paren.surround(tokens, |tokens| binding.to_tokens(tokens));
        in_token.to_tokens(tokens);
        iter.to_tokens(tokens);
        end.to_tokens(tokens);
    }
}

impl ToTokens for ForNodeElement {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            open_tag,
            children,
            close_tag,
        } = self;

        open_tag.to_tokens(tokens);
        for child in children {
            child.to_tokens(tokens);
        }
        close_tag.to_tokens(tokens);
    }
}

impl ToTokens for MatchOpenTag {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            start,
            match_token,
            value,
            end,
        } = self;

        start.to_tokens(tokens);
        match_token.to_tokens(tokens);
        value.to_tokens(tokens);
        end.to_tokens(tokens);
    }
}

impl ToTokens for MatchArmTag {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            start,
            on_token,
            case_token,
            binding_paren,
            binding,
            guard,
            end,
        } = self;

        start.to_tokens(tokens);
        on_token.to_tokens(tokens);
        case_token.to_tokens(tokens);
        binding_paren.surround(tokens, |tokens| binding.to_tokens(tokens));
        if let Some((if_token, value)) = guard {
            if_token.to_tokens(tokens);
            value.to_tokens(tokens);
        }
        end.to_tokens(tokens);
    }
}

impl ToTokens for MatchNodeElement {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            open_tag,
            arms,
            close_tag,
        } = self;

        open_tag.to_tokens(tokens);
        for (tag, children) in arms {
            tag.to_tokens(tokens);
            for child in children {
                child.to_tokens(tokens)
            }
        }
        close_tag.to_tokens(tokens);
    }
}

impl ToTokens for RawHtmlNodeName {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            raw_token,
            colon_token,
            punctuated,
        } = self;

        raw_token.to_tokens(tokens);
        colon_token.to_tokens(tokens);
        punctuated.to_tokens(tokens);
    }
}

impl ToTokens for HtmlNodeName {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Raw(raw) => raw.to_tokens(tokens),
            Self::Ident(ident) => ident.to_tokens(tokens),
            Self::Punctuated(puncts) => puncts.to_tokens(tokens),
        }
    }
}

impl ToTokens for NodeAttributeValue {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { eq_token, value } = self;

        eq_token.to_tokens(tokens);
        value.to_tokens(tokens);
    }
}

impl ToTokens for KeyedHtmlNodeAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { key, value } = self;

        key.to_tokens(tokens);
        value.to_tokens(tokens);
    }
}

impl ToTokens for HtmlNodeAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Block(block) => block.to_tokens(tokens),
            Self::Keyed(keyed) => keyed.to_tokens(tokens),
        }
    }
}

impl ToTokens for HtmlOpenTag {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            start,
            name,
            attributes,
            end,
        } = self;

        start.to_tokens(tokens);
        name.to_tokens(tokens);
        for attr in attributes {
            attr.to_tokens(tokens);
        }
        end.to_tokens(tokens);
    }
}

impl ToTokens for HtmlCloseTag {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { start, name, end } = self;

        start.to_tokens(tokens);
        name.to_tokens(tokens);
        end.to_tokens(tokens);
    }
}

impl ToTokens for HtmlNodeElement {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            open_tag,
            children,
            close_tag,
        } = self;

        open_tag.to_tokens(tokens);
        for child in children {
            child.to_tokens(tokens);
        }
        close_tag.to_tokens(tokens);
    }
}

impl ToTokens for StaticTmplNodeAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { key, value } = self;

        key.to_tokens(tokens);
        value.to_tokens(tokens);
    }
}

impl ToTokens for StaticTmplOpenTag {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            start,
            name,
            attributes,
            end,
        } = self;
        start.to_tokens(tokens);
        name.to_tokens(tokens);
        for attr in attributes {
            attr.to_tokens(tokens);
        }
        end.to_tokens(tokens);
    }
}

impl ToTokens for PropOpenTag {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            start,
            prop_token,
            name,
            end,
        } = self;

        start.to_tokens(tokens);
        prop_token.to_tokens(tokens);
        name.to_tokens(tokens);
        end.to_tokens(tokens);
    }
}

impl ToTokens for PropNodeElement {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            open_tag,
            children,
            close_tag,
        } = self;

        open_tag.to_tokens(tokens);
        for child in children {
            child.to_tokens(tokens);
        }
        close_tag.to_tokens(tokens);
    }
}

impl ToTokens for StaticTmplCloseTag {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { start, name, end } = self;

        start.to_tokens(tokens);
        name.to_tokens(tokens);
        end.to_tokens(tokens);
    }
}

impl ToTokens for StaticTmplNodeElement {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            open_tag,
            prop_children,
            children,
            close_tag,
        } = self;

        open_tag.to_tokens(tokens);
        for prop in prop_children {
            prop.to_tokens(tokens);
        }
        for child in children {
            child.to_tokens(tokens);
        }
        close_tag.to_tokens(tokens);
    }
}

impl ToTokens for DynTmplNodeElement {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            start,
            name_block,
            end1,
            end2,
        } = self;

        start.to_tokens(tokens);
        name_block.to_tokens(tokens);
        end1.to_tokens(tokens);
        end2.to_tokens(tokens);
    }
}

impl ToTokens for Node {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Comment(node) => node.to_tokens(tokens),
            Self::Doctype(node) => node.to_tokens(tokens),
            Self::Fragment(node) => node.to_tokens(tokens),
            Self::LetElement(node) => node.to_tokens(tokens),
            Self::TrustElement(node) => node.to_tokens(tokens),
            Self::DiscardElement(node) => node.to_tokens(tokens),
            Self::IfElement(node) => node.to_tokens(tokens),
            Self::ForElement(node) => node.to_tokens(tokens),
            Self::MatchElement(node) => node.to_tokens(tokens),
            Self::HtmlElement(node) => node.to_tokens(tokens),
            Self::StaticTmplElement(node) => node.to_tokens(tokens),
            Self::DynTmplElement(node) => node.to_tokens(tokens),
            Self::Block(node) => node.to_tokens(tokens),
            Self::Text(node) => node.to_tokens(tokens),
            Self::RawText(node) => node.to_tokens(tokens),
        }
    }
}
