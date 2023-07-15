use pest::{iterators::Pair, Span};

use crate::parser::Rule;

pub trait Node<'i> {
    fn parse(pair: Pair<'i, Rule>) -> Self;
}

#[derive(Debug)]
pub struct SourceFile<'i> {
    pub top_level: Vec<TopLevelDecl<'i>>,
}

#[derive(Debug)]
pub enum TopLevelDecl<'i> {
    FnDecl(FnDecl<'i>),
}

#[derive(Debug)]
pub struct FnDecl<'i> {
    pub name: Ident<'i>,
    pub params: Vec<FnParameterDecl<'i>>,
    pub result_kind: Option<Kind<'i>>,
    pub body: Vec<Statement>,
    pub span: Span<'i>,
}

impl<'i> Node<'i> for FnDecl<'i> {
    fn parse(pair: Pair<'i, Rule>) -> Self {
        if pair.as_rule() != Rule::FunctionDecl {
            unreachable!("FnDecl has been checked");
        }

        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let name = Ident::parse(pairs.next().unwrap());
        let signature = pairs.next().unwrap();
        let body_pair = pairs.next().unwrap();

        let mut sig_children = signature.into_inner();
        let param_pair = sig_children.next().unwrap();
        let result_kind = sig_children.next().map(|p| Kind::parse(p));
        let mut params = vec![];
        if let Some(param_list_pair) = param_pair.into_inner().next() {
            let param_list = ParameterList::parse(param_list_pair);
            let param_decls = param_list.0;
            for param_decl in &param_decls {
                if param_decl.name.is_none() {
                    panic!("cant have only type in fn decl params")
                }
            }
            params = param_decls.into_iter().map(|p| p.into()).collect();
        }

        let body = body_pair
            .into_inner()
            .map(|p| Statement::parse(p))
            .collect();

        Self {
            name,
            params,
            result_kind,
            body,
            span,
        }
    }
}

#[derive(Debug)]
pub struct ParameterList<'i>(pub Vec<ParameterDecl<'i>>);

impl<'i> Node<'i> for ParameterList<'i> {
    fn parse(pair: Pair<'i, Rule>) -> Self {
        if pair.as_rule() != Rule::ParameterList {
            unreachable!("FnDecl has been checked");
        }

        let mut params = vec![];
        for param_decl in pair.into_inner() {
            let mut pairs = param_decl.into_inner();
            let first = pairs.next().unwrap();
            match first.as_rule() {
                // idents with types
                Rule::IdentifierList => {
                    let second = pairs.next().unwrap();

                    for ident in first.into_inner() {
                        let span = ident.as_span();
                        params.push(ParameterDecl {
                            name: Some(Ident::parse(ident)),
                            kind: Kind::parse(second.clone()),
                            span,
                        });
                    }
                }
                // only types
                Rule::Type => {
                    let span = first.as_span();
                    params.push(ParameterDecl {
                        name: None,
                        kind: Kind::parse(first),
                        span,
                    });
                    for kind in pairs {
                        let span = kind.as_span();
                        params.push(ParameterDecl {
                            name: None,
                            kind: Kind::parse(kind),
                            span,
                        });
                    }
                }
                _ => unreachable!("parameter list should not have anything else"),
            }
        }
        Self(params)
    }
}

#[derive(Debug)]
pub struct Ident<'i> {
    pub value: String,
    pub span: Span<'i>,
}

impl<'i> Node<'i> for Ident<'i> {
    fn parse(pair: Pair<'i, Rule>) -> Self {
        if pair.as_rule() != Rule::Ident {
            unreachable!("Ident has been checked");
        }

        Self {
            value: pair.as_str().to_owned(),
            span: pair.as_span(),
        }
    }
}

#[derive(Debug)]
pub struct ParameterDecl<'i> {
    pub name: Option<Ident<'i>>,
    pub kind: Kind<'i>,
    pub span: Span<'i>,
}

#[derive(Debug)]
pub struct FnParameterDecl<'i> {
    pub name: Ident<'i>,
    pub kind: Kind<'i>,
    pub span: Span<'i>,
}

impl<'i> From<ParameterDecl<'i>> for FnParameterDecl<'i> {
    fn from(value: ParameterDecl<'i>) -> Self {
        Self {
            name: value.name.unwrap(),
            kind: value.kind,
            span: value.span,
        }
    }
}

#[derive(Debug)]
pub enum Kind<'i> {
    Simple {
        name: String,
        span: Span<'i>,
    },
    Tuple {
        kinds: Vec<Kind<'i>>,
        span: Span<'i>,
    },
}

impl<'i> Node<'i> for Kind<'i> {
    fn parse(pair: Pair<'i, Rule>) -> Self {
        if pair.as_rule() != Rule::Type {
            unreachable!("Type is already checked");
        }

        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::Ident => Self::Simple {
                name: inner.as_str().to_owned(),
                span: inner.as_span(),
            },
            Rule::Tuple => todo!("Tuple types cannot be parsed yet"),
            _ => unreachable!(""),
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    // TODO: add expression list
    Return,
}

impl<'i> Node<'i> for Statement {
    fn parse(pair: Pair<'i, Rule>) -> Self {
        Self::Return
    }
}
