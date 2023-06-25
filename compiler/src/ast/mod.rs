use pest::{iterators::Pair, Span};

use crate::parser::Rule;

#[derive(Debug)]
pub enum Node<'i> {
    Statements(Vec<Statement<'i>>),
}

#[derive(Debug)]
pub struct FnDecl<'i> {
    pub name: Ident<'i>,
    pub params: Vec<Parameter<'i>>,
    pub result_kind: Option<Kind<'i>>,
    pub body: Vec<Statement<'i>>,
    pub span: Span<'i>,
}

impl<'i> FnDecl<'i> {
    pub fn parse(pair: Pair<'i, Rule>) -> Self {
        match pair.as_rule() {
            Rule::FunctionDecl => todo!(""),
            _ => unreachable!("FnDecl has been checked")
        }
    }
}

#[derive(Debug)]
pub struct Ident<'i> {
    pub value: String,
    pub span: Span<'i>,
}

#[derive(Debug)]
pub struct Parameter<'i> {
    pub name: Ident<'i>,
    pub kind: Option<Kind<'i>>,
    pub span: Span<'i>,
}

#[derive(Debug)]
pub enum Kind<'i> {
    Simple {
        name: Ident<'i>,
        span: Span<'i>,
    },
    Tuple {
        kinds: Vec<Kind<'i>>,
        span: Span<'i>,
    },
}

#[derive(Debug)]
pub enum Statement<'i> {
    FnDecl(FnDecl<'i>),
}
