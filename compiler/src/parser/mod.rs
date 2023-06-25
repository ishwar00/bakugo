use pest::{
    iterators::{Pair, Pairs},
    Parser,
};

use crate::ast::{FnDecl, Node, Statement};

#[derive(pest_derive::Parser)]
#[grammar = "bakugo.pest"]
pub struct BakugoParser;

pub fn parse_string(s: &str) -> Result<Pairs<'_, Rule>, pest::error::Error<Rule>> {
    BakugoParser::parse(Rule::TopLevelDecl, s)
}

pub fn construct_ast(pair: Pair<'_, Rule>) -> Node<'_> {
    match pair.as_rule() {
        Rule::TopLevelDecl => {
            let mut stmts = vec![];
            let inner = pair.into_inner().next().unwrap();
            match inner.as_rule() {
                Rule::FunctionDecl => stmts.push(Statement::FnDecl(FnDecl::parse(inner))),
                _ => todo!("non function decls"),
            }
            Node::Statements(stmts)
        }
        _ => unreachable!("Expected a top level decl"),
    }
}
