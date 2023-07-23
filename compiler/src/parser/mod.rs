use pest::{
    error::Error,
    iterators::{Pair, Pairs},
    Parser,
};

use crate::ast::{FnDecl, Node, SourceFile, TopLevelDecl};

#[derive(pest_derive::Parser)]
#[grammar = "bakugo.pest"]
pub struct BakugoParser;

pub fn parse_string(s: &str) -> Result<Pairs<'_, Rule>, Box<Error<Rule>>> {
    match BakugoParser::parse(Rule::SourceFile, s) {
        Ok(ast) => Ok(ast),
        Err(err) => Err(Box::new(err)),
    }
}

pub fn construct_ast(pair: Pair<'_, Rule>) -> SourceFile<'_> {
    match pair.as_rule() {
        Rule::SourceFile => {
            let pairs = pair.into_inner();
            let mut top_level = vec![];
            for pair in pairs {
                let inner = pair.into_inner().next().unwrap();
                match inner.as_rule() {
                    Rule::FunctionDecl => {
                        top_level.push(TopLevelDecl::FnDecl(FnDecl::parse(inner)))
                    }
                    _ => todo!("non function decls"),
                }
            }
            SourceFile { top_level }
        }
        _ => unreachable!("Expected a top level decl"),
    }
}
