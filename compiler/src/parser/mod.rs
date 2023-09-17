use miette::NamedSource;
use pest::{
    error::Error,
    iterators::{Pair, Pairs},
    Parser,
};

use crate::ast::{BakugoParsingErrorDisplay, FnDecl, Node, SourceFile, TopLevelDecl};

#[derive(pest_derive::Parser)]
#[grammar = "bakugo.pest"]
pub struct BakugoParser;

pub fn parse_string(s: &str) -> Result<Pairs<'_, Rule>, Box<Error<Rule>>> {
    match BakugoParser::parse(Rule::SourceFile, s) {
        Ok(ast) => Ok(ast),
        Err(err) => Err(Box::new(err)),
    }
}

pub fn construct_ast(
    source: NamedSource,
    pair: Pair<'_, Rule>,
) -> Result<SourceFile<'_>, BakugoParsingErrorDisplay> {
    match pair.as_rule() {
        Rule::SourceFile => {
            let pairs = pair.into_inner();
            let mut top_level = vec![];
            for pair in pairs {
                // TODO: find a better way to do this
                // see: https://github.com/pest-parser/pest/issues/327
                if matches!(pair.as_rule(), Rule::Semicolon | Rule::EOI) {
                    continue;
                }
                let inner = pair.into_inner().next().unwrap();
                match inner.as_rule() {
                    Rule::FunctionDecl => match FnDecl::parse(inner) {
                        Ok(fn_decl) => top_level.push(TopLevelDecl::FnDecl(fn_decl)),
                        Err(bakugo_err) => {
                            return Err(BakugoParsingErrorDisplay::from_error(bakugo_err, source))
                        }
                    },
                    _ => todo!("non function decls"),
                }
            }
            Ok(SourceFile { top_level })
        }
        _ => unreachable!("Expected a top level decl"),
    }
}
