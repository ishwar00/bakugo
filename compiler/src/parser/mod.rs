use miette::NamedSource;
use pest::{
    error::Error,
    iterators::{Pair, Pairs},
    Parser,
};

use crate::ast::{BakugoParsingErrorDisplay, Node, SourceFile};

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
    match SourceFile::parse(pair) {
        Ok(s) => Ok(s),
        Err(bakugo_err) => Err(BakugoParsingErrorDisplay::from_error(bakugo_err, source)),
    }
}
