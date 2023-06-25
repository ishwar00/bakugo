use pest::{iterators::Pairs, Parser};

#[derive(pest_derive::Parser)]
#[grammar = "bakugo.pest"]
pub struct BakugoParser;

pub fn parse_string(s: &str) -> Result<Pairs<'_, Rule>, pest::error::Error<Rule>> {
    BakugoParser::parse(Rule::Statement, s)
}
