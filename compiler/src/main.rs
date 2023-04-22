use bakugo::parser::{BakugoParser, Rule};
use pest::Parser;

fn main() {
    let _ = BakugoParser::parse(Rule::And, "&");
}

