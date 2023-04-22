use bakugo::parser::*;
use insta::{assert_snapshot, assert_yaml_snapshot, glob};
use pest::Parser;

#[test]
fn test_parser() {
    glob!("examples/*.bakugo", |path| {});
}

#[test]
fn test_identifier() {
    let ident_tests = vec![
        "a",
        "_x9",
        "ThisIsVariable",
        "αβ",
        "_",
        // invalid
        "9_nope",
        "(_)43",
        " space",
    ];

    for ident in ident_tests {
        let parse_result = BakugoParser::parse(Rule::Ident, ident);
        match parse_result {
            Ok(parsed) => assert_yaml_snapshot!(parsed.as_str()),
            Err(err) => assert_snapshot!(err.to_string()),
        }
    }
}

#[test]
fn test_decimal_lit() {
    let decimal_tests = vec![
        "433",
        "3_4_3",
        "0",
        "170141183460469231731687303715884105727",
        "170_141183_460469_231731_687303_715884_105727",
        // invalid
        "_43",
        "i32",
        " space",
        "(333)",
    ];

    for decimal_test in decimal_tests {
        let parse_result = BakugoParser::parse(Rule::IntLit, decimal_test);
        match parse_result {
            Ok(parsed) => assert_yaml_snapshot!(parsed.as_str(), decimal_test),
            Err(err) => assert_snapshot!(err.to_string()),
        }
    }
}

#[test]
fn test_rune_lit() {
    let runes = r#"
        'a'
        'ä'
        '本'
        '\t'
        '\000'
        '\007'
        '\377'
        '\x07'
        '\xff'
        '\u12e4'
        '\U00101234'
    "#;

    let rune_tests: Vec<&str> = runes
        .lines()
        .map(|line| line.trim())
        .filter(|line| line.len() > 0)
        .collect();

    for rune_test in rune_tests {
        let parse_result = BakugoParser::parse(Rule::RuneLit, rune_test);
        match parse_result {
            Ok(parsed) => assert_yaml_snapshot!(parsed.as_str(), rune_test),
            Err(err) => assert_snapshot!(err.to_string()),
        }
    }
}
