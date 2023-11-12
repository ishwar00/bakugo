use std::{fmt::Display, fs, path::Path};

use bakugo::{ast::BakugoParsingErrorDisplay, parser::*};
use insta::{
    assert_debug_snapshot, assert_display_snapshot, assert_snapshot, assert_yaml_snapshot, glob,
};
use miette::{GraphicalReportHandler, GraphicalTheme, NamedSource};
use pest::Parser;

struct BakugoParsingErrDebug(BakugoParsingErrorDisplay);

impl Display for BakugoParsingErrDebug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let report_handler = GraphicalReportHandler::new_themed(GraphicalTheme::unicode_nocolor());
        report_handler.render_report(f, &self.0)
    }
}

#[test]
fn test_parser() {
    fn test_parser_from_path(path: &Path) {
        let input = fs::read_to_string(path).unwrap();
        let parsed = parse_string(&input);
        match parsed {
            Ok(parsed) => assert_yaml_snapshot!(parsed),
            Err(err) => assert_snapshot!(err.to_string()),
        }
    }

    glob!("examples/*.bakugo", test_parser_from_path);
    glob!("examples/ast_error/*.bakugo", test_parser_from_path);
    glob!("examples/parser_error/*.bakugo", test_parser_from_path);
}

#[test]
fn test_ast() {
    fn test_ast_from_path(path: &Path) {
        let input = fs::read_to_string(path).unwrap();
        let mut parsed = parse_string(&input).unwrap();
        let source_file = parsed.next().unwrap();
        let source = NamedSource::new(path.file_name().unwrap().to_str().unwrap(), input.clone());
        match construct_ast(source, source_file) {
            Ok(parsed) => assert_debug_snapshot!(parsed),
            Err(err) => assert_display_snapshot!(BakugoParsingErrDebug(err)),
        }
    }

    glob!("examples/*.bakugo", test_ast_from_path);
    glob!("examples/ast_error/*.bakugo", test_ast_from_path);
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
    let runes = r"
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
        '\''

        'aa'
        '\k'
        '\xa'
        '\0'
        '\400'
        '\uDFFF'
        '\U00110000'
    ";
    // NOTE:
    // '\''        // rune literal containing single quote character
    //'aa'         // illegal: too many characters
    //'\k'         // illegal: k is not recognized after a backslash
    //'\xa'        // illegal: too few hexadecimal digits
    //'\0'         // illegal: too few octal digits
    //'\400'       // illegal: octal value over 255
    //'\uDFFF'     // illegal: surrogate half
    //'\U00110000' // illegal: invalid Unicode code point

    let rune_tests: Vec<&str> = runes
        .lines()
        .map(|line| line.trim())
        .filter(|trim_line| !trim_line.is_empty())
        .collect();

    for rune_test in rune_tests {
        let parse_result = BakugoParser::parse(Rule::RuneLit, rune_test);
        match parse_result {
            Ok(parsed) => assert_yaml_snapshot!(parsed.as_str(), rune_test),
            Err(err) => assert_snapshot!(err.to_string()),
        }
    }
}

#[test]
fn test_string_lit() {
    let strings = [
        r#" `abc` "#,
        r#" "\n" "#,
        r#" "\"" "#,
        r#" "Hello, world!\n" "#,
        r#" "日本語" "#,
        r#" `日本語` "#,
        r#" "\u65e5本\U00008a9e" "#,
        r#" "\U000065e5\U0000672c\U00008a9e" "#,
        r#" "\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e" "#,
        r#" "\xff\u00FF" "#,
        r#" "\uD800" "#,
        r#" "\U00110000" "#,
    ];

    let string_tests: Vec<&str> = strings.iter().map(|string| string.trim()).collect();

    for string_test in string_tests {
        let parse_result = BakugoParser::parse(Rule::StringLit, string_test);
        match parse_result {
            Ok(mut parsed) => {
                let string_lit = parsed.next().unwrap();
                if let Rule::RawStringLit | Rule::InterpretedStringLit = string_lit.as_rule() {
                    assert_yaml_snapshot!(string_lit.as_str(), string_test);
                } else {
                    panic!(
                        "testing error: recieved {}, with rule {:?}",
                        string_test,
                        string_lit.as_rule()
                    );
                }
            }
            Err(err) => assert_snapshot!(err.to_string()),
        }
    }
}
