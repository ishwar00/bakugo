use std::fs;

use bakugo::parser::parse_string;
use insta::{assert_snapshot, assert_yaml_snapshot, glob};

#[test]
fn test_parser() {
    glob!("examples/*.bakugo", |path| {
        let input = fs::read_to_string(path).unwrap();
        let parsed = parse_string(&input);
        match parsed {
            Ok(parsed) => assert_yaml_snapshot!(parsed),
            Err(err) => assert_snapshot!(err.to_string()),
        }
    });
}
