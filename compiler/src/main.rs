use std::{env::args, fs};

use bakugo::parser::parse_string;

fn main() {
    // TODO: REPL?
    let filepath = args().nth(1).expect("give me a file name to run pls");

    // TODO: no panic!
    let unparsed_file =
        fs::read_to_string(&filepath).expect(&format!("cannot read file: {filepath}"));

    let parsed = parse_string(&unparsed_file); // get and unwrap the `file` rule; never fails

    match parsed {
        Ok(parsed) => {
            let package = parsed.peek().unwrap(); // never fails?
            for item in package.into_inner() {
                println!("{item:?}");
            }
        }
        Err(err) => {
            println!("parsing error: {err}")
        }
    }
}
