use std::{env::args, fs, println};

use bakugo::parser::{construct_ast, parse_string};

fn main() {
    // TODO: REPL?
    let filepath = args().nth(1).expect("give me a file name to run pls");

    // TODO: no panic!
    let unparsed_file =
        fs::read_to_string(&filepath).unwrap_or_else(|_| panic!("cannot read file: {filepath}"));

    let parsed = parse_string(&unparsed_file); // get and unwrap the `file` rule; never fails

    match parsed {
        Ok(parsed) => {
            for package in parsed {
                for item in package.clone().into_inner() {
                    println!("{item:#?}");
                }
                let ast = construct_ast(package);
                println!("{ast:#?}");
            }
        }
        Err(err) => {
            println!("parsing error: {err}")
        }
    }
}
