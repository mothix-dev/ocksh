#![feature(let_chains)]

pub mod ast;
pub mod parser;

use std::io::Write;

fn prompt(p: &str, input: &mut String) -> std::io::Result<usize> {
    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();

    write!(stdout, "{p}")?;
    stdout.flush()?;

    stdin.read_line(input)
}

const PS1: &str = "@ ";
const PS2: &str = "> ";
const PS3: &str = "#? ";

fn main() {
    let mut args = std::env::args().peekable();

    // skip program name
    let program_name = args.next().unwrap();

    if args.peek().is_none() {
        // no script was passed, enter interactive mode
        loop {
            let mut input = String::new();
            if prompt(PS1, &mut input).expect("prompt failed") == 0 {
                break;
            }

            loop {
                //input.pop();

                let exec = || -> Result<(), parser::ParseError> {
                    for token in parser::Parser::new(&input) {
                        println!("{:?}", token?);
                    }

                    Ok(())
                };

                if let Err(err) = exec() {
                    match err {
                        parser::ParseError::Unmatched(_) => {
                            if prompt(PS2, &mut input).expect("prompt failed") == 0 {
                                eprintln!("{program_name}: {err}");
                                break;
                            }
                        }
                        _ => {
                            eprintln!("{program_name}: {err}");
                            break;
                        }
                    }
                } else {
                    break;
                }
            }
        }

        println!();
    } else {
        let script_name = args.next().unwrap();
        let arguments: Vec<String> = args.collect();

        let script_contents = std::fs::read_to_string(script_name).expect("failed to read file");

        match ast::parse_ast(parser::Parser::new(&script_contents)) {
            Ok(res) => println!("{res:#?}"),
            Err(err) => eprintln!("{program_name}: {err}"),
        }
    }
}
