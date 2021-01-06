extern crate static_assertions as sa;

use std::env;
use std::fs;
use std::path::Path;
use std::process;

use ::lrpar::Span;
use latte_l as lexer;
use latte_y as ast;

mod compiler;
mod type_checker;

lrlex::lrlex_mod!("latte.l");
lrpar::lrpar_mod!("latte.y");

fn main() {
    let args: Vec<String> = env::args().collect();
    let source = match args.len() {
        2 => {
            let path = Path::new(&args[1]);
            match fs::read_to_string(path) {
                Ok(content) => content,
                Err(_) => {
                    eprintln!("ERROR");
                    eprintln!("failed to read input file {:?}", path);
                    process::exit(5);
                }
            }
        }
        _ => {
            println!("usage: {} FILEPATH", &args[0]);
            process::exit(1);
        }
    };

    let lexerdef = lexer::lexerdef();
    let lexer = lexerdef.lexer(&source);
    let (res, errs) = ast::parse(&lexer);
    if errs.len() != 0 {
        eprintln!("ERROR");
        for e in errs {
            eprintln!("{}", e.pp(&lexer, &ast::token_epp));
        }
        process::exit(2);
    }
    let parsed = match res.unwrap() {
        Ok(parsed) => parsed,
        Err(()) => {
            eprintln!("ERROR");
            eprintln!("unexpected error during parsing");
            eprintln!("most probably the program encountered an integer that is too big");
            process::exit(3);
        }
    };

    match type_checker::check_types(&parsed, &lexer) {
        Ok(_) => (),
        Err(msg) => {
            eprintln!("ERROR");
            eprintln!("{}", msg);
            process::exit(4);
        }
    }

    compiler::compile(&parsed);
    eprintln!("OK");
    process::exit(0);
}
