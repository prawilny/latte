use std::env;
use std::process;
use std::fs;
use std::path::Path;

use latte_l as lexer;
use latte_y as ast;
use ::lrpar::Span;

mod type_checker;

lrlex::lrlex_mod!("latte.l");
lrpar::lrpar_mod!("latte.y");

fn main() {
    let args: Vec<String> = env::args().collect();
    let source = match args.len() {
        2 => {
            let path = Path::new(&args[1]);
            fs::read_to_string(path).unwrap()
        }
        _ => {
            println!("usage: {} FILEPATH", &args[0]);
            process::exit(1);
        },
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
    let parsed = res.unwrap().unwrap();
    match type_checker::check_types(&parsed, &source) {
        Ok(_) => (),
        Err(msg) => {
            eprintln!("ERROR");
            eprintln!("{}", msg);
            process::exit(3);
        }
    }

    eprintln!("OK");
    process::exit(0);
}
