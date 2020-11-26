use std::env;
use std::process;
use std::fs;
use std::path::Path;

use latte_l as lexer;
use latte_y as ast;

lrlex::lrlex_mod!("latte.l");
lrpar::lrpar_mod!("latte.y");

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_content = match args.len() {
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
    let lexer = lexerdef.lexer(&file_content);
    let (res, errs) = ast::parse(&lexer);
    if errs.len() != 0 {
        eprintln!("ERROR");
        for e in errs {
            eprintln!("{}", e.pp(&lexer, &ast::token_epp));
        }
        process::exit(2);
    }
    let parsed = res.unwrap().unwrap();

    eprintln!("OK");
    process::exit(0);
}
