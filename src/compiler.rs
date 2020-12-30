use std::collections::{HashMap, HashSet};

use crate::latte_y as ast;

// r12 - r15: "Preserved across function calls"
static REG_OP_TARGET: &str = "r12";
static REG_OP_ARG: &str = "r13";
static REG_FN: &str = "rax";
static REG_HELPER: &str = "r14";

static MEM_VAR_SIZE: &str = "qword";

static OP_MOV: &str = "mov";

// TODO: zapisanie stringów w .data
// TODO: uwaga na scope w if/while
// TODO: czy wywalanie kompilatora w przypadku błędu typecheckera jest dobre?
//       (niby "ERROR" w pierwszej lini stderr itd)
// TODO: null na końcu stringów?
// TODO: dodatkowe funkcje w runtime (długość stringa) lub jakoś inaczej długość stringa

// wychodzenie ze scope
// (identyfikatory na stosie,
//    długość stosu do tego miejsca)
type VStack = (Vec<ast::Ident>, Vec<usize>);
type Label = String;
type Literal = String;
type Labels = HashSet<Label>;

// TODO: deduplikacja stringów w .rodata
// type StrEnv = HashMap<Literal, Label>;

// (dyrektywy, .rodata, .text)
#[derive(Default)]
struct Output {
    directives: Vec<String>,
    rodata: Vec<String>,
    text: Vec<String>,
}

fn error(msg: &str) -> ! {
    eprintln!("Error");
    eprintln!("type checker didn't catch error: {}", msg);
    std::process::exit(42)
}

fn vstack_get_offset(vstack: &VStack, ident: &ast::Ident) -> usize {
    match vstack.0.iter().position(|i| i == ident) {
        None => error("use of undeclared variable"),
        Some(offset) => offset,
    }
}

fn vstack_insert(vstack: &mut VStack, ident: ast::Ident) {
    vstack.0.push(ident);
}

fn vstack_enter_scope(vstack: &mut VStack) {
    vstack.1.push(vstack.0.len());
}

fn vstack_exit_scope(vstack: &mut VStack) {
    let h_before = vstack.0.len();
    let h_after = match vstack.1.pop() {
        None => error("too many scope exits"),
        Some(h) => h,
    };

    vstack.0.truncate(h_after);
    // TODO: czy to jest ok?
    println!("inc rsp {}", h_before - h_after);
}

fn directives(fdefs: &Vec<ast::Node<ast::FunDef>>, output: &mut Output) {
    output.directives.push(".intel_syntax".to_string());

    for fdef in fdefs {
        let fn_name = &fdef.data().1.data();
        println!(".global {}", fn_name);
    }
}

pub fn compile(fdefs: &Vec<ast::Node<ast::FunDef>>) {
    let mut output = Output::default();
    let mut labels = HashSet::new();

    directives(&fdefs, &mut output);

    // TODO: skasować type hint
    let mut vstack: VStack = (Vec::new(), vec![0]);

    for fdef in fdefs {
        compile_fn(fdef, &mut vstack, &mut labels, &mut output);
    }

    for directive in output.directives {
        println!("{}", directive);
    }

    println!();
    println!(".rodata");
    for rodata in output.rodata {
        println!("    {}", rodata);
    }

    println!();
    for instruction in output.text {
        println!("    {}", instruction);
    }
}

fn compile_fn(
    fdef: &ast::Node<ast::FunDef>,
    vstack: &VStack,
    labels: &mut HashSet<Label>,
    output: &mut Output,
) {
    // println!("
    //     main:
    //       ret
    // ");

    unimplemented!();
}

fn compile_expr(
    expr: &ast::Node<ast::Expr>,
    vstack: &VStack,
    labels: &mut HashSet<Label>,
    output: &mut Output,
) {
    match expr.data() {
        ast::Expr::Var(ident_node) => {
            let offset = vstack_get_offset(vstack, ident_node.data());
            output.text.push(format!(
                "{} {} {}",
                OP_MOV,
                REG_OP_TARGET,
                format!("{}[{}]", MEM_VAR_SIZE, offset)
            ));
        }
        ast::Expr::Int(n) => {
            output
                .text
                .push(format!("{} {} {}", OP_MOV, REG_OP_TARGET, n.to_string()))
        }
        ast::Expr::Bool(b) => output.text.push(format!(
            "{} {} {}",
            OP_MOV,
            REG_OP_TARGET,
            if *b { 1 } else { 0 }
        )),
        ast::Expr::Str(s) => {
            let label = format!("str_{}", labels.len() + 1);
            labels.insert(label.clone());
            output.rodata.push(format!("{}: db `{}`, 0", label, s,));
            // TODO?: .len:  equ   $ - label
            output
                .text
                .push(format!("{} {} {}", OP_MOV, REG_OP_TARGET, label));
        }
        ast::Expr::Var(ident_node) => {
            let offset = vstack_get_offset(vstack, ident_node.data());
            // output.text.push(format!("{} {} {}", ));
        }
        _ => unimplemented!(),
    }
}
