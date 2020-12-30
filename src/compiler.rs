use std::collections::{HashMap, HashSet};

use crate::latte_y as ast;

// r12 - r15: "Preserved across function calls"
static REG_OP_TARGET: &str = "r12";
static REG_OP_AUX: &str = "r13";
// TODO: przerobić na maszynę stosową
static REG_STORAGE: &str = "r14";
static REG_TMP: &str = "r15";

static REG_FN: &str = "rax";

static REG_BASE: &str = "rsp";
static REG_STACK: &str = "rbp";

static REG_QUOTIENT: &str = "rax";
static REG_REMAINDER: &str = "rdx";

static REG_DIVIDEND_HIGH: &str = "rdx";
static REG_DIVIDEND_LOW: &str = "rax";

static ARG_REGS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

// static MEM_VAR_SIZE: &str = "qword";

static OP_MOV: &str = "mov";
static OP_XOR: &str = "xor";
static OP_OR: &str = "or";
static OP_AND: &str = "and";
static OP_DEC: &str = "dec";
static OP_INC: &str = "inc";
static OP_IMUL: &str = "imul";
static OP_IDIV: &str = "idiv";
static OP_NEGATION: &str = "neg";
static OP_POP: &str = "pop";
static OP_PUSH: &str = "push";
static OP_PUSH_RFLAGS: &str = "pushfq";
static OP_LOG_SHIFTR: &str = "shr";
// static OP_NUM_SHIFTR: &str = "sar";
static OP_CALL: &str = "call";
static OP_CMP: &str = "cmp";

// bit >= 0
static BIT_ZERO_FLAG: usize = 6;
// static BIT_SIGN_FLAG: u8 = 7

// TODO: zapisanie stringów w .data
// TODO: uwaga na scope w if/while
// TODO: czy wywalanie kompilatora w przypadku błędu typecheckera jest dobre?
//       (niby "ERROR" w pierwszej lini stderr itd)
// TODO: null na końcu stringów?
// TODO: dodatkowe funkcje w runtime (długość stringa) lub jakoś inaczej długość stringa
// TODO: czy mam błąd nie rezerwując miejsca na zmienne lokalne z góry?
// TODO: upewnienie się, że flagi są ustawiane przez dobre instrukcje

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
    match vstack.0.iter().rev().position(|i| i == ident) {
        None => error("use of undeclared variable"),
        Some(n) => n,
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
        ast::Expr::Int(n) => output.text.push(format!("{} {} {}", OP_MOV, REG_OP_TARGET, n.to_string())),
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
            output.text.push(format!("{} {} {}", OP_MOV, REG_OP_TARGET, label));
        }
        ast::Expr::Var(ident_node) => {
            let offset = vstack_get_offset(vstack, ident_node.data());
            output.text.push(format!("{} {} {}", OP_MOV, REG_OP_AUX, REG_STACK));
            output.text.push(format!("{} {} {}", OP_DEC, REG_OP_AUX, offset));
            output.text.push(format!("{} {} [{}]", OP_MOV, REG_OP_TARGET, REG_OP_AUX));
        }
        ast::Expr::Neg(expr_node) => {
            compile_expr(expr_node, vstack, labels, output);
            output.text.push(format!("{} {}", OP_NEGATION, REG_OP_TARGET));
        }
        ast::Expr::Not(expr_node) => {
            compile_expr(expr_node, vstack, labels, output);
            output.text.push(OP_PUSH_RFLAGS.to_string());
            output.text.push(format!("{} {}", OP_POP, REG_OP_TARGET));
            output.text.push(format!("{} {} {}", OP_AND, REG_OP_TARGET, 1 << BIT_ZERO_FLAG));
            output.text.push(format!("{} {} {}", OP_LOG_SHIFTR, REG_OP_TARGET, BIT_ZERO_FLAG - 1));
            output.text.push(format!("{} {} {}", OP_XOR, REG_OP_TARGET, 1)); // 0 => 1, 1 => 0
        }
        ast::Expr::App(fname_node, arg_expr_nodes) => {
            let fname = fname_node.data();
            for i in 0..=5 {
                compile_expr(&arg_expr_nodes[i], vstack, labels, output);
                output.text.push(format!("{} {} {}", OP_MOV, REG_OP_TARGET, ARG_REGS[i]));
            }
            // TODO: obsługa więcej niż 6 argumentów (obecnie OP_PUSH psuje)
            // // TODO: dobra kolejność?
            // // TODO: OP_PUSH mi psuje niezmiennik stosu :(
            // for i in (6..arg_expr_nodes.len()).rev() {
            //     compile_expr(&arg_expr_nodes[i], vstack, labels, output);
            //     output.text.push(format!("{} {}", OP_PUSH, REG_OP_TARGET));
            // }
            // TODO: czy dobry prolog (lub jego brak?)
            output.text.push(format!("{} {}", OP_CALL, fname));
        }
        ast::Expr::Add(expr1, expr2)
        | ast::Expr::Sub(expr1, expr2)
        | ast::Expr::Mul(expr1, expr2) => {
            compile_expr(&expr2, vstack, labels, output);
            output.text.push(format!("{} {} {}", OP_MOV, REG_STORAGE, REG_OP_TARGET));
            compile_expr(&expr1, vstack, labels, output);

            let opcode = match expr.data() {
                ast::Expr::Add(_, _) => OP_INC,
                ast::Expr::Sub(_, _) => OP_DEC,
                ast::Expr::Mul(_, _) => OP_IMUL,
                _ => unreachable!(),
            };
            output.text.push(format!("{} {} {}", opcode, REG_OP_TARGET, REG_STORAGE));
        }
        ast::Expr::Div(expr1, expr2)
        | ast::Expr::Mod(expr1, expr2) => {
            compile_expr(&expr2, vstack, labels, output);
            output.text.push(format!("{} {} {}", OP_MOV, REG_STORAGE, REG_OP_TARGET));
            compile_expr(&expr1, vstack, labels, output);

            output.text.push(format!("{} {} {}", OP_MOV, REG_DIVIDEND_HIGH, 0));
            output.text.push(format!("{} {} {}", OP_MOV, REG_DIVIDEND_LOW, REG_OP_TARGET));
            output.text.push(format!("{} {}", OP_IDIV, REG_STORAGE));

            let result_reg = match expr.data() {
                ast::Expr::Div(_, _) => REG_QUOTIENT,
                ast::Expr::Mod(_, _) => REG_REMAINDER,
                _ => unreachable!(),
            };
            output.text.push(format!("{} {} {}", OP_MOV, REG_OP_TARGET, result_reg));
        }
        ast::Expr::And(expr1, expr2) |
        ast::Expr::Or(expr1, expr2) => {
            // TODO: leniwość
            compile_expr(&expr2, vstack, labels, output);
            output.text.push(format!("{} {} {}", OP_MOV, REG_STORAGE, REG_OP_TARGET));
            compile_expr(&expr1, vstack, labels, output);

            let opcode = match expr.data() {
                ast::Expr::And(_, _) => OP_AND,
                ast::Expr::Or(_, _) => OP_OR,
                _ => unreachable!(),
            };
            output.text.push(format!("{} {} {}", opcode, REG_OP_TARGET, REG_STORAGE));
        }
        ast::Expr::EQ(expr1, expr2) |
        ast::Expr::NEQ(expr1, expr2) => {
            compile_expr(&expr2, vstack, labels, output);
            output.text.push(format!("{} {} {}", OP_MOV, REG_STORAGE, REG_OP_TARGET));
            compile_expr(&expr1, vstack, labels, output);

            output.text.push(format!("{} {} {}", OP_CMP, REG_OP_TARGET, REG_STORAGE));

            output.text.push(OP_PUSH_RFLAGS.to_string());
            output.text.push(format!("{} {}", OP_POP, REG_OP_TARGET));
            output.text.push(format!("{} {} {}", OP_AND, REG_OP_TARGET, 1 << BIT_ZERO_FLAG));
            output.text.push(format!("{} {} {}", OP_LOG_SHIFTR, REG_OP_TARGET, BIT_ZERO_FLAG - 1));

            if let ast::Expr::NEQ(_, _) = expr.data() {
                output.text.push(format!("{} {} {}", OP_XOR, REG_OP_TARGET, 1)); // 0 => 1, 1 => 0
            }
        }
        _ => unimplemented!(),
    }
}
