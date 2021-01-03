use std::collections::{HashMap, HashSet};

use lrpar::Span;

use crate::latte_y as ast;

static REG_OP_MAIN: &str = "r11";
static REG_OP_AUX: &str = "r10";
static REG_TEMP: &str = "rax";
static REG_TEMP_BYTE: &str = "al";
// tj: wrapperu na push i pop wpisujące/kasujące z VStack ".stack"

static REG_FN_RETVAL: &str = "rax";

static REG_BASE: &str = "rbp";
static REG_STACK: &str = "rsp";

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
// static OP_PUSH_RFLAGS: &str = "pushfq";
// static OP_LOG_SHIFTR: &str = "shr";
// static OP_NUM_SHIFTR: &str = "sar";
static OP_CALL: &str = "call";
static OP_CMP: &str = "cmp";
static OP_RET: &str = "ret";

static OP_SETCC_EQ: &str = "sete";
static OP_SETCC_NEQ: &str = "setne";
static OP_SETCC_LTH: &str = "setl";
static OP_SETCC_LEQ: &str = "setle";
static OP_SETCC_GTH: &str = "setg";
static OP_SETCC_GEQ: &str = "setge";
// static OP_SETCC_NONZERO: &str = "setnz";

static JMP_EQ: &str = "je";

// bit# \in [0, 1, ... 63]
// static BIT_ZERO_FLAG: usize = 6;
// static BIT_SIGN_FLAG: u8 = 7

// TODO: uwaga na scope w if/while [rozwiązanie: przerobić deklaracje na wyrażenie]
// TODO: czy wywalanie kompilatora w przypadku błędu typecheckera jest dobre?
//       (niby "ERROR" w pierwszej lini stderr itd)
// TODO: null na końcu stringów?
// TODO: dodatkowe funkcje w runtime (długość stringa) lub jakoś inaczej długość stringa
// TODO: czy mam błąd nie rezerwując miejsca na zmienne lokalne z góry?
// TODO: upewnienie się, że flagi są ustawiane przez dobre instrukcje
// TODO: return 0 na końcu
// TODO: zapisywanie nie-volatile rejestrów (te od dzielenia?)

// TODO: zawartość stosu (przy call i ret głównie)

// TODO: rzeczy stosowe (tj. kontatenacj: string + string)

// TODO: sprawdzenie minimalności mutowalności argumentów

// TODO: upewnienie się, że dobrze wskazuję górę stosu

// TODO: uwaga na stringi, bo sizeof(char) != sizeof(intXX)

// wychodzenie ze scope
// (identyfikatory na stosie,
//    wysokość stosu do tego miejsca)
type VStack = (Vec<ast::Ident>, Vec<usize>);
type Label = String;
// type Literal = String;
// type Labels = HashSet<Label>;

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

fn push_wrapper(val: &str, name: Option<&str>, vstack: &mut VStack, output: &mut Output) {
    unimplemented!();
    output.text.push(format!("{} {}", OP_PUSH, val));
    // TODO: utrzymać niezmiennik
}

fn pop_wrapper(reg: &str, vstack: &mut VStack, output: &mut Output) {
    unimplemented!();
    output.text.push(format!("{} {}", OP_PUSH, reg));
}

// TODO: fix vstack...

// TODO: czy jednak adresować względem rbp (a nie rsp)?
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
    println!("{} {} {}", OP_INC, REG_STACK, h_before - h_after);
}

fn vstack_bind_stack_args(vstack: &mut VStack, arg_names: &Vec<ast::Ident>) {
    unimplemented!();
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

    for fdef in fdefs {
        compile_fn(fdef, &mut labels, &mut output);
    }

    for directive in output.directives {
        println!("{}", directive);
    }

    println!();
    println!(".rodata");
    for rodata in output.rodata {
        println!("{}", rodata);
    }

    println!();
    println!(".text");
    for instruction in output.text {
        println!("{}", instruction);
    }
}

fn compile_fn(fdef: &ast::Node<ast::FunDef>, labels: &mut HashSet<Label>, output: &mut Output) {
    let mut vstack: VStack = (vec![], vec![0]);

    let (_, ident_node, arg_nodes, block_node) = fdef.data();
    let arg_names: Vec<ast::Ident> = arg_nodes
        .iter()
        .map(|arg_node| arg_node.data().1.data().clone())
        .collect();
    output.text.push(format!("{}:", ident_node.data()));

    // prolog
    output.text.push(format!("{} {}", OP_PUSH, REG_BASE));
    output
        .text
        .push(format!("{} {} {}", OP_MOV, REG_BASE, REG_STACK));

    vstack_bind_stack_args(&mut vstack, &arg_names);
    for i in 0..std::cmp::min(6, arg_nodes.len()) {
        push_wrapper(ARG_REGS[i], Some(&arg_names[i]), &mut vstack, output);
    }

    compile_block(block_node.data(), &mut vstack, labels, output);

    // epilog
    output
        .text
        .push(format!("{} {} [{}]", OP_MOV, REG_BASE, REG_BASE));
    output.text.push(format!("{}", OP_RET));
}

fn compile_block(
    stmts: &Vec<ast::Node<ast::Stmt>>,
    vstack: &mut VStack,
    labels: &mut HashSet<Label>,
    output: &mut Output,
) {
    vstack_enter_scope(vstack);

    // TODO: czy to wystarczy?
    for stmt in stmts {
        compile_stmt(stmt, vstack, labels, output);
    }

    vstack_exit_scope(vstack);
}

fn compile_stmt(
    stmt: &ast::Node<ast::Stmt>,
    vstack: &mut VStack,
    labels: &mut HashSet<Label>,
    output: &mut Output,
) {
    match stmt.data() {
        ast::Stmt::Empty => (),
        ast::Stmt::Expr(expr_node) => compile_expr(expr_node, vstack, labels, output),
        ast::Stmt::Block(block_node) => compile_block(block_node.data(), vstack, labels, output),
        ast::Stmt::Incr(ident_node) | ast::Stmt::Decr(ident_node) => {
            let var_expr = ast::Expr::Var(ident_node.clone());
            let var_node = ast::Node::new(Span::new(0, 0), var_expr);
            compile_expr(&var_node, vstack, labels, output);
            pop_wrapper(REG_TEMP, vstack, output);
            let op_code = match stmt.data() {
                ast::Stmt::Incr(_) => OP_INC,
                ast::Stmt::Decr(_) => OP_DEC,
                _ => unreachable!(),
            };
            output.text.push(format!("{} {} {}", op_code, REG_TEMP, 1));
            // TODO: aktualizacja wartości na stosie:
            // wczytać wartość do rejestru
            // zmienić
            // zapisać do rejestru
            // NIE wpychać na stos
        }
        ast::Stmt::VRet => (),
        ast::Stmt::Ret(expr_node) => {
            compile_expr(expr_node, vstack, labels, output);
            pop_wrapper(REG_FN_RETVAL, vstack, output);
        }
        ast::Stmt::Decl(_, item_nodes) => {
            for item_node in item_nodes {
                match item_node.data() {
                    ast::Item::NoInit(ident_node) => {
                        push_wrapper("0", Some(&ident_node.data().clone()), vstack, output);
                    }
                    ast::Item::Init(ident_node, expr_node) => {
                        compile_expr(expr_node, vstack, labels, output);
                        vstack_insert(vstack, ident_node.data().clone());
                    }
                };
            }
        }
        ast::Stmt::Asgn(ident_node, expr_node) => {
            unimplemented!();
        }
        // TODO: uwaga na scope (w ifie bez {})
        ast::Stmt::If(expr_node, stmt_node) => {
            let if_label_after = format!("if_{}_after", labels.len()); // "{}_after", if_label
            labels.insert(if_label_after.clone());

            compile_expr(expr_node, vstack, labels, output);
            pop_wrapper(REG_OP_AUX, vstack, output);
            output
                .text
                .push(format!("{} {} {}", OP_XOR, REG_TEMP, REG_TEMP));

            output.text.push(format!("{} {} {}", OP_CMP, REG_TEMP, 0));
            output.text.push(format!("{} {}", JMP_EQ, if_label_after));
            compile_stmt(stmt_node, vstack, labels, output);
            output.text.push(format!("{}:", if_label_after));
        }
        ast::Stmt::IfElse(expr_node, true_stmt_node, false_stmt_node) => {
            unimplemented!();
        }
        ast::Stmt::While(expr_node, stmt_node) => {
            unimplemented!();
        }
    }
}

fn compile_expr(
    expr: &ast::Node<ast::Expr>,
    vstack: &mut VStack,
    labels: &mut HashSet<Label>,
    output: &mut Output,
) {
    match expr.data() {
        ast::Expr::Int(n) => push_wrapper(&n.to_string(), None, vstack, output),
        ast::Expr::Bool(b) => {
            push_wrapper(&(if *b { 1 } else { 0 }).to_string(), None, vstack, output)
        }
        ast::Expr::Str(s) => {
            let label = format!("str_{}", labels.len() + 1);
            labels.insert(label.clone());
            output.rodata.push(format!("{}: db `{}`, 0", label, s,));
            // TODO?: .len:  equ   $ - label
            push_wrapper(REG_OP_MAIN, None, vstack, output);
        }
        ast::Expr::Var(ident_node) => {
            // TODO: vstack... | OP_INC vs OP_DEC
            let offset = vstack_get_offset(vstack, ident_node.data());
            output.text.push(format!(
                "{} {} [{} - {}]",
                OP_MOV, REG_OP_AUX, REG_BASE, offset
            ));
            push_wrapper(&format!("[{}]", REG_OP_AUX), None, vstack, output);
        }
        ast::Expr::Neg(expr_node) => {
            compile_expr(expr_node, vstack, labels, output);
            pop_wrapper(REG_OP_MAIN, vstack, output);
            output.text.push(format!("{} {}", OP_NEGATION, REG_OP_MAIN));
            push_wrapper(REG_OP_MAIN, None, vstack, output);
        }
        ast::Expr::Not(expr_node) => {
            compile_expr(expr_node, vstack, labels, output);
            pop_wrapper(REG_OP_AUX, vstack, output);
            output
                .text
                .push(format!("{} {} {}", OP_XOR, REG_TEMP, REG_TEMP));
            output.text.push(format!("{} {} {}", OP_CMP, REG_OP_AUX, 0));
            output
                .text
                .push(format!("{} {}", OP_SETCC_NEQ, REG_TEMP_BYTE));
            push_wrapper(REG_TEMP_BYTE, None, vstack, output);
        }
        ast::Expr::App(fname_node, arg_expr_nodes) => {
            let fname = fname_node.data();
            let args_count = arg_expr_nodes.len();
            for i in 0..std::cmp::min(6, args_count) {
                compile_expr(&arg_expr_nodes[i], vstack, labels, output);
                pop_wrapper(ARG_REGS[i], vstack, output);
            }
            // TODO: dobra kolejność? Chyba tak...
            for i in (6..args_count).rev() {
                compile_expr(&arg_expr_nodes[i], vstack, labels, output);
            }
            output.text.push(format!("{} {}", OP_CALL, fname));
            push_wrapper(REG_FN_RETVAL, None, vstack, output);
        }
        ast::Expr::Add(expr1, expr2)
        | ast::Expr::Sub(expr1, expr2)
        | ast::Expr::Mul(expr1, expr2) => {
            compile_expr(&expr1, vstack, labels, output);
            compile_expr(&expr2, vstack, labels, output);
            pop_wrapper(REG_OP_AUX, vstack, output);
            pop_wrapper(REG_OP_MAIN, vstack, output);

            let opcode = match expr.data() {
                ast::Expr::Add(_, _) => OP_INC,
                ast::Expr::Sub(_, _) => OP_DEC,
                ast::Expr::Mul(_, _) => OP_IMUL,
                _ => unreachable!(),
            };
            output
                .text
                .push(format!("{} {} {}", opcode, REG_OP_MAIN, REG_OP_AUX));

            push_wrapper(REG_OP_MAIN, None, vstack, output);
        }
        ast::Expr::Div(expr1, expr2) | ast::Expr::Mod(expr1, expr2) => {
            compile_expr(&expr1, vstack, labels, output);
            compile_expr(&expr2, vstack, labels, output);
            pop_wrapper(REG_OP_AUX, vstack, output);
            pop_wrapper(REG_DIVIDEND_LOW, vstack, output);

            output.text.push(format!(
                "{} {} {}",
                OP_XOR, REG_DIVIDEND_HIGH, REG_DIVIDEND_HIGH
            ));
            output.text.push(format!("{} {}", OP_IDIV, REG_OP_AUX));

            let result_reg = match expr.data() {
                ast::Expr::Div(_, _) => REG_QUOTIENT,
                ast::Expr::Mod(_, _) => REG_REMAINDER,
                _ => unreachable!(),
            };
            push_wrapper(result_reg, None, vstack, output);
        }
        ast::Expr::And(expr1, expr2) | ast::Expr::Or(expr1, expr2) => {
            // TODO: leniwość
            compile_expr(&expr1, vstack, labels, output);
            compile_expr(&expr2, vstack, labels, output);
            pop_wrapper(REG_OP_AUX, vstack, output);
            pop_wrapper(REG_OP_MAIN, vstack, output);

            let opcode = match expr.data() {
                ast::Expr::And(_, _) => OP_AND,
                ast::Expr::Or(_, _) => OP_OR,
                _ => unreachable!(),
            };
            output
                .text
                .push(format!("{} {} {}", opcode, REG_OP_MAIN, REG_OP_AUX));
            push_wrapper(REG_OP_MAIN, None, vstack, output);
        }
        ast::Expr::EQ(expr1, expr2)
        | ast::Expr::NEQ(expr1, expr2)
        | ast::Expr::LTH(expr1, expr2)
        | ast::Expr::LEQ(expr1, expr2)
        | ast::Expr::GTH(expr1, expr2)
        | ast::Expr::GEQ(expr1, expr2) => {
            compile_expr(&expr1, vstack, labels, output);
            compile_expr(&expr2, vstack, labels, output);
            pop_wrapper(REG_OP_AUX, vstack, output);
            pop_wrapper(REG_OP_MAIN, vstack, output);
            output
                .text
                .push(format!("{} {} {}", OP_XOR, REG_TEMP, REG_TEMP));

            output
                .text
                .push(format!("{} {} {}", OP_CMP, REG_OP_MAIN, REG_OP_AUX));
            let opcode = match expr.data() {
                ast::Expr::EQ(_, _) => OP_SETCC_EQ,
                ast::Expr::NEQ(_, _) => OP_SETCC_NEQ,
                ast::Expr::LTH(_, _) => OP_SETCC_LTH,
                ast::Expr::LEQ(_, _) => OP_SETCC_LEQ,
                ast::Expr::GTH(_, _) => OP_SETCC_GTH,
                ast::Expr::GEQ(_, _) => OP_SETCC_GEQ,
                _ => unreachable!(),
            };
            output.text.push(format!("{} {}", opcode, REG_TEMP_BYTE));
        }
    }
}
