use crate::latte_y as ast;

// r12 - r15: "Preserved across function calls"
static REG_OP_TARGET: &str = "r12";
static REG_OP_ARG: &str = "r13";
static REG_FN: &str = "rax";
static MEM_WORD: &str = "qword";

// TODO: zapisanie stringów w .data
// TODO: uwaga na scope w if/while
// TODO: czy wywalanie kompilatora w przypadku błędu typecheckera jest dobre?
//       (niby "ERROR" w pierwszej lini stderr itd)

// wychodzenie ze scope
// (identyfikatory na stosie,
//    długość stosu do tego miejsca)
type VStack = (Vec<ast::Ident>, Vec<usize>);

fn error(msg: &str) -> ! {
    eprintln!("type checker didn't catch error: {}", msg);
    std::process::exit(42)
}

fn vstack_get_offset(vstack: &VStack, ident: ast::Ident) -> usize {
    match vstack.0.iter().position(|i| *i == ident) {
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

fn directives(fdefs: &Vec<ast::Node<ast::FunDef>>) {
    println!(".intel_syntax");

    for fdef in fdefs {
        let fn_name = &fdef.data().1.data();
        println!(".global {}", fn_name);
    }
}

pub fn compile(fdefs: &Vec<ast::Node<ast::FunDef>>) {
    directives(&fdefs);

    // TODO: skasować type hint
    let mut vstack: VStack = (Vec::new(), vec![0]);

    for fdef in fdefs {
        compile_fn(fdef, &mut vstack);
    }
}

fn compile_fn(fdef: &ast::Node<ast::FunDef>, vstack: &mut VStack) {
    // println!("
    //     main:
    //       ret
    // ");

    unimplemented!();
}

fn compile_expr(expr: &ast::Node<ast::Expr>, vstack: &VStack) {
    match expr.data() {
        _ => unimplemented!(),
    }
}
