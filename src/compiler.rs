use std::collections::HashSet;

use crate::latte_y as ast;
use crate::latte_y::IntType;
use std::collections::HashMap;

use ast::SELF_IDENT;

sa::assert_eq_size!(usize, i64, IntType);

static MEM_WORD_SIZE: &str = "qword";
static VAR_SIZE: usize = std::mem::size_of::<IntType>();

static FN_STRCAT: &str = "__strcat";
static FN_NEW: &str = "__new";

static REG_MAIN: &str = "r11";
static REG_AUX: &str = "r10";
static REG_TMP: &str = "r9";
static REG_TMP_BYTE: &str = "r9b";

static REG_FN_RETVAL: &str = "rax";

static REG_BASE: &str = "rbp";
static REG_STACK: &str = "rsp";

static REG_QUOTIENT: &str = "rax";
static REG_REMAINDER: &str = "rdx";
static REG_DIVIDEND: &str = "rax";

static ARG_REGS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

static OP_MOV: &str = "mov";
static OP_LEA: &str = "lea";
static OP_XOR: &str = "xor";
static OP_SUB: &str = "sub";
static OP_ADD: &str = "add";
static OP_INC: &str = "inc";
static OP_DEC: &str = "dec";
static OP_IMUL: &str = "imul";
static OP_IDIV: &str = "idiv";
static OP_NEGATION: &str = "neg";
static OP_POP: &str = "pop";
static OP_PUSH: &str = "push";
static OP_CALL: &str = "call";
static OP_CMP: &str = "cmp";
static OP_RET: &str = "ret";
static OP_SIGN_EXTEND_DIVIDEND: &str = "cqo";

static OP_SETCC_EQ: &str = "sete";
static OP_SETCC_NEQ: &str = "setne";
static OP_SETCC_LTH: &str = "setl";
static OP_SETCC_LEQ: &str = "setle";
static OP_SETCC_GTH: &str = "setg";
static OP_SETCC_GEQ: &str = "setge";

static JMP_EQ: &str = "je";
static JMP_ALWAYS: &str = "jmp";

static VAL_TRUE: IntType = 1;
static VAL_FALSE: IntType = 0;

static EMPTY_STRING_LABEL: &str = "__blank";

static STACK_ARG_OFFSET: usize = 2 * VAR_SIZE;
static VSTACK_VAR_OFFSET: usize = VAR_SIZE;

static VTABLE_IDENT: &str = "__vtable";

// TODO: deduplikacja stringów
// TODO: sprawdzić, czy stos jest posprzątany (nie ma śmieci przy obliczaniu wyrażeń), żeby wołanie funkcji działało
// TODO: sprawdzić "_", unimplemented!, unreachable!
// TODO: funkcje obiektów w runtime (malloc, free, ...)
// TODO: test kompilatora na większej wersji niedziałającego testu backendu (ten z nadpisywaniem rejestrów, w których przekazywane są argumenty)
// TODO: uwaga na zmienne z klasy w metodach
// TODO: upewnienie się, że obliczanie argumentów nie śmieci na stosie (lub sprzątać .tmp przy/przed obliczaniem)
// TODO: dodawanie self do argumentów
// TODO: czy chcemy vtable w structach?
//       chyba tak, ale wtedy chyba warto mieć coś po vtable, żeby pusty vtable nie wskazywał na vtable innej klasy
// TODO: sprawdzić, czy gdzieś wstawienie obiektu nie psuje
// TODO: problem z przedeklarowaniem zmiennej klasy wewnątrz metody
//       rozwiązanie: vstack_variable_present => bool (korzystamy z tego przy wyciąganiu )
// TODO: choćby i jednoużyciowe wrappery na {v,f}members_get_offset?
// TODO: README: wszystkie metody są wirtualne

type VOffsets = Vec<ast::Ident>;
type FOffsets = Vec<(ast::Ident, ast::Ident)>;
type CEnv = HashMap<ast::Ident, (VOffsets, FOffsets)>;

type VStack = (Vec<ast::Ident>, Vec<usize>);
type Label = String;
type Labels = HashSet<Label>;

#[derive(Default)]
struct Output {
    directives: Vec<String>,
    rodata: Vec<String>,
    text: Vec<String>,
}

fn push_wrapper(val: &str, name: Option<&str>, vstack: &mut VStack, output: &mut Output) {
    let stack_name = match name {
        Some(s) => s,
        None => ".tmp",
    };
    output.text.push(format!("{} {}", OP_PUSH, val));
    vstack.0.push(stack_name.to_string());
}

fn pop_wrapper(target: &str, vstack: &mut VStack, output: &mut Output) {
    output.text.push(format!("{} {}", OP_POP, target));
    vstack.0.pop();
}

fn vstack_get_offset(vstack: &VStack, ident: &ast::Ident) -> usize {
    vstack.0.iter().rposition(|i| i == ident).unwrap() * VAR_SIZE + VSTACK_VAR_OFFSET
}

fn vstack_enter_scope(vstack: &mut VStack) {
    vstack.1.push(vstack.0.len());
}

fn vstack_exit_scope(vstack: &mut VStack, output: &mut Output) {
    let h_before = vstack.0.len();
    let h_after = vstack.1.pop().unwrap();

    vstack.0.truncate(h_after);
    if h_after != h_before {
        output.text.push(code_shrink_stack(h_before - h_after));
    }
}

fn vstack_exit_fn(vstack: &mut VStack, output: &mut Output) {
    let locals = vstack.0.len();
    if locals > 0 {
        output.text.push(code_shrink_stack(locals));
    }
}

fn vstack_rename_top(vstack: &mut VStack, new_name: ast::Ident) {
    *vstack.0.last_mut().unwrap() = new_name;
}

fn vstack_shrink_stack(vstack: &mut VStack, vars: usize, output: &mut Output) {
    vstack.0.pop();
    output.text.push(code_shrink_stack(vars));
}

fn vstack_local_exists(vstack: &VStack, vname: &ast::Ident) -> bool {
    vstack.0.contains(vname)
}

fn code_shrink_stack(n: usize) -> String {
    format!("{} {}, {}", OP_ADD, REG_STACK, n * VAR_SIZE)
}

fn code_epilogue() -> Vec<String> {
    let s1 = format!("{} {}, {}", OP_MOV, REG_STACK, REG_BASE);
    let s2 = format!("{} {}", OP_POP, REG_BASE);
    let s3 = format!("{}", OP_RET);
    vec![s1, s2, s3]
}

fn directives(fdefs: &Vec<ast::Node<ast::FunDef>>, output: &mut Output) {
    output.directives.push(".intel_syntax".to_string());

    for fdef in fdefs {
        let fn_name = &fdef.data().1.data();
        print_wrapper(&format!(".global {}", fn_name));
    }

    // TODO: vtable
}

fn register_class_in_env(ident: &ast::Ident, cenv: &mut CEnv, cdefs: &HashMap<ast::Ident, ast::Node<ast::ClassDef>>) {
    let cdef = cdefs.get(ident).unwrap();
    let (self_ident_node, parent_ident_node_option, field_nodes, method_nodes) = cdef.data();
    let self_ident = self_ident_node.data();

    if let Some(_) = cenv.get(self_ident) {
        return;
    }

    let (mut voffsets, mut foffsets) = match parent_ident_node_option {
        Some(parent_ident_node) => {
            register_class_in_env(parent_ident_node.data(), cenv, cdefs);
            cenv.get(parent_ident_node.data()).unwrap().clone()
        }
        None => (vec![VTABLE_IDENT.to_string()], vec![]),
    };

    for field_node in field_nodes {
        let field_ident = field_node.data().1.data();

        if let None = voffsets.iter().position(|vname| vname == field_ident) {
            voffsets.push(field_ident.to_string());
        }
    }

    for method_node in method_nodes {
        let method_ident = method_node.data().1.data();
        let method_entry = (method_ident.to_string(), self_ident.to_string());

        match foffsets.iter().position(|(mname, _mimpl)| mname == method_ident) {
            Some(idx) => foffsets[idx] = method_entry,
            None => foffsets.push(method_entry),
        }
    }

    cenv.insert(self_ident_node.data().to_string(), (voffsets, foffsets));
}

fn class_env(cdefs: &Vec<ast::Node<ast::ClassDef>>) -> CEnv {
    let mut cenv: CEnv = HashMap::new();
    let mut cdefs_map: HashMap<ast::Ident, ast::Node<ast::ClassDef>> = HashMap::new();
    for cdef in cdefs {
        cdefs_map.insert(cdef.data().0.data().clone(), cdef.clone());
    }
    for cdef in cdefs {
        register_class_in_env(cdef.data().0.data(), &mut cenv, &cdefs_map);
    }
    cenv
}

fn print_wrapper(s: &str) {
    if let None = s.find(|c: char| c == '.' || c == ':') {
        println!("    {}", s);
    } else {
        println!("{}", s);
    }
}

pub fn compile(cfdefs: &(Vec<ast::Node<ast::ClassDef>>, Vec<ast::Node<ast::FunDef>>)) {
    let mut output = Output::default();
    let mut labels = HashSet::new();

    let cenv = class_env(&cfdefs.0);

    directives(&cfdefs.1, &mut output);

    labels.insert(EMPTY_STRING_LABEL.to_string());
    output.rodata.push(format!("{}: .asciz \"\"", EMPTY_STRING_LABEL));

    for fdef in &cfdefs.1 {
        compile_fn(fdef, &cenv, &mut labels, &mut output);
    }

    for directive in output.directives {
        print_wrapper(&directive);
    }

    print_wrapper("");
    print_wrapper(".rodata");
    for rodata in output.rodata {
        print_wrapper(&rodata);
    }

    print_wrapper("");
    print_wrapper(".text");
    for instruction in output.text {
        print_wrapper(&instruction);
    }
}

fn compile_fn(fdef: &ast::Node<ast::FunDef>, cenv: &CEnv, labels: &mut Labels, output: &mut Output) {
    let mut vstack: VStack = (vec![], vec![0]);

    let (_, ident_node, arg_nodes, block_node) = fdef.data();
    let arg_names: Vec<ast::Ident> = arg_nodes.iter().map(|arg_node| arg_node.data().1.data().clone()).collect();
    let stack_args_count = if arg_names.len() > ARG_REGS.len() {
        arg_names.len() - ARG_REGS.len()
    } else {
        0
    };

    output.text.push(format!("{}:", ident_node.data()));
    output.text.push(format!("{} {}", OP_PUSH, REG_BASE));
    output.text.push(format!("{} {}, {}", OP_MOV, REG_BASE, REG_STACK));

    for i in 0..std::cmp::min(ARG_REGS.len(), arg_names.len()) {
        push_wrapper(ARG_REGS[i], Some(&arg_names[i]), &mut vstack, output);
    }
    for i in 0..stack_args_count {
        push_wrapper(
            &format!("{} ptr [{} + {}]", MEM_WORD_SIZE, REG_BASE, STACK_ARG_OFFSET + i * VAR_SIZE),
            Some(&arg_names[ARG_REGS.len() + i]),
            &mut vstack,
            output,
        );
    }

    compile_block(block_node.data(), &mut vstack, cenv, labels, output);

    output.text.extend(code_epilogue());
}

fn compile_block(stmts: &Vec<ast::Node<ast::Stmt>>, vstack: &mut VStack, cenv: &CEnv, labels: &mut Labels, output: &mut Output) {
    vstack_enter_scope(vstack);

    for stmt in stmts {
        compile_stmt(stmt, vstack, cenv, labels, output);
    }

    vstack_exit_scope(vstack, output);
}

fn compile_stmt(stmt: &ast::Node<ast::Stmt>, vstack: &mut VStack, cenv: &CEnv, labels: &mut Labels, output: &mut Output) {
    match stmt.data() {
        ast::Stmt::Empty => (),
        ast::Stmt::Expr(expr_node) => {
            compile_expr_ptr(expr_node, vstack, cenv, labels, output);
            vstack_shrink_stack(vstack, 1, output);
        }
        ast::Stmt::Block(block_node) => compile_block(block_node.data(), vstack, cenv, labels, output),
        ast::Stmt::Incr(ident_node) | ast::Stmt::Decr(ident_node) => {
            let offset = vstack_get_offset(vstack, ident_node.data());
            let op_code = match stmt.data() {
                ast::Stmt::Incr(_) => OP_INC,
                ast::Stmt::Decr(_) => OP_DEC,
                _ => unreachable!(),
            };
            output
                .text
                .push(format!("{} {} ptr [{} - {}]", op_code, MEM_WORD_SIZE, REG_BASE, offset));
        }
        ast::Stmt::VRet => {
            vstack_exit_fn(vstack, output);
            output.text.extend(code_epilogue());
        }
        ast::Stmt::Ret(expr_node) => {
            compile_expr_val(expr_node, vstack, cenv, labels, output);
            pop_wrapper(REG_FN_RETVAL, vstack, output);

            vstack_exit_fn(vstack, output);
            output.text.extend(code_epilogue());
        }
        ast::Stmt::Decl(prim_node, item_nodes) => {
            for item_node in item_nodes {
                match item_node.data() {
                    ast::Item::NoInit(ident_node) => {
                        match prim_node.data() {
                            ast::Prim::Int | ast::Prim::Bool | ast::Prim::Class(_) => {
                                let default_value = 0; // VAL_FALSE == 0 == DEFAULT_INT = NULL
                                output.text.push(format!("{} {}, {}", OP_MOV, REG_MAIN, default_value));
                            }
                            ast::Prim::Str => {
                                output.text.push(format!(
                                    "{} {}, {} ptr [{}]",
                                    OP_LEA, REG_MAIN, MEM_WORD_SIZE, EMPTY_STRING_LABEL
                                ));
                            }
                            ast::Prim::Void => unreachable!(),
                        }
                        push_wrapper(REG_MAIN, Some(&ident_node.data().clone()), vstack, output);
                    }
                    ast::Item::Init(ident_node, expr_node) => {
                        compile_expr_val(expr_node, vstack, cenv, labels, output);
                        vstack_rename_top(vstack, ident_node.data().clone());
                    }
                };
            }
        }
        ast::Stmt::Asgn(lhs_expr_node, rhs_expr_node) => {
            compile_expr_val(rhs_expr_node, vstack, cenv, labels, output);

            match lhs_expr_node.data() {
                ast::Expr::Var(_) | ast::Expr::Dot(_, _) => {
                    compile_expr_ptr(lhs_expr_node, vstack, cenv, labels, output);
                }
                _ => unreachable!(),
            }

            pop_wrapper(REG_MAIN, vstack, output);
            pop_wrapper(&format!("{} ptr [{}]", MEM_WORD_SIZE, REG_MAIN), vstack, output);
        }
        ast::Stmt::If(expr_node, stmt_node) => {
            let if_label_after = format!("if_{}_after", labels.len());
            labels.insert(if_label_after.clone());

            compile_expr_val(expr_node, vstack, cenv, labels, output);
            pop_wrapper(REG_MAIN, vstack, output);
            output.text.push(format!("{} {}, {}", OP_CMP, REG_MAIN, VAL_FALSE));
            output.text.push(format!("{} {}", JMP_EQ, if_label_after));
            compile_block(&vec![*stmt_node.clone()], vstack, cenv, labels, output);
            output.text.push(format!("{}:", if_label_after));
        }
        ast::Stmt::IfElse(expr_node, true_stmt_node, false_stmt_node) => {
            let cond_label_after = format!("cond_{}_after", labels.len());
            let cond_label_true = format!("cond_{}_if", labels.len());
            let cond_label_false = format!("cond_{}_else", labels.len());
            labels.extend(vec![
                cond_label_after.clone(),
                cond_label_true.clone(),
                cond_label_false.clone(),
            ]);

            compile_expr_val(expr_node, vstack, cenv, labels, output);
            pop_wrapper(REG_MAIN, vstack, output);
            output.text.push(format!("{} {}, {}", OP_CMP, REG_MAIN, VAL_FALSE));
            output.text.push(format!("{} {}", JMP_EQ, cond_label_false));
            output.text.push(format!("{} {}", JMP_ALWAYS, cond_label_true));

            output.text.push(format!("{}:", &cond_label_true));
            compile_block(&vec![*true_stmt_node.clone()], vstack, cenv, labels, output);
            output.text.push(format!("{} {}", JMP_ALWAYS, cond_label_after));

            output.text.push(format!("{}:", &cond_label_false));
            compile_block(&vec![*false_stmt_node.clone()], vstack, cenv, labels, output);
            output.text.push(format!("{} {}", JMP_ALWAYS, cond_label_after));

            output.text.push(format!("{}:", &cond_label_after));
        }
        ast::Stmt::While(expr_node, stmt_node) => {
            let while_label_cond = format!("while_{}_cond", labels.len());
            let while_label_after = format!("while_{}_after", labels.len());
            labels.extend(vec![while_label_cond.clone(), while_label_after.clone()]);

            output.text.push(format!("{}:", &while_label_cond));
            compile_expr_val(expr_node, vstack, cenv, labels, output);
            pop_wrapper(REG_MAIN, vstack, output);
            output.text.push(format!("{} {}, {}", OP_CMP, REG_MAIN, VAL_FALSE));
            output.text.push(format!("{} {}", JMP_EQ, while_label_after));

            compile_block(&vec![*stmt_node.clone()], vstack, cenv, labels, output);
            output.text.push(format!("{} {}", JMP_ALWAYS, while_label_cond));

            output.text.push(format!("{}:", &while_label_after));
        }
    }
}

// na stosie jest wartość (która dla obiektów i stringów jest wskaźnikiem)
fn compile_expr_val(expr: &ast::Node<ast::Expr>, vstack: &mut VStack, cenv: &CEnv, labels: &mut Labels, output: &mut Output) {
    compile_expr_ptr(expr, vstack, cenv, labels, output);
    match expr.data() {
        ast::Expr::Var(_) | ast::Expr::Dot(_, _) => {
            pop_wrapper(REG_MAIN, vstack, output);
            push_wrapper(&format!("{} ptr [{}]", MEM_WORD_SIZE, REG_MAIN), None, vstack, output);
        }
        _ => (),
    }
}

// na stosie jest wartość (która dla obiektów i stingów jest wskaźnikiem)
//                lub wskaźnik (w przypadku zmiennej lub pola struktury)
fn compile_expr_ptr(expr: &ast::Node<ast::Expr>, vstack: &mut VStack, cenv: &CEnv, labels: &mut Labels, output: &mut Output) {
    match expr.data() {
        ast::Expr::Int(n) => {
            output.text.push(format!("{} {}, {}", OP_MOV, REG_MAIN, n));
            push_wrapper(REG_MAIN, None, vstack, output);
        }
        ast::Expr::Bool(b) => {
            output
                .text
                .push(format!("{} {}, {}", OP_MOV, REG_MAIN, if *b { VAL_TRUE } else { VAL_FALSE }));
            push_wrapper(REG_MAIN, None, vstack, output);
        }
        ast::Expr::Str(s) => {
            let label = format!("str_{}", labels.len() + 1);
            labels.insert(label.clone());
            output.rodata.push(format!("{}: .asciz {}", label, s));
            output
                .text
                .push(format!("{} {}, {} ptr [{}]", OP_LEA, REG_MAIN, MEM_WORD_SIZE, label));
            push_wrapper(REG_MAIN, None, vstack, output);
        }
        ast::Expr::Var(ident_node) => {
            if vstack_local_exists(vstack, ident_node.data()) {
                let offset = vstack_get_offset(vstack, ident_node.data());
                output.text.push(format!("{} {}, {}", OP_MOV, REG_MAIN, REG_BASE));
                output.text.push(format!("{} {}, {}", OP_SUB, REG_MAIN, offset));
                push_wrapper(REG_MAIN, None, vstack, output);
            } else {
                // problem: w której funkcji jestem? (dodatkowy argument do wszystkiego)?
                //                                    Option<ast::Ident>
                unimplemented!();
                let self_offset = vstack_get_offset(vstack, &SELF_IDENT.to_string());
                let field_offset = 0; // * VAR_SIZE

                output.text.push(format!("{} {}, {}", OP_MOV, REG_MAIN, REG_BASE));
                output.text.push(format!("{} {}, {}", OP_SUB, REG_MAIN, self_offset));
                output.text.push(format!("{} {}, {}", OP_ADD, REG_MAIN, field_offset));
                push_wrapper(REG_MAIN, None, vstack, output);
            }
        }
        ast::Expr::Neg(expr_node) => {
            compile_expr_val(expr_node, vstack, cenv, labels, output);
            pop_wrapper(REG_MAIN, vstack, output);
            output.text.push(format!("{} {}", OP_NEGATION, REG_MAIN));
            push_wrapper(REG_MAIN, None, vstack, output);
        }
        ast::Expr::Not(expr_node) => {
            compile_expr_val(expr_node, vstack, cenv, labels, output);
            pop_wrapper(REG_AUX, vstack, output);
            output.text.push(format!("{} {}, {}", OP_XOR, REG_TMP, REG_TMP));
            output.text.push(format!("{} {}, {}", OP_CMP, REG_AUX, VAL_FALSE));
            output.text.push(format!("{} {}", OP_SETCC_EQ, REG_TMP_BYTE));
            push_wrapper(REG_TMP, None, vstack, output);
        }
        ast::Expr::Fun(fname_node, arg_expr_nodes) => {
            let fname = fname_node.data();
            let args_count = arg_expr_nodes.len();
            let stack_args_count = if args_count > ARG_REGS.len() {
                args_count - ARG_REGS.len()
            } else {
                0
            };
            // TODO: nadpisywany rejestr jeśli przy obliczaniu wartości wyrażenia wywołujemy funkcję
            // TODO: policzyć wszystkie argumenty, a potem dopiero robić pop()
            for i in (0..args_count).rev() {
                compile_expr_val(&arg_expr_nodes[i], vstack, cenv, labels, output);
            }
            for i in 0..std::cmp::min(ARG_REGS.len(), args_count) {
                pop_wrapper(ARG_REGS[i], vstack, output);
            }
            output.text.push(format!("{} {}", OP_CALL, fname));
            if stack_args_count > 0 {
                vstack_shrink_stack(vstack, stack_args_count, output);
            }
            push_wrapper(REG_FN_RETVAL, None, vstack, output);
        }
        ast::Expr::Add(expr1, expr2) if expr1.get_prim() == ast::Prim::Str => {
            compile_expr_val(&expr1, vstack, cenv, labels, output);
            compile_expr_val(&expr2, vstack, cenv, labels, output);
            pop_wrapper(ARG_REGS[1], vstack, output);
            pop_wrapper(ARG_REGS[0], vstack, output);

            output.text.push(format!("{} {}", OP_CALL, FN_STRCAT));

            push_wrapper(REG_FN_RETVAL, None, vstack, output);
        }
        ast::Expr::Add(expr1, expr2) | ast::Expr::Sub(expr1, expr2) | ast::Expr::Mul(expr1, expr2) => {
            compile_expr_val(&expr1, vstack, cenv, labels, output);
            compile_expr_val(&expr2, vstack, cenv, labels, output);
            pop_wrapper(REG_AUX, vstack, output);
            pop_wrapper(REG_MAIN, vstack, output);

            let opcode = match expr.data() {
                ast::Expr::Add(_, _) => OP_ADD,
                ast::Expr::Sub(_, _) => OP_SUB,
                ast::Expr::Mul(_, _) => OP_IMUL,
                _ => unreachable!(),
            };
            output.text.push(format!("{} {}, {}", opcode, REG_MAIN, REG_AUX));

            push_wrapper(REG_MAIN, None, vstack, output);
        }
        ast::Expr::Div(expr1, expr2) | ast::Expr::Mod(expr1, expr2) => {
            compile_expr_val(&expr1, vstack, cenv, labels, output);
            compile_expr_val(&expr2, vstack, cenv, labels, output);
            pop_wrapper(REG_AUX, vstack, output);
            pop_wrapper(REG_DIVIDEND, vstack, output);

            output.text.push(OP_SIGN_EXTEND_DIVIDEND.to_string());
            output.text.push(format!("{} {}", OP_IDIV, REG_AUX));

            let result_reg = match expr.data() {
                ast::Expr::Div(_, _) => REG_QUOTIENT,
                ast::Expr::Mod(_, _) => REG_REMAINDER,
                _ => unreachable!(),
            };
            push_wrapper(result_reg, None, vstack, output);
        }
        ast::Expr::And(expr1, expr2) | ast::Expr::Or(expr1, expr2) => {
            let skipping_value = match expr.data() {
                ast::Expr::And(_, _) => VAL_FALSE,
                ast::Expr::Or(_, _) => VAL_TRUE,
                _ => unreachable!(),
            };

            let or_and_label_after = format!("or_and_{}_after", labels.len());
            labels.insert(or_and_label_after.clone());

            compile_expr_val(&expr1, vstack, cenv, labels, output);
            // TODO: dlaczego nie pop_wrapper() ?
            output
                .text
                .push(format!("{} {}, {} ptr [{}]", OP_MOV, REG_MAIN, MEM_WORD_SIZE, REG_STACK));

            output.text.push(format!("{} {}, {}", OP_CMP, REG_MAIN, skipping_value));
            output.text.push(format!("{} {}", JMP_EQ, or_and_label_after));

            vstack_shrink_stack(vstack, 1, output);
            compile_expr_val(&expr2, vstack, cenv, labels, output);
            output.text.push(format!("{}:", or_and_label_after));
        }
        ast::Expr::EQ(expr1, expr2)
        | ast::Expr::NEQ(expr1, expr2)
        | ast::Expr::LTH(expr1, expr2)
        | ast::Expr::LEQ(expr1, expr2)
        | ast::Expr::GTH(expr1, expr2)
        | ast::Expr::GEQ(expr1, expr2) => {
            compile_expr_val(&expr1, vstack, cenv, labels, output);
            compile_expr_val(&expr2, vstack, cenv, labels, output);
            pop_wrapper(REG_AUX, vstack, output);
            pop_wrapper(REG_MAIN, vstack, output);
            output.text.push(format!("{} {}, {}", OP_XOR, REG_TMP, REG_TMP));

            output.text.push(format!("{} {}, {}", OP_CMP, REG_MAIN, REG_AUX));
            let opcode = match expr.data() {
                ast::Expr::EQ(_, _) => OP_SETCC_EQ,
                ast::Expr::NEQ(_, _) => OP_SETCC_NEQ,
                ast::Expr::LTH(_, _) => OP_SETCC_LTH,
                ast::Expr::LEQ(_, _) => OP_SETCC_LEQ,
                ast::Expr::GTH(_, _) => OP_SETCC_GTH,
                ast::Expr::GEQ(_, _) => OP_SETCC_GEQ,
                _ => unreachable!(),
            };
            output.text.push(format!("{} {}", opcode, REG_TMP_BYTE));
            push_wrapper(REG_TMP, None, vstack, output);
        }
        ast::Expr::Null(_ident_node) => {
            output.text.push(format!("{} {}, {}", OP_MOV, REG_MAIN, 0));
            push_wrapper(REG_MAIN, None, vstack, output);
        }
        ast::Expr::New(ident_node) => {
            // TODO: inicjalizacja stringów na puste
            // TODO: ustawienie vtable (argument do __new?)
            let object_size = cenv.get(ident_node.data()).unwrap().0.len() * VAR_SIZE;
            output.text.push(format!("{} {}, {}", OP_MOV, ARG_REGS[0], object_size));
            output.text.push(format!("{} {}", OP_CALL, FN_NEW));
            push_wrapper(REG_FN_RETVAL, None, vstack, output);
        }
        ast::Expr::Dot(lhs_node, field_ident_node) => {
            compile_expr_val(lhs_node, vstack, cenv, labels, output);
            pop_wrapper(REG_MAIN, vstack, output);

            match lhs_node.get_prim() {
                ast::Prim::Class(class_name) => {
                    let voffset = cenv
                        .get(&class_name)
                        .unwrap()
                        .0
                        .iter()
                        .position(|vname| vname == field_ident_node.data())
                        .unwrap()
                        * VAR_SIZE;

                    output.text.push(format!("{} {}, {}", OP_ADD, REG_MAIN, voffset));
                    push_wrapper(REG_MAIN, None, vstack, output);
                }
                _ => unreachable!(),
            }
        }
        ast::Expr::Mthd(_expr_node, _ident_node, _arg_nodes) => unimplemented!(),
    }
}
