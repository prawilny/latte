// TODO: data() => data()

use std::collections::HashMap;
use crate::latte_y as ast;
use crate::latte_y::IntType;
use crate::Span;
use ::lrpar::NonStreamingLexer as Lexer;

type VEnv = Vec<HashMap<ast::Ident, ast::Prim>>;
type FEnv = HashMap<ast::Ident, ast::FunType>;

fn venv_get(venv: &VEnv, key: &ast::Ident) -> Option<ast::Prim> {
    for scope in venv.iter().rev() {
        if let Some(prim) = scope.get(key) {
            return Some(prim.clone());
        }
    }
    None
}

fn venv_insert(venv: &mut VEnv, key: ast::Ident, val: ast::Prim) -> Option<ast::Prim> {
    let prev = venv_get(venv, &key);
    venv.last_mut().unwrap().insert(key, val);
    prev
}

fn venv_enter_scope(venv: &mut VEnv) -> () {
    venv.push(HashMap::new());
}

fn venv_exit_scope(venv: &mut VEnv) -> () {
    venv.pop();
}

fn wrap_error_msg(lexer: &dyn Lexer<u32>, span: &Span, msg: &str) -> String {
    let token = lexer.span_str(*span);
    let token_lines = lexer.span_lines_str(*span);
    let ((start_line, start_column), (end_line, end_column)) = lexer.line_col(*span);

    format!("{} at {}:{}-{}:{} \nin {} \nin '{}'",
        msg, start_line, start_column, end_line, end_column,
        token, token_lines)
}

fn undeclared_var_msg(lexer: &dyn Lexer<u32>, span: &Span) -> String {
    wrap_error_msg(lexer, span, "use of undeclared variable")
}

fn type_mismatch_msg(expected_type: ast::Prim, actual_type: &ast::Prim, lexer: &dyn Lexer<u32>, span: &Span) -> String {
    let msg = format!("type mismatch at: expected {}, got {}", expected_type, actual_type);
    wrap_error_msg(lexer, span, &msg)
}

fn multiple_return_types(prim1: &ast::Prim, prim2: &ast::Prim, lexer: &dyn Lexer<u32>, span: &Span) -> String {
    let msg = format!("multiple return types: {} and {}", prim1, prim2);
    wrap_error_msg(lexer, span, &msg)
}

pub fn check_types(fdefs: &Vec<ast::Node<ast::FunDef>>, lexer: &dyn Lexer<u32>) -> Result<(), String> {
    let env = fn_env(fdefs, lexer)?;

    for fdef in fdefs {
        check_fn(fdef, &env, lexer)?;
    }

    Ok(())
}

fn expr_bool(expr: &ast::Node<ast::Expr>, lexer: &dyn Lexer<u32>) -> Result<Option<bool>, String> {
    match expr.data() {
        ast::Expr::Bool(b) => Ok(Some(*b)),
        ast::Expr::Not(expr_node) => {
            if let Ok(Some(b)) = expr_bool(expr_node, lexer) {
                Ok(Some(!b))
            } else {
                Ok(None)
            }
        },
        ast::Expr::And(expr1_node, expr2_node) |
        ast::Expr::Or(expr1_node, expr2_node) => {
            match (expr_bool(expr1_node, lexer)?, expr_bool(expr2_node, lexer)?) {
                (Some(b1), Some(b2)) => {
                    match expr.data() {
                        ast::Expr::And(_, _) => Ok(Some(b1 && b2)),
                        ast::Expr::Or(_, _) => Ok(Some(b1 || b2)),
                        _ => unreachable!(),
                    }
                },
                _ => Ok(None),
            }
        },
        ast::Expr::LTH(expr1_node, expr2_node) |
        ast::Expr::LEQ(expr1_node, expr2_node) |
        ast::Expr::GTH(expr1_node, expr2_node) |
        ast::Expr::GEQ(expr1_node, expr2_node) |
        ast::Expr::NEQ(expr1_node, expr2_node) |
        ast::Expr::EQ(expr1_node, expr2_node) => {
            match (expr_int(expr1_node, lexer)?, expr_int(expr2_node, lexer)?) {
                (Some(e1), Some(e2)) => {
                    match expr.data() {
                        ast::Expr::LTH(_, _) => Ok(Some(e1 < e2)),
                        ast::Expr::LEQ(_, _) => Ok(Some(e1 <= e2)),
                        ast::Expr::GTH(_, _) => Ok(Some(e1 > e2)),
                        ast::Expr::GEQ(_, _) => Ok(Some(e1 >= e2)),
                        ast::Expr::NEQ(_, _) => Ok(Some(e1 != e2)),
                        ast::Expr::EQ(_, _) => Ok(Some(e1 == e2)),
                        _ => unreachable!(),
                    }
                },
                _ => Ok(None),
            }
        },
        _ => Ok(None),
    }
}

fn expr_int(expr: &ast::Node<ast::Expr>, lexer: &dyn Lexer<u32>) -> Result<Option<IntType>, String> {
    match expr.data() {
        ast::Expr::Int(i) => Ok(Some(*i)),
        ast::Expr::Neg(expr_node) => {
            if let Ok(Some(i)) = expr_int(expr_node, lexer) {
                Ok(Some(-1 * i))
            } else {
                Ok(None)
            }
        },
        ast::Expr::Add(expr1_node, expr2_node) |
        ast::Expr::Sub(expr1_node, expr2_node) |
        ast::Expr::Mul(expr1_node, expr2_node) => {
            match (expr_int(expr1_node, lexer)?, expr_int(expr2_node, lexer)?) {
                (Some(e1), Some(e2)) => {
                    match expr.data() {
                        ast::Expr::Add(_, _) => Ok(Some(e1 + e2)),
                        ast::Expr::Sub(_, _) => Ok(Some(e1 - e2)),
                        ast::Expr::Mul(_, _) => Ok(Some(e1 * e2)),
                        _ => unreachable!(),
                    }
                },
                _ => Ok(None),
            }
        },
        ast::Expr::Div(expr1_node, expr2_node) |
        ast::Expr::Mod(expr1_node, expr2_node) => {
            match (expr_int(expr1_node, lexer)?, expr_int(expr2_node, lexer)?) {
                (_, Some(0)) => {
                    Err(wrap_error_msg(lexer, expr2_node.span(), "Div/mod by 0"))
                }
                (Some(e1), Some(e2)) => {
                    match expr.data() {
                        ast::Expr::Div(_, _) => Ok(Some(e1 / e2)),
                        ast::Expr::Mod(_, _) => Ok(Some(e1 % e2)),
                        _ => unreachable!(),
                    }
                },
                _ => Ok(None),
            }
        },
        _ => Ok(None),
    }
}

fn check_expr(expr: &ast::Node<ast::Expr>, venv: &VEnv, fenv: &FEnv, lexer: &dyn Lexer<u32>) -> Result<ast::Prim, String> {
    match expr.data() {
        ast::Expr::App(ident_node, expr_nodes) => {
            let (fun_type, fun_arg_types) = match fenv.get(ident_node.data()) {
                None => {
                    return Err(wrap_error_msg(lexer, ident_node.span(), "use of undeclared function"));
                }
                Some(ft) => ft,
            };

            if fun_arg_types.len() != expr_nodes.len() {
                let msg = format!("use of {}-argument function with {} arguments",
                    fun_arg_types.len(), expr_nodes.len());
                return Err(wrap_error_msg(lexer, ident_node.span(), &msg));
            }

            for (arg_prim, expr_node) in fun_arg_types.iter().zip(expr_nodes.iter()) {
                let expr_prim = check_expr(expr_node, venv, fenv, lexer)?;
                if expr_prim != *arg_prim {
                    return Err(type_mismatch_msg(arg_prim.clone(), &expr_prim, lexer, expr_node.span()))
                }
            }

            Ok(fun_type.clone())
        },
        ast::Expr::Var(ident_node) => {
            match venv_get(venv, ident_node.data()) {
                Some(prim) => Ok(prim),
                None => {
                    Err(undeclared_var_msg(lexer, ident_node.span()))
                }
            }
        },
        ast::Expr::Int(_) => Ok(ast::Prim::Int),
        ast::Expr::Bool(_) => Ok(ast::Prim::Bool),
        ast::Expr::Str(_) => Ok(ast::Prim::Str),
        ast::Expr::Neg(expr_node) | ast::Expr::Not(expr_node) => {
            match check_expr(expr_node, venv, fenv, lexer)? {
                ast::Prim::Bool => Ok(ast::Prim::Bool),
                prim => Err(type_mismatch_msg(ast::Prim::Bool, &prim, lexer, expr_node.span())),
            }
        }
        ast::Expr::And(expr1_node, expr2_node) | ast::Expr::Or(expr1_node, expr2_node) => {
            match check_expr(expr1_node, venv, fenv, lexer)? {
                ast::Prim::Bool => {
                    match check_expr(expr2_node, venv, fenv, lexer)? {
                        ast::Prim::Bool => Ok(ast::Prim::Bool),
                        prim => Err(type_mismatch_msg(ast::Prim::Bool, &prim, lexer, expr2_node.span())),
                    }
                },
                prim => Err(type_mismatch_msg(ast::Prim::Bool, &prim, lexer, expr1_node.span())),
            }
        }
        ast::Expr::Add(expr1_node, expr2_node) | ast::Expr::Div(expr1_node, expr2_node) |
        ast::Expr::Sub(expr1_node, expr2_node) | ast::Expr::Mod(expr1_node, expr2_node) |
        ast::Expr::Mul(expr1_node, expr2_node) => {
            match check_expr(expr1_node, venv, fenv, lexer)? {
                ast::Prim::Int => {
                    match check_expr(expr2_node, venv, fenv, lexer)? {
                        ast::Prim::Int => Ok(ast::Prim::Int),
                        prim => Err(type_mismatch_msg(ast::Prim::Int, &prim, lexer, expr2_node.span())),
                    }
                },
                prim => Err(type_mismatch_msg(ast::Prim::Int, &prim, lexer, expr1_node.span())),
            }
        }
        ast::Expr::LTH(expr1_node, expr2_node) | ast::Expr::LEQ(expr1_node, expr2_node) |
        ast::Expr::GTH(expr1_node, expr2_node) | ast::Expr::GEQ(expr1_node, expr2_node) |
        ast::Expr::NEQ(expr1_node, expr2_node) | ast::Expr::EQ(expr1_node, expr2_node) => {
            match check_expr(expr1_node, venv, fenv, lexer)? {
                ast::Prim::Int => {
                    match check_expr(expr2_node, venv, fenv, lexer)? {
                        ast::Prim::Int => Ok(ast::Prim::Bool),
                        prim => Err(type_mismatch_msg(ast::Prim::Int, &prim, lexer, expr2_node.span())),
                    }
                },
                prim => Err(type_mismatch_msg(ast::Prim::Int, &prim, lexer, expr1_node.span())),
            }
        }
    }

}

fn check_block(stmts: &Vec<ast::Node<ast::Stmt>>, mut venv: &mut VEnv, fenv: &FEnv, lexer: &dyn Lexer<u32>, block_span: &Span) -> Result<Option<ast::Prim>, String> {
    let mut block_returns = None;

    venv_enter_scope(venv);
    for stmt in stmts {
        if let Some(stmt_prim) = check_stmt(&stmt, &mut venv, fenv, lexer)? {
            match block_returns.clone() {
                None => {
                    block_returns = Some(stmt_prim.clone());
                },
                Some(prev_stmt_prim) => {
                    if prev_stmt_prim != stmt_prim {
                        return Err(multiple_return_types(&prev_stmt_prim, &stmt_prim, lexer, block_span));
                    }
                }
            }
        }
    }
    venv_exit_scope(venv);
    Ok(block_returns)
}

// TODO: typ na Result<Option<ast::Prim>, ()>, żeby sprawdzać typ returna
fn check_stmt(stmt: &ast::Node<ast::Stmt>, mut venv: &mut VEnv, fenv: &FEnv, lexer: &dyn Lexer<u32>) -> Result<Option<ast::Prim>, String> {
    match stmt.data() {
        ast::Stmt::Empty => Ok(None),
        ast::Stmt::VRet => Ok(Some(ast::Prim::Void)),
        ast::Stmt::Block(block_node) => check_block(block_node.data(), &mut venv, fenv, lexer, block_node.span()),
        ast::Stmt::Expr(expr_node) | ast::Stmt::Ret(expr_node) => Ok(Some(check_expr(&expr_node, &mut venv, fenv, lexer)?)),
        ast::Stmt::Decl(prim_node, item_nodes) => {
            let prim = prim_node.data();
            for item_node in item_nodes {
                let (key, val) = match item_node.data() {
                    ast::Item::NoInit(ident_node) => (ident_node.data().clone(), prim.clone()),
                    ast::Item::Init(ident_node, expr_node)
                        => (ident_node.data().clone(), check_expr(&expr_node, &mut venv, fenv, lexer)?),
                };
                venv_insert(venv, key, val);
            }
            Ok(None)
        },
        ast::Stmt::Asgn(ident_node, expr_node) => {
            let expr_prim = check_expr(&expr_node, &mut venv, fenv, lexer)?;
            match venv_insert(venv, ident_node.data().clone(), expr_prim) {
                Some(_) => Ok(None),
                None => Err(undeclared_var_msg(lexer, ident_node.span())),
            }

        },
        ast::Stmt::Incr(ident_node) | ast::Stmt::Decr(ident_node) => {
            let ident = ident_node.data().clone();
            match venv_get(venv, &ident) {
                Some(ast::Prim::Int) => Ok(Some(ast::Prim::Int)),
                Some(prim) => Err(type_mismatch_msg(ast::Prim::Int, &prim, lexer, ident_node.span())),
                None => Err(undeclared_var_msg(lexer, ident_node.span()))
            }
        },
        ast::Stmt::If(expr_node, stmt_node) | ast::Stmt::While(expr_node, stmt_node) => {
            let expr_prim = check_expr(&expr_node, &mut venv, fenv, lexer)?;
            if expr_prim == ast::Prim::Bool {
                venv_enter_scope(venv);
                let stmt_return_type = check_stmt(&stmt_node, &mut venv, fenv, lexer)?;
                venv_exit_scope(venv);
                if let Some(false) = expr_bool(expr_node, lexer)? {
                    Ok(None)
                } else {
                    Ok(stmt_return_type)
                }
            } else {
                Err(type_mismatch_msg(ast::Prim::Bool, &expr_prim, lexer, expr_node.span()))
            }
        },
        ast::Stmt::IfElse(expr_node, stmt_true_node, stmt_false_node) => {
            let expr_prim = check_expr(&expr_node, &mut venv, fenv, lexer)?;
            if expr_prim == ast::Prim::Bool {
                venv_enter_scope(venv);
                let stmt_true_return_type = check_stmt(&stmt_true_node, &mut venv, fenv, lexer)?;
                venv_exit_scope(venv);
                venv_enter_scope(venv);
                let stmt_false_return_type = check_stmt(&stmt_false_node, &mut venv, fenv, lexer)?;
                venv_exit_scope(venv);
                match expr_bool(expr_node, lexer)? {
                    Some(true) => Ok(stmt_true_return_type),
                    Some(false) => Ok(stmt_false_return_type),
                    _ => {
                        match (stmt_true_return_type, stmt_false_return_type) {
                            (Some(prim_true), Some(prim_false)) => {
                                if prim_true != prim_false {
                                    Err(multiple_return_types(&prim_true, &prim_false, lexer, stmt.span()))
                                } else {
                                    Ok(Some(prim_true))
                                }
                            },
                            (None, Some(prim)) => Ok(Some(prim)),
                            (Some(prim), None) => Ok(Some(prim)),
                            (_, _) => Ok(None),
                        }
                    },
                }
            } else {
                Err(type_mismatch_msg(ast::Prim::Bool, &expr_prim, lexer, expr_node.span()))
            }
        },
    }
}

// w tym momencie żądam returna na końcu funkcji typu różnego niż void
fn check_fn(fdef: &ast::Node<ast::FunDef>, fenv: &FEnv, lexer: &dyn Lexer<u32>) -> Result<(), String> {
    let mut venv = VEnv::new();
    venv_enter_scope(&mut venv);

    let (prim_node, _, arg_nodes, block_node) = fdef.data();
    for arg_node in arg_nodes {
        let (arg_prim_node, arg_ident_node) = arg_node.data();
        let (prim, ident) = (arg_prim_node.data(), arg_ident_node.data());
        if let Some(_) = venv_insert(&mut venv, ident.clone(), prim.clone()) {
            return Err(wrap_error_msg(lexer, fdef.span(), "Argument name repeated"))
        }
    }

    let expected_type = prim_node.data();
    let actual_type = match check_block(block_node.data(), &mut venv, fenv, lexer, block_node.span())? {
        None => {
            match *expected_type {
                ast::Prim::Void => ast::Prim::Void,
                _ => return Err(wrap_error_msg(lexer, fdef.span(), "No return statement in nonvoid function")),
            }
        }
        Some(prim) => prim,
    };

    if *expected_type != ast::Prim::Void {
        if let Some(last_stmt_node) = block_node.data().last() {
            venv_enter_scope(&mut venv);
            let last_stmt_return_type = check_stmt(last_stmt_node, &mut venv, fenv, lexer)?;
            venv_exit_scope(&mut venv);

            if let None = last_stmt_return_type {
                return Err(wrap_error_msg(lexer, fdef.span(), "Last statement does not return"));
            }
        } else {
            return Err(wrap_error_msg(lexer, fdef.span(), "Empty nonvoid function"));
        }
    }

    if actual_type != *expected_type {
        let msg = format!("Wrong return type in: expected: {}, got: {}",
            expected_type, actual_type);
        Err(wrap_error_msg(lexer, fdef.span(), &msg))
    } else {
        Ok(())
    }
}

fn fn_env(fdefs: &Vec<ast::Node<ast::FunDef>>, lexer: &dyn Lexer<u32>) -> Result<FEnv, String> {
    let mut fenv: HashMap<ast::Ident, ast::FunType> = [
        ("printInt".to_string(), (ast::Prim::Void, vec![ast::Prim::Int])),
        ("printString".to_string(), (ast::Prim::Void, vec![ast::Prim::Str])),
        ("error".to_string(), (ast::Prim::Void, vec![])),
        ("readInt".to_string(), (ast::Prim::Int, vec![])),
        ("readString".to_string(), (ast::Prim::Str, vec![])),
    ].iter().cloned().collect();

    for fdef in fdefs.iter() {
        let (prim_node, ident_node, arg_nodes, _) = fdef.data();
        let arg_types = arg_nodes.iter().map(|arg_node| {
            match arg_node.data() {
                (prim_node, _) => prim_node.data().clone()
            }
        }).collect();

        let (fn_type, fn_name) = (prim_node.data(), ident_node.data());
        if let Some(_) = fenv.insert(fn_name.clone(), (fn_type.clone(), arg_types)) {
            return Err(wrap_error_msg(lexer, fdef.span(), "Function name not unique"))
        }
    }

    Ok(fenv)
}