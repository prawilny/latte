// TODO: poprawianie wielu `match` (guardy)
// TODO?: przerobienie operacji na unarne/binarne z operatorem

use std::collections::HashMap;
use crate::latte_y as ast;
use crate::latte_y::IntType;
use crate::Span;
use ::lrpar::NonStreamingLexer as Lexer;

type VEnv = Vec<HashMap<ast::Ident, ast::Prim>>;
type FEnv = HashMap<ast::Ident, ast::FunType>;

fn venv_get_in_scope(venv: &VEnv, key: &ast::Ident) -> Option<ast::Prim> {
    match venv.last().unwrap().get(key) {
        Some(prim) => Some(prim.clone()),
        None => None,
    }
}

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

    format!("{} at {}:{}-{}:{} \nin\n{} \nin\n'{}'",
        msg, start_line, start_column, end_line, end_column,
        token, token_lines)
}

fn undeclared_var_msg(lexer: &dyn Lexer<u32>, span: &Span) -> String {
    wrap_error_msg(lexer, span, "use of undeclared variable")
}

fn wrong_return_msg(expected_type: ast::Prim, actual_type: &ast::Prim, lexer: &dyn Lexer<u32>, span: &Span) -> String {
    let msg = format!("wrong return type: expected {}, got: {}", expected_type, actual_type);
    wrap_error_msg(lexer, span, &msg)
}

fn type_mismatch_msg(expected_type: ast::Prim, actual_type: &ast::Prim, lexer: &dyn Lexer<u32>, span: &Span) -> String {
    let msg = format!("type mismatch: expected {}, got {}", expected_type, actual_type);
    wrap_error_msg(lexer, span, &msg)
}

fn wrong_operator_arguments(expected_types: &Vec<(ast::Prim, ast::Prim)>, actual_types: (ast::Prim, ast::Prim),
    lexer: &dyn Lexer<u32>, span: &Span) -> String {
    let msg = format!("wrong operator arguments: acceptable {:?}, got {:?}", expected_types, actual_types);
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
        ast::Expr::GEQ(expr1_node, expr2_node) => {
            match (expr_int(expr1_node, lexer)?, expr_int(expr2_node, lexer)?) {
                (Some(e1), Some(e2)) => {
                    match expr.data() {
                        ast::Expr::LTH(_, _) => Ok(Some(e1 < e2)),
                        ast::Expr::LEQ(_, _) => Ok(Some(e1 <= e2)),
                        ast::Expr::GTH(_, _) => Ok(Some(e1 > e2)),
                        ast::Expr::GEQ(_, _) => Ok(Some(e1 >= e2)),
                        _ => unreachable!(),
                    }
                },
                _ => Ok(None),
            }
        },
        ast::Expr::NEQ(expr1_node, expr2_node) |
        ast::Expr::EQ(expr1_node, expr2_node) => {
            match (expr_int(expr1_node, lexer)?, expr_int(expr2_node, lexer)?, expr_int(expr1_node, lexer)?, expr_int(expr2_node, lexer)?) {
                (Some(e1), Some(e2), _, _) => {
                    match expr.data() {
                        ast::Expr::NEQ(_, _) => Ok(Some(e1 != e2)),
                        ast::Expr::EQ(_, _) => Ok(Some(e1 == e2)),
                        _ => unreachable!(),
                    }
                },
                ( _, _, Some(b1), Some(b2)) => {
                    match expr.data() {
                        ast::Expr::NEQ(_, _) => Ok(Some(b1 != b2)),
                        ast::Expr::EQ(_, _) => Ok(Some(b1 == b2)),
                        _ => unreachable!(),
                    }
                }
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
                    Err(wrap_error_msg(lexer, expr2_node.span(), "div/mod by 0"))
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
        ast::Expr::Not(expr_node) => {
            match check_expr(expr_node, venv, fenv, lexer)? {
                ast::Prim::Bool => Ok(ast::Prim::Bool),
                prim => Err(type_mismatch_msg(ast::Prim::Bool, &prim, lexer, expr_node.span())),
            }
        },
        ast::Expr::Neg(expr_node) => {
            match check_expr(expr_node, venv, fenv, lexer)? {
                ast::Prim::Int => Ok(ast::Prim::Int),
                prim => Err(type_mismatch_msg(ast::Prim::Int, &prim, lexer, expr_node.span())),
            }
        },
        ast::Expr::And(expr1_node, expr2_node)|
        ast::Expr::Or(expr1_node, expr2_node) => {
            let acceptable_prims = vec![(ast::Prim::Bool, ast::Prim::Bool)];
            match (check_expr(expr1_node, venv, fenv, lexer)?, check_expr(expr2_node, venv, fenv, lexer)?)  {
                (ast::Prim::Bool, ast::Prim::Bool) => Ok(ast::Prim::Bool),
                (prim1, prim2) => Err(wrong_operator_arguments(&acceptable_prims, (prim1, prim2), lexer, expr.span())),
            }
        },
        ast::Expr::Add(expr1_node, expr2_node) => {
            let acceptable_prims = vec![
                (ast::Prim::Int, ast::Prim::Int),
                (ast::Prim::Str, ast::Prim::Str),
            ];
            match (check_expr(expr1_node, venv, fenv, lexer)?, check_expr(expr2_node, venv, fenv, lexer)?)  {
                (ast::Prim::Int, ast::Prim::Int) => Ok(ast::Prim::Int),
                (ast::Prim::Str, ast::Prim::Str) => Ok(ast::Prim::Str),
                (prim1, prim2) => Err(wrong_operator_arguments(&acceptable_prims, (prim1, prim2), lexer, expr.span())),
            }
        },
        ast::Expr::Div(expr1_node, expr2_node) |
        ast::Expr::Sub(expr1_node, expr2_node) |
        ast::Expr::Mod(expr1_node, expr2_node) |
        ast::Expr::Mul(expr1_node, expr2_node) => {
            let acceptable_prims = vec![(ast::Prim::Int, ast::Prim::Int)];
            match (check_expr(expr1_node, venv, fenv, lexer)?, check_expr(expr2_node, venv, fenv, lexer)?)  {
                (ast::Prim::Int, ast::Prim::Int) => Ok(ast::Prim::Int),
                (prim1, prim2) => Err(wrong_operator_arguments(&acceptable_prims, (prim1, prim2), lexer, expr.span())),
            }
        },
        ast::Expr::LTH(expr1_node, expr2_node) |
        ast::Expr::LEQ(expr1_node, expr2_node) |
        ast::Expr::GTH(expr1_node, expr2_node) |
        ast::Expr::GEQ(expr1_node, expr2_node) => {
            let acceptable_prims = vec![(ast::Prim::Bool, ast::Prim::Bool)];
            match (check_expr(expr1_node, venv, fenv, lexer)?, check_expr(expr2_node, venv, fenv, lexer)?)  {
                (ast::Prim::Int, ast::Prim::Int) => Ok(ast::Prim::Bool),
                (prim1, prim2) => Err(wrong_operator_arguments(&acceptable_prims, (prim1, prim2), lexer, expr.span())),
            }
        },
        ast::Expr::NEQ(expr1_node, expr2_node) |
        ast::Expr::EQ(expr1_node, expr2_node) => {
            let acceptable_prims = vec![
                (ast::Prim::Bool, ast::Prim::Bool),
                (ast::Prim::Int, ast::Prim::Int),
            ];
            match (check_expr(expr1_node, venv, fenv, lexer)?, check_expr(expr2_node, venv, fenv, lexer)?)  {
                (ast::Prim::Bool, ast::Prim::Bool) |
                (ast::Prim::Int, ast::Prim::Int) => Ok(ast::Prim::Bool),
                (prim1, prim2) => Err(wrong_operator_arguments(&acceptable_prims, (prim1, prim2), lexer, expr.span())),
            }
        },
    }
}

fn check_block(stmts: &Vec<ast::Node<ast::Stmt>>, fn_prim: &ast::Prim, mut venv: &mut VEnv, fenv: &FEnv, lexer: &dyn Lexer<u32>) -> Result<bool, String> {
    let mut block_returns = false;

    venv_enter_scope(venv);
    for stmt in stmts {
        block_returns = block_returns || check_stmt(&stmt, fn_prim, &mut venv, fenv, lexer)?;
    }
    venv_exit_scope(venv);

    Ok(block_returns)
}

fn check_stmt(stmt: &ast::Node<ast::Stmt>, fn_prim: &ast::Prim, mut venv: &mut VEnv, fenv: &FEnv, lexer: &dyn Lexer<u32>) -> Result<bool, String> {
    match stmt.data() {
        ast::Stmt::Empty => Ok(false),
        ast::Stmt::Expr(expr_node) => {
            check_expr(&expr_node, &mut venv, fenv, lexer)?;
            Ok(false)
        }
        ast::Stmt::Decl(prim_node, item_nodes) => {
            let decl_prim = prim_node.data();
            for item_node in item_nodes {
                let (var_name, var_prim) = match item_node.data() {
                    ast::Item::NoInit(ident_node) => (ident_node.data().clone(), decl_prim.clone()),
                    ast::Item::Init(ident_node, expr_node)
                        => (ident_node.data().clone(), check_expr(&expr_node, &mut venv, fenv, lexer)?),
                };
                if *decl_prim != var_prim {
                    return Err(type_mismatch_msg(decl_prim.clone(), &var_prim, lexer, item_node.span()))
                }
                if let Some(_) = venv_get_in_scope(venv, &var_name) {
                    return Err(wrap_error_msg(lexer, item_node.span(), "variable redeclared within block"))
                }
                venv_insert(venv, var_name, var_prim);
            }
            Ok(false)
        },
        ast::Stmt::Asgn(ident_node, expr_node) => {
            let expr_prim = check_expr(&expr_node, &mut venv, fenv, lexer)?;
            match venv_insert(venv, ident_node.data().clone(), expr_prim.clone()) {
                Some(var_prim) => {
                    if var_prim == expr_prim {
                        Ok(false)
                    } else {
                        Err(type_mismatch_msg(var_prim, &expr_prim, lexer, expr_node.span()))
                    }
                },
                None => Err(undeclared_var_msg(lexer, ident_node.span())),
            }
        },
        ast::Stmt::Incr(ident_node) | ast::Stmt::Decr(ident_node) => {
            let ident = ident_node.data().clone();
            match venv_get(venv, &ident) {
                Some(ast::Prim::Int) => Ok(false),
                Some(prim) => Err(type_mismatch_msg(ast::Prim::Int, &prim, lexer, ident_node.span())),
                None => Err(undeclared_var_msg(lexer, ident_node.span()))
            }
        },
        ast::Stmt::Ret(expr_node) => {
            let expr_prim = check_expr(&expr_node, &mut venv, fenv, lexer)?;
            if *fn_prim != expr_prim {
                Err(wrong_return_msg(fn_prim.clone(), &expr_prim, lexer, stmt.span()))
            } else {
                Ok(true)
            }
        }
        ast::Stmt::VRet => {
            if *fn_prim != ast::Prim::Void {
                Err(wrong_return_msg(fn_prim.clone(), &ast::Prim::Void, lexer, stmt.span()))
            } else {
                Ok(true)
            }
        }
        ast::Stmt::Block(block_node) => check_block(block_node.data(), fn_prim, &mut venv, fenv, lexer),
        ast::Stmt::If(expr_node, stmt_node) | ast::Stmt::While(expr_node, stmt_node) => {
            let expr_prim = check_expr(&expr_node, &mut venv, fenv, lexer)?;
            if expr_prim == ast::Prim::Bool {
                venv_enter_scope(venv);
                let stmt_returns = check_stmt(&stmt_node, fn_prim, &mut venv, fenv, lexer)?;
                venv_exit_scope(venv);
                if let Some(true) = expr_bool(expr_node, lexer)? {
                    Ok(stmt_returns)
                } else {
                    Ok(false)
                }
            } else {
                Err(type_mismatch_msg(ast::Prim::Bool, &expr_prim, lexer, expr_node.span()))
            }
        },
        ast::Stmt::IfElse(expr_node, stmt_true_node, stmt_false_node) => {
            let expr_prim = check_expr(&expr_node, &mut venv, fenv, lexer)?;
            if expr_prim == ast::Prim::Bool {
                venv_enter_scope(venv);
                let stmt_true_returns = check_stmt(&stmt_true_node, fn_prim, &mut venv, fenv, lexer)?;
                venv_exit_scope(venv);
                venv_enter_scope(venv);
                let stmt_false_returns = check_stmt(&stmt_false_node, fn_prim, &mut venv, fenv, lexer)?;
                venv_exit_scope(venv);
                match expr_bool(expr_node, lexer)? {
                    Some(true) => Ok(stmt_true_returns),
                    Some(false) => Ok(stmt_false_returns),
                    None => Ok(stmt_true_returns && stmt_false_returns),
                }
            } else {
                Err(type_mismatch_msg(ast::Prim::Bool, &expr_prim, lexer, expr_node.span()))
            }
        },
    }
}

fn check_fn(fdef: &ast::Node<ast::FunDef>, fenv: &FEnv, lexer: &dyn Lexer<u32>) -> Result<(), String> {
    let mut venv = VEnv::new();
    venv_enter_scope(&mut venv);

    let (prim_node, _, arg_nodes, block_node) = fdef.data();
    for arg_node in arg_nodes {
        let (arg_prim_node, arg_ident_node) = arg_node.data();
        let (prim, ident) = (arg_prim_node.data(), arg_ident_node.data());
        if let Some(_) = venv_insert(&mut venv, ident.clone(), prim.clone()) {
            return Err(wrap_error_msg(lexer, fdef.span(), "argument name repeated"))
        }
    }

    let fn_prim = prim_node.data();
    if !check_block(block_node.data(), fn_prim, &mut venv, fenv, lexer)?
        && *fn_prim != ast::Prim::Void {
        return Err(wrap_error_msg(lexer, fdef.span(), "nonvoid function does not return"))
    }

    Ok(())
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
            return Err(wrap_error_msg(lexer, fdef.span(), "function name not unique"))
        }
    }

    match fenv.get("main") {
        None => return Err("no main()".to_string()),
        Some((prim, args)) => {
            if *prim != ast::Prim::Int {
                return Err("wrong main() type".to_string())
            }
            if args.len() != 0 {
                return Err("main() has arguments".to_string())
            }
        }
    }

    Ok(fenv)
}