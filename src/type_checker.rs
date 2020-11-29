// TODO: node() => data()

use std::collections::HashMap;
use crate::latte_y as ast;
use crate::Span;

type VEnv = HashMap<ast::Ident, ast::Prim>;
type FEnv = HashMap<ast::Ident, ast::FunType>;

pub fn source_token(source: &str, span: &Span) -> String {
    source.get(span.start()..span.end()).unwrap().to_string()
}

pub fn check_types(fdefs: &Vec<ast::Node<ast::FunDef>>, source: &str) -> Result<(), String> {
    let env = fn_env(fdefs, source)?;

    for fdef in fdefs {
        check_fn(fdef, &env, source)?;
    }

    Ok(())
}

pub fn check_expr(expr: &ast::Node<ast::Expr>, mut venv: &mut VEnv, fenv: &FEnv, source: &str) -> Result<ast::Prim, String> {
    Ok(ast::Prim::Void)
}

pub fn check_stmts(stmts: &Vec<ast::Node<ast::Stmt>>, mut venv: &mut VEnv, fenv: &FEnv, source: &str) -> Result<(), String> {
    for stmt in stmts {
        check_stmt(&stmt, &mut venv, fenv, source)?;
    }
    Ok(())
}

pub fn check_stmt(stmt: &ast::Node<ast::Stmt>, mut venv: &mut VEnv, fenv: &FEnv, source: &str) -> Result<(), String> {
    match stmt.node() {
        ast::Stmt::Block(block_node) => {
            check_stmts(block_node.node(), &mut venv, fenv, source)
        },
        ast::Stmt::Decl(prim_node, item_nodes) => {
            Ok(())
        },
        ast::Stmt::Asgn(ident_node, expr_node) => {
            Ok(())
        },
        ast::Stmt::Incr(ident_node) => {
            Ok(())
        },
        ast::Stmt::Decr(ident_node) => {
            Ok(())
        },
        ast::Stmt::Ret(expr_node) => {
            Ok(())
        },
        ast::Stmt::VRet => {
            Ok(())
        },
        ast::Stmt::If(expr_node, stmt_node) => {
            Ok(())
        },
        ast::Stmt::IfElse(expr_node, stmt1_nodem, stmt2_node) => {
            Ok(())
        },
        ast::Stmt::While(expr_node, stmt_node) => {
            Ok(())
        },
        ast::Stmt::Expr(expr_node) => {
            Ok(())
        },
        ast::Stmt::Empty => Ok(()),
    }
}

// argument zdublowany lub przesłania funkcję
pub fn check_fn(fdef: &ast::Node<ast::FunDef>, fenv: &FEnv, source: &str) -> Result<(), String> {
    let mut venv = HashMap::new();

    let (prim_node, ident_node, arg_nodes, block_node) = fdef.node();
    for arg_node in arg_nodes {
        let (arg_prim_node, arg_ident_node) = arg_node.node();
        let (prim, ident) = (arg_prim_node.node(), arg_ident_node.node());
        if let Some(_) = venv.insert(ident.clone(), prim.clone()) {
            let msg = format!("Argument name {} repeated in function {}",
                ident, source_token(source, ident_node.span()));
            return Err(msg)
        }
    }

    let stmts = block_node.node();
    check_stmts(&stmts, &mut venv, fenv, source)?;

    let expected_type = prim_node.node();
    let actual_type = match stmts.last() {
        None => ast::Prim::Void,
        Some(last_stmt_node) => match last_stmt_node.node() {
            ast::Stmt::VRet => ast::Prim::Void,
            ast::Stmt::Ret(expr_node) => {
                check_expr(&expr_node, &mut venv, fenv, source)?
            },
            _ => {
                let msg = format!("No return statement in function {} of type {}",
                    source_token(source, ident_node.span()), expected_type);
                return Err(msg);
            }
        },
    };

    if actual_type != *expected_type {
        let msg = format!("Wrong return type in '{}': expected: {}, got: {}",
            source_token(source, ident_node.span()), expected_type, actual_type);
        Err(msg)
    } else {
        Ok(())
    }
}

pub fn fn_env(fdefs: &Vec<ast::Node<ast::FunDef>>, source: &str) -> Result<FEnv, String> {
    let mut fenv = HashMap::new();

    for fdef in fdefs.iter() {
        let (prim_node, ident_node, arg_nodes, _) = fdef.node();
        let arg_types = arg_nodes.iter().map(|arg_node| {
            match arg_node.node() {
                (prim_node, _) => prim_node.node().clone()
            }
        }).collect();

        let (fn_type, fn_name) = (prim_node.node(), ident_node.node());
        if let Some(_) = fenv.insert(fn_name.clone(), (fn_type.clone(), arg_types)) {
            let msg = format!("Function name {} not unique", fn_name);
            return Err(msg)
        }
    }

    Ok(fenv)
}