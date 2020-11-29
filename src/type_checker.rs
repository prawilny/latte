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

pub fn check_expr(stmt: &ast::Node<ast::Stmt>, mut venv: &mut VEnv, fenv: &FEnv, source: &str) -> Result<(), String> {
    Ok(())
}

pub fn check_stmt(stmt: &ast::Node<ast::Stmt>, mut venv: &mut VEnv, fenv: &FEnv, source: &str) -> Result<(), String> {
    Ok(())
}

// argument zdublowany lub przesłania funkcję
pub fn check_fn(fdef: &ast::Node<ast::FunDef>, fenv: &FEnv, source: &str) -> Result<(), String> {
    let mut venv = HashMap::new();

    match fdef.node() {
        (_, ident_node, arg_nodes, block_node) => {
            for arg_node in arg_nodes {
                match arg_node.node() {
                    (arg_prim_node, arg_ident_node) => {
                        match (arg_prim_node.node(), arg_ident_node.node()) {
                            (prim, ident) => {
                                match venv.insert(ident.clone(), prim.clone()) {
                                    None => (),
                                    Some(_) => {
                                        let msg = format!("Argument name {} repeated in function {}",
                                            ident, source_token(source, ident_node.span()));
                                        return Err(msg)
                                    }
                                }
                            }
                        }
                    }
                }
            }

            match block_node.node() {
                stmts =>  {
                    for stmt in stmts {
                        check_stmt(&stmt, &mut venv, fenv, source)?;
                    }
                    Ok(())
                },
            }
        }
    }
}

pub fn fn_env(fdefs: &Vec<ast::Node<ast::FunDef>>, source: &str) -> Result<FEnv, String> {
    let mut fenv = HashMap::new();

    for fdef in fdefs.iter() {
        match fdef.node() {
            (prim_node, ident_node, arg_nodes, _) => {
                let arg_types = arg_nodes.iter().map(|arg_node| {
                    match arg_node.node() {
                        (prim_node, _) => prim_node.node().clone()
                    }
                }).collect();

                match (prim_node.node(), ident_node.node()) {
                    (fn_type, fn_name) => {
                        match fenv.insert(fn_name.clone(), (fn_type.clone(), arg_types)) {
                            None => (),
                            Some(_) => {
                                let msg = format!("Function name {} not unique", fn_name);
                                return Err(msg)
                            }
                        }
                    }
                }
            }
        };
    }

    Ok(fenv)
}