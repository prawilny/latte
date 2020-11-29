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
        check_function(fdef, &env, source)?;
    }

    Ok(())
}

// argument zdublowany lub przesłania funkcję
pub fn check_function(fdef: &ast::Node<ast::FunDef>, fenv: &FEnv, source: &str) -> Result<(), String> {
    let mut venv = HashMap::new();

    Ok(())
}

pub fn fn_env(fdefs: &Vec<ast::Node<ast::FunDef>>, source: &str) -> Result<FEnv, String> {
    let mut fenv = HashMap::new();

    for fdef in fdefs.iter() {
        match fdef.node() {
            (prim_node, ident_node, arg_nodes, _) => {
                let mut arg_types = Vec::new();

                let arg_types = arg_nodes.iter().map(|arg_node| {
                    arg_node.node();
                }).collect();

                match (prim_node.node(), ident_node.node()) {
                    (fn_type, fn_name) => {
                        match env.insert(fn_name.clone(), (fn_type, arg_types)) {
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