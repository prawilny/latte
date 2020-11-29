use std::collections::HashMap;
use crate::latte_y as ast;
use crate::Span;

type Env = HashMap<ast::Ident, ast::FunType>;

pub fn source_token(source: &str, span: &Span) -> String {
    source.get(span.start()..span.end()).unwrap().to_string()
}

pub fn check_types(fdefs: &Vec<ast::Node<ast::FunDef>>, source: &str) -> Result<(), String> {
    let env = fn_env(fdefs, source)?;

    for fdef in fdefs {
        check_function(fdef, env.clone(), source)?;
    }

    Ok(())
}

// argument zdublowany lub przesłania funkcję
pub fn check_function(fdef: &ast::Node<ast::FunDef>, env: Env, source: &str) -> Result<(), String> {
    Ok(())
}

pub fn fn_env(fdefs: &Vec<ast::Node<ast::FunDef>>, source: &str) -> Result<Env, String> {
    let mut env = HashMap::new();

    // for def in fdefs.iter() {
    //     match def.node() {
    //         (type_node, ident_node, arg_nodes, _) => {
    //             let mut arg_types = Vec::new();

    //             for arg_node in arg_nodes {
    //                 match arg_node.node() {
    //                     (arg_type_node, arg_ident_node) => match arg_type_node.node(){
    //                         ast::Type::VType(prim) => {
    //                             arg_types.push(prim.clone());
    //                         },
    //                         ast::Type::FType(_, _) => {
    //                             let msg = format!("Nonprimitive arg {} of type {}",
    //                                 source_token(source, arg_ident_node.span()),
    //                                 source_token(source, arg_type_node.span()));
    //                             return Err(msg)
    //                         },
    //                     }
    //                 }
    //             }

    //             match (type_node.node(), ident_node.node()) {
    //                 (fn_type, fn_ident) => {
    //                     let fn_prim = match fn_type {
    //                         ast::Type::VType(prim) => prim.clone(),
    //                         ast::Type::FType(_, _) => {
    //                             let msg = format!("Nonprimitive type {} of function {}",
    //                                 source_token(source, type_node.span()),
    //                                 source_token(source, ident_node.span()));
    //                             return Err(msg)
    //                         },
    //                     };

    //                     match env.insert(fn_ident.clone(), ast::Type::FType(fn_prim, arg_types)) {
    //                         None => (),
    //                         Some(_) => {
    //                             let msg = format!("Function name {} not unique", fn_ident);
    //                             return Err(msg)
    //                         }
    //                     }
    //                 }
    //             }
    //         }
    //     }

    // }

    Ok(env)
}