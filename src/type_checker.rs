// TODO: node() => data()

use std::collections::HashMap;
use crate::latte_y as ast;
use crate::Span;

type VEnv = Vec<HashMap<ast::Ident, ast::Prim>>;
type FEnv = HashMap<ast::Ident, ast::FunType>;

trait VarEnv {
    fn vget(&self, key: &ast::Ident) -> Option<ast::Prim>;
    fn vinsert(&mut self, key: ast::Ident, key: ast::Prim) -> Option<ast::Prim>;
    fn vnew(&mut self) -> ();
    fn vdrop(&mut self) -> ();
}

impl VarEnv for VEnv {
    fn vget(&self, key: &ast::Ident) -> Option<ast::Prim> {
        for venv in self.iter().rev() {
            if let Some(prim) = venv.get(key) {
                return Some(prim.clone());
            }
        }
        None
    }

    fn vinsert(&mut self, key: ast::Ident, val: ast::Prim) -> Option<ast::Prim> {
        self.last_mut().unwrap().insert(key, val)
    }

    fn vnew(&mut self) -> () {
        self.push(HashMap::new());
    }

    fn vdrop(&mut self) -> () {
        self.pop();
    }
}

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

pub fn undeclared_var_msg(source: &str, span: &Span) -> String {
    format!("use of undeclared variable {}", source_token(source, span))
}

pub fn type_mismatch_msg(expected_type: ast::Prim, actual_type: &ast::Prim, source: &str, span: &Span) -> String {
    format!("type mismatch at '{}': expected {}, got {}", source_token(source, span), expected_type, actual_type)
}

pub fn check_expr(expr: &ast::Node<ast::Expr>, mut venv: &mut VEnv, fenv: &FEnv, source: &str) -> Result<ast::Prim, String> {
    Ok(ast::Prim::Void)
}

pub fn check_block(stmts: &Vec<ast::Node<ast::Stmt>>, mut venv: &mut VEnv, fenv: &FEnv, source: &str) -> Result<Option<ast::Prim>, String> {
    venv.vnew();
    for stmt in stmts {
        check_stmt(&stmt, &mut venv, fenv, source)?;
    }
    let return_type = match stmts.last() {
        None => Some(ast::Prim::Void),
        Some(last_stmt_node) => match last_stmt_node.node() {
            ast::Stmt::VRet => Some(ast::Prim::Void),
            ast::Stmt::Ret(expr_node) => Some(check_expr(&expr_node, &mut venv, fenv, source)?),
            _ => None,
        },
    };
    venv.vdrop();
    Ok(return_type)
}

pub fn check_stmt(stmt: &ast::Node<ast::Stmt>, mut venv: &mut VEnv, fenv: &FEnv, source: &str) -> Result<(), String> {
    match stmt.node() {
        ast::Stmt::Empty => Ok(()),
        ast::Stmt::VRet => Ok(()),
        ast::Stmt::Block(block_node) => {
            check_block(block_node.node(), &mut venv, fenv, source)?;
            Ok(())
        }
        ast::Stmt::Decl(prim_node, item_nodes) => {
            let prim = prim_node.node();
            for item_node in item_nodes {
                let (key, val) = match item_node.node() {
                    ast::Item::NoInit(ident_node) => (ident_node.node().clone(), prim.clone()),
                    ast::Item::Init(ident_node, expr_node)
                        => (ident_node.node().clone(), check_expr(&expr_node, &mut venv, fenv, source)?),
                };
                venv.vinsert(key, val);
            }
            Ok(())
        },
        ast::Stmt::Asgn(ident_node, expr_node) => {
            let expr_prim = check_expr(&expr_node, &mut venv, fenv, source)?;
            match venv.vinsert(ident_node.node().clone(), expr_prim) {
                Some(_) => Ok(()),
                None => Err(undeclared_var_msg(source, ident_node.span())),
            }

        },
        ast::Stmt::Incr(ident_node) | ast::Stmt::Decr(ident_node) => {
            let ident = ident_node.node().clone();
            match venv.vget(&ident) {
                Some(ast::Prim::Int) => Ok(()),
                Some(prim) => Err(type_mismatch_msg(ast::Prim::Int, &prim, source, ident_node.span())),
                None => Err(undeclared_var_msg(source, ident_node.span()))
            }
        },
        ast::Stmt::Expr(expr_node) | ast::Stmt::Ret(expr_node) => {
            check_expr(&expr_node, &mut venv, fenv, source)?;
            Ok(())
        },
        ast::Stmt::If(expr_node, stmt_node) | ast::Stmt::While(expr_node, stmt_node) => {
            let expr_prim = check_expr(&expr_node, &mut venv, fenv, source)?;
            if expr_prim == ast::Prim::Bool {
                venv.vnew();
                check_stmt(&stmt_node, &mut venv, fenv, source)?;
                venv.vdrop();
                Ok(())
            } else {
                Err(type_mismatch_msg(ast::Prim::Bool, &expr_prim, source, expr_node.span()))
            }
        },
        ast::Stmt::IfElse(expr_node, stmt1_node, stmt2_node) => {
            let expr_prim = check_expr(&expr_node, &mut venv, fenv, source)?;
            if expr_prim == ast::Prim::Bool {
                venv.vnew();
                check_stmt(&stmt1_node, &mut venv, fenv, source)?;
                venv.vdrop();
                venv.vnew();
                check_stmt(&stmt2_node, &mut venv, fenv, source)?;
                venv.vdrop();
                Ok(())
            } else {
                Err(type_mismatch_msg(ast::Prim::Bool, &expr_prim, source, expr_node.span()))
            }
        },
    }
}

// w tym momencie żądam returna na końcu funkcji typu różnego niż void
pub fn check_fn(fdef: &ast::Node<ast::FunDef>, fenv: &FEnv, source: &str) -> Result<(), String> {
    let mut venv = VEnv::new();
    venv.vnew();

    let (prim_node, ident_node, arg_nodes, block_node) = fdef.node();
    for arg_node in arg_nodes {
        let (arg_prim_node, arg_ident_node) = arg_node.node();
        let (prim, ident) = (arg_prim_node.node(), arg_ident_node.node());
        if let Some(_) = venv.vinsert(ident.clone(), prim.clone()) {
            let msg = format!("Argument name {} repeated in function {}",
                ident, source_token(source, ident_node.span()));
            return Err(msg)
        }
    }

    let stmts = block_node.node();

    let expected_type = prim_node.node();
    let actual_type = match check_block(&stmts, &mut venv, fenv, source)? {
        None => {
            let msg = format!("No return statement in function {} of type {}",
                source_token(source, ident_node.span()), expected_type);
            return Err(msg);
        }
        Some(prim) => prim,
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