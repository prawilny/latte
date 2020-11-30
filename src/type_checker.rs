// TODO: node() => data()
// TODO: statyczne sprawdzanie const booli
// ^: expr_int, expr_bool


use std::collections::HashMap;
use crate::latte_y as ast;
use crate::latte_y::IntType;
use crate::Span;
use ::lrpar::NonStreamingLexer as Lexer;

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
        let prev = self.vget(&key);
        self.last_mut().unwrap().insert(key, val);
        prev
    }

    fn vnew(&mut self) -> () {
        self.push(HashMap::new());
    }

    fn vdrop(&mut self) -> () {
        self.pop();
    }
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

pub fn check_types(fdefs: &Vec<ast::Node<ast::FunDef>>, lexer: &dyn Lexer<u32>) -> Result<(), String> {
    let env = fn_env(fdefs, lexer)?;

    for fdef in fdefs {
        check_fn(fdef, &env, lexer)?;
    }

    Ok(())
}

fn expr_bool(expr: &ast::Node<ast::Expr>, lexer: &dyn Lexer<u32>) -> Result<Option<bool>, String> {
    match expr.node() {
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
                    match expr.node() {
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
                    match expr.node() {
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
    match expr.node() {
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
                    match expr.node() {
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
                    match expr.node() {
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
    match expr.node() {
        ast::Expr::App(ident_node, expr_nodes) => {
            let (fun_type, fun_arg_types) = match fenv.get(ident_node.node()) {
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
            match venv.vget(ident_node.node()) {
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

fn check_block(stmts: &Vec<ast::Node<ast::Stmt>>, mut venv: &mut VEnv, fenv: &FEnv, lexer: &dyn Lexer<u32>) -> Result<Option<ast::Prim>, String> {
    venv.vnew();
    for stmt in stmts {
        check_stmt(&stmt, &mut venv, fenv, lexer)?;
    }
    let return_type = match stmts.last() {
        None => Some(ast::Prim::Void),
        Some(last_stmt_node) => match last_stmt_node.node() {
            ast::Stmt::VRet => Some(ast::Prim::Void),
            ast::Stmt::Ret(expr_node) => Some(check_expr(&expr_node, &mut venv, fenv, lexer)?),
            _ => None,
        },
    };
    venv.vdrop();
    Ok(return_type)
}

// TODO: typ na Result<Option<ast::Prim>, ()>, żeby sprawdzać typ returna
fn check_stmt(stmt: &ast::Node<ast::Stmt>, mut venv: &mut VEnv, fenv: &FEnv, lexer: &dyn Lexer<u32>) -> Result<(), String> {
    match stmt.node() {
        ast::Stmt::Empty => Ok(()),
        ast::Stmt::VRet => Ok(()),
        ast::Stmt::Block(block_node) => {
            check_block(block_node.node(), &mut venv, fenv, lexer)?;
            Ok(())
        }
        ast::Stmt::Decl(prim_node, item_nodes) => {
            let prim = prim_node.node();
            for item_node in item_nodes {
                let (key, val) = match item_node.node() {
                    ast::Item::NoInit(ident_node) => (ident_node.node().clone(), prim.clone()),
                    ast::Item::Init(ident_node, expr_node)
                        => (ident_node.node().clone(), check_expr(&expr_node, &mut venv, fenv, lexer)?),
                };
                venv.vinsert(key, val);
            }
            Ok(())
        },
        ast::Stmt::Asgn(ident_node, expr_node) => {
            let expr_prim = check_expr(&expr_node, &mut venv, fenv, lexer)?;
            match venv.vinsert(ident_node.node().clone(), expr_prim) {
                Some(_) => Ok(()),
                None => Err(undeclared_var_msg(lexer, ident_node.span())),
            }

        },
        ast::Stmt::Incr(ident_node) | ast::Stmt::Decr(ident_node) => {
            let ident = ident_node.node().clone();
            match venv.vget(&ident) {
                Some(ast::Prim::Int) => Ok(()),
                Some(prim) => Err(type_mismatch_msg(ast::Prim::Int, &prim, lexer, ident_node.span())),
                None => Err(undeclared_var_msg(lexer, ident_node.span()))
            }
        },
        ast::Stmt::Expr(expr_node) | ast::Stmt::Ret(expr_node) => {
            check_expr(&expr_node, &mut venv, fenv, lexer)?;
            Ok(())
        },
        ast::Stmt::If(expr_node, stmt_node) | ast::Stmt::While(expr_node, stmt_node) => {
            let expr_prim = check_expr(&expr_node, &mut venv, fenv, lexer)?;
            if expr_prim == ast::Prim::Bool {
                venv.vnew();
                check_stmt(&stmt_node, &mut venv, fenv, lexer)?;
                venv.vdrop();
                Ok(())
            } else {
                Err(type_mismatch_msg(ast::Prim::Bool, &expr_prim, lexer, expr_node.span()))
            }
        },
        ast::Stmt::IfElse(expr_node, stmt1_node, stmt2_node) => {
            let expr_prim = check_expr(&expr_node, &mut venv, fenv, lexer)?;
            if expr_prim == ast::Prim::Bool {
                venv.vnew();
                check_stmt(&stmt1_node, &mut venv, fenv, lexer)?;
                venv.vdrop();
                venv.vnew();
                check_stmt(&stmt2_node, &mut venv, fenv, lexer)?;
                venv.vdrop();
                Ok(())
            } else {
                Err(type_mismatch_msg(ast::Prim::Bool, &expr_prim, lexer, expr_node.span()))
            }
        },
    }
}

// w tym momencie żądam returna na końcu funkcji typu różnego niż void
fn check_fn(fdef: &ast::Node<ast::FunDef>, fenv: &FEnv, lexer: &dyn Lexer<u32>) -> Result<(), String> {
    let mut venv = VEnv::new();
    venv.vnew();

    let (prim_node, _, arg_nodes, block_node) = fdef.node();
    for arg_node in arg_nodes {
        let (arg_prim_node, arg_ident_node) = arg_node.node();
        let (prim, ident) = (arg_prim_node.node(), arg_ident_node.node());
        if let Some(_) = venv.vinsert(ident.clone(), prim.clone()) {
            return Err(wrap_error_msg(lexer, fdef.span(), "Argument name repeated"))
        }
    }

    let stmts = block_node.node();

    let expected_type = prim_node.node();
    let actual_type = match check_block(&stmts, &mut venv, fenv, lexer)? {
        None => {
            return Err(wrap_error_msg(lexer, fdef.span(), "No return statement in function"));
        }
        Some(prim) => prim,
    };

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
        let (prim_node, ident_node, arg_nodes, _) = fdef.node();
        let arg_types = arg_nodes.iter().map(|arg_node| {
            match arg_node.node() {
                (prim_node, _) => prim_node.node().clone()
            }
        }).collect();

        let (fn_type, fn_name) = (prim_node.node(), ident_node.node());
        if let Some(_) = fenv.insert(fn_name.clone(), (fn_type.clone(), arg_types)) {
            return Err(wrap_error_msg(lexer, fdef.span(), "Function name not unique"))
        }
    }

    Ok(fenv)
}