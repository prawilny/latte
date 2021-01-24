// TODO: sprawdzić "_", unimplemented!, unreachable!
// TODO: upewnić się, że dodanie obiektów nie wymaga zmiany niczego więcej
// TODO: priorytet '.'
// TODO: sprawdzenie gramatyki
// TODO: sprawdzenie uwagi o castach w README
// TODO: wypełnienie typów nowych wyrażeń
// TODO: wypełnianie typów wyrażeń w instrukcjach
// TODO: zmiana Class("C") na cokolwiek innego
// TODO: czy obsługa self jest poprawna?
// TODO: któryś unwrap() się wywala
// TODO: README: null tylko typowany
// TODO: wykryć cykliczne dziedziczenie
// TODO: wykryć dziedziczenie po nieistniejącym
// TODO: upewnić się, że wszystkie wyrażenia mają ustawione primy
// TODO: set_prim wszystkim
// TODO: czy typ zmiennym dać? chyba jest już przechowywany w VEnv...

use crate::latte_y as ast;
use crate::latte_y::IntType;
use crate::Span;
use ::lrpar::NonStreamingLexer as Lexer;
use std::collections::{HashMap, HashSet};

type CMembers = HashMap<ast::Ident, ast::Type>;
type CEnv = HashMap<ast::Ident, CMembers>;
type FEnv = HashMap<ast::Ident, ast::FunType>;
type VEnv = Vec<HashMap<ast::Ident, ast::Prim>>;
type IEnv = HashMap<ast::Ident, HashSet<ast::Ident>>;
type CFIEnv = (CEnv, FEnv, IEnv);

use ast::SELF_IDENT;

fn is_subclass(child: &ast::Ident, parent: &ast::Ident, ienv: &IEnv) -> bool {
    match ienv.get(child) {
        None => false,
        Some(superclasses) => superclasses.get(parent).is_some(),
    }
}

fn cmp_prims(expected: &ast::Prim, actual: &ast::Prim, ienv: &IEnv) -> bool {
    match (expected, actual) {
        (ast::Prim::Int, ast::Prim::Int) => true,
        (ast::Prim::Str, ast::Prim::Str) => true,
        (ast::Prim::Bool, ast::Prim::Bool) => true,
        (ast::Prim::Void, ast::Prim::Void) => true,
        (ast::Prim::Class(c1), ast::Prim::Class(c2)) if c1 == c2 => true,
        (ast::Prim::Class(c1), ast::Prim::Class(c2)) if is_subclass(&c2, &c1, ienv) => true,
        _ => false,
    }
}

fn cmp_types(expected: &ast::Type, actual: &ast::Type, ienv: &IEnv) -> bool {
    match (expected, actual) {
        (ast::Type::Var(exp_prim), ast::Type::Var(act_prim)) => cmp_prims(exp_prim, act_prim, ienv),
        (ast::Type::Fun((exp_prim, exp_arg_prims)), ast::Type::Fun((act_prim, act_arg_prims))) => {
            cmp_prims(act_prim, exp_prim, ienv) && {
                // odwrotne porównanie - możemy zwrócić podklasę
                if exp_arg_prims.len() == act_arg_prims.len() {
                    let cmps: Vec<bool> = exp_arg_prims[1..] // obcinamy self
                        .iter()
                        .zip(act_arg_prims[1..].iter()) // obcinamy self
                        .map(|(exp_arg_prim, act_arg_prim)| cmp_prims(exp_arg_prim, act_arg_prim, ienv))
                        .collect();
                    cmps.iter().all(|b| *b)
                } else {
                    false
                }
            }
        }
        _ => false,
    }
}

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

fn venv_enter_scope(venv: &mut VEnv) {
    venv.push(HashMap::new());
}

fn venv_exit_scope(venv: &mut VEnv) {
    venv.pop();
}

fn wrap_error_msg(lexer: &dyn Lexer<u32>, span: &Span, msg: &str) -> String {
    let offender = lexer.span_str(*span).trim();
    let context = lexer.span_lines_str(*span).trim();
    let ((start_line, start_column), (end_line, end_column)) = lexer.line_col(*span);

    let wrappepd = format!(
        "{} at {}:{}-{}:{}\nin\n'{}'",
        msg, start_line, start_column, end_line, end_column, offender
    );

    if offender.len() < context.len() {
        format!("{}\nin\n'{}'", wrappepd, context)
    } else {
        wrappepd
    }
}

fn undeclared_var_msg(lexer: &dyn Lexer<u32>, span: &Span) -> String {
    wrap_error_msg(lexer, span, "use of undeclared variable")
}

fn no_such_msg(lexer: &dyn Lexer<u32>, span: &Span, entity: &str) -> String {
    let msg = format!("such {} does not exist", entity);
    wrap_error_msg(lexer, span, &msg)
}

fn wrong_return_msg(expected_type: ast::Prim, actual_type: &ast::Prim, lexer: &dyn Lexer<u32>, span: &Span) -> String {
    let msg = format!("wrong return type: expected {}, got {}", expected_type, actual_type);
    wrap_error_msg(lexer, span, &msg)
}

fn type_mismatch_msg(expected_type: ast::Prim, actual_type: &ast::Prim, lexer: &dyn Lexer<u32>, span: &Span) -> String {
    let msg = format!("type mismatch: expected {}, got {}", expected_type, actual_type);
    wrap_error_msg(lexer, span, &msg)
}

fn wrong_operator_arguments(
    expected_types: &Vec<(ast::Prim, ast::Prim)>,
    actual_types: (ast::Prim, ast::Prim),
    lexer: &dyn Lexer<u32>,
    span: &Span,
) -> String {
    let msg = format!(
        "wrong operator arguments: acceptable {:?}, got {:?}",
        expected_types, actual_types
    );
    wrap_error_msg(lexer, span, &msg)
}

fn method_venv(self_name_node: &ast::Node<ast::Ident>, cfienv: &CFIEnv, lexer: &dyn Lexer<u32>) -> Result<VEnv, String> {
    let members_map = match cfienv.0.get(self_name_node.data()) {
        Some(members_map) => members_map,
        None => return Err(wrap_error_msg(lexer, self_name_node.span(), "nonexistent class")),
    };

    let mut class_venv = VEnv::new();
    venv_enter_scope(&mut class_venv);
    venv_insert(
        &mut class_venv,
        SELF_IDENT.to_string(),
        ast::Prim::Class(self_name_node.data().to_string()),
    );
    for (member_name, member) in members_map {
        if let ast::Type::Var(prim) = member {
            venv_insert(&mut class_venv, member_name.to_string(), prim.clone());
        }
    }
    Ok(class_venv)
}

pub fn check_types(
    cfdefs: &(Vec<ast::Node<ast::ClassDef>>, Vec<ast::Node<ast::FunDef>>),
    lexer: &dyn Lexer<u32>,
) -> Result<(), String> {
    let (cenv, ienv) = class_env(&cfdefs.0, lexer)?;
    let fenv = fn_env(&cfdefs.1, lexer)?;
    let cfienv = (cenv, fenv, ienv);

    for fdef in &cfdefs.1 {
        let mut venv = VEnv::new();
        venv_enter_scope(&mut venv);

        check_fn(fdef, &mut venv, &cfienv, lexer)?;
    }

    for cdef in &cfdefs.0 {
        let (class_name_node, _, _, method_nodes) = cdef.data();
        let class_venv = method_venv(class_name_node, &cfienv, lexer)?;

        for method_node in method_nodes {
            check_fn(method_node, &mut class_venv.clone(), &cfienv, lexer)?;
        }
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
        }
        ast::Expr::And(expr1_node, expr2_node) => match (expr_bool(expr1_node, lexer)?, expr_bool(expr2_node, lexer)?) {
            (Some(b1), Some(b2)) => Ok(Some(b1 && b2)),
            _ => Ok(None),
        },
        ast::Expr::Or(expr1_node, expr2_node) => match (expr_bool(expr1_node, lexer)?, expr_bool(expr2_node, lexer)?) {
            (Some(b1), Some(b2)) => Ok(Some(b1 || b2)),
            (Some(b), None) | (None, Some(b)) => Ok(Some(b)),
            _ => Ok(None),
        },
        ast::Expr::LTH(expr1_node, expr2_node)
        | ast::Expr::LEQ(expr1_node, expr2_node)
        | ast::Expr::GTH(expr1_node, expr2_node)
        | ast::Expr::GEQ(expr1_node, expr2_node) => match (expr_int(expr1_node, lexer)?, expr_int(expr2_node, lexer)?) {
            (Some(e1), Some(e2)) => match expr.data() {
                ast::Expr::LTH(_, _) => Ok(Some(e1 < e2)),
                ast::Expr::LEQ(_, _) => Ok(Some(e1 <= e2)),
                ast::Expr::GTH(_, _) => Ok(Some(e1 > e2)),
                ast::Expr::GEQ(_, _) => Ok(Some(e1 >= e2)),
                _ => unreachable!(),
            },
            _ => Ok(None),
        },
        ast::Expr::NEQ(expr1_node, expr2_node) | ast::Expr::EQ(expr1_node, expr2_node) => match (
            expr_int(expr1_node, lexer)?,
            expr_int(expr2_node, lexer)?,
            expr_bool(expr1_node, lexer)?,
            expr_bool(expr2_node, lexer)?,
        ) {
            (Some(e1), Some(e2), _, _) => match expr.data() {
                ast::Expr::NEQ(_, _) => Ok(Some(e1 != e2)),
                ast::Expr::EQ(_, _) => Ok(Some(e1 == e2)),
                _ => unreachable!(),
            },
            (_, _, Some(b1), Some(b2)) => match expr.data() {
                ast::Expr::NEQ(_, _) => Ok(Some(b1 != b2)),
                ast::Expr::EQ(_, _) => Ok(Some(b1 == b2)),
                _ => unreachable!(),
            },
            _ => Ok(None),
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
        }
        ast::Expr::Add(expr1_node, expr2_node)
        | ast::Expr::Sub(expr1_node, expr2_node)
        | ast::Expr::Mul(expr1_node, expr2_node) => match (expr_int(expr1_node, lexer)?, expr_int(expr2_node, lexer)?) {
            (Some(e1), Some(e2)) => match expr.data() {
                ast::Expr::Add(_, _) => Ok(Some(e1 + e2)),
                ast::Expr::Sub(_, _) => Ok(Some(e1 - e2)),
                ast::Expr::Mul(_, _) => Ok(Some(e1 * e2)),
                _ => unreachable!(),
            },
            _ => Ok(None),
        },
        ast::Expr::Div(expr1_node, expr2_node) | ast::Expr::Mod(expr1_node, expr2_node) => {
            match (expr_int(expr1_node, lexer)?, expr_int(expr2_node, lexer)?) {
                (_, Some(0)) => Err(wrap_error_msg(lexer, expr2_node.span(), "div/mod by 0")),
                (Some(e1), Some(e2)) => match expr.data() {
                    ast::Expr::Div(_, _) => Ok(Some(e1 / e2)),
                    ast::Expr::Mod(_, _) => Ok(Some(e1 % e2)),
                    _ => unreachable!(),
                },
                _ => Ok(None),
            }
        }
        _ => Ok(None),
    }
}

fn check_call(
    fun: &ast::FunType,
    ident_node: &ast::Node<ast::Ident>,
    arg_nodes: &Vec<ast::Node<ast::Expr>>,
    venv: &VEnv,
    cfienv: &CFIEnv,
    lexer: &dyn Lexer<u32>,
) -> Result<ast::Prim, String> {
    let (fun_type, fun_arg_types) = fun;

    if fun_arg_types.len() != arg_nodes.len() {
        let msg = format!(
            "use of {}-argument function or method with {} arguments",
            fun_arg_types.len(),
            arg_nodes.len()
        );
        return Err(wrap_error_msg(lexer, ident_node.span(), &msg));
    }

    for (arg_prim, expr_node) in fun_arg_types.iter().zip(arg_nodes.iter()) {
        let expr_prim = check_expr(expr_node, venv, cfienv, lexer)?;
        if !cmp_prims(arg_prim, &expr_prim, &cfienv.2) {
            return Err(type_mismatch_msg(arg_prim.clone(), &expr_prim, lexer, expr_node.span()));
        }
    }

    Ok(fun_type.clone())
}

fn check_expr(expr: &ast::Node<ast::Expr>, venv: &VEnv, cfienv: &CFIEnv, lexer: &dyn Lexer<u32>) -> Result<ast::Prim, String> {
    let expr_prim = match expr.data() {
        ast::Expr::Dot(expr_node, ident_node) => match check_expr(expr_node, venv, cfienv, lexer)? {
            ast::Prim::Class(class_name) => {
                let members = match cfienv.0.get(&class_name) {
                    None => return Err(no_such_msg(lexer, ident_node.span(), "class")),
                    Some(members) => members,
                };
                match members.get(ident_node.data()) {
                    None => return Err(no_such_msg(lexer, ident_node.span(), "field")),
                    Some(ast::Type::Var(prim)) => prim.clone(),
                    Some(ast::Type::Fun(_)) => {
                        return Err(wrap_error_msg(lexer, expr_node.span(), "right side of . is a method"))
                    }
                }
            }
            non_class_prim => {
                return Err(wrap_error_msg(
                    lexer,
                    expr_node.span(),
                    &format!("left side of . is a {:?}", non_class_prim),
                ))
            }
        },
        ast::Expr::Mthd(expr_node, ident_node, arg_nodes) => {
            match check_expr(expr_node, venv, cfienv, lexer)? {
                ast::Prim::Class(class_name) => {
                    let members = match cfienv.0.get(&class_name) {
                        None => return Err(no_such_msg(lexer, ident_node.span(), "class")),
                        Some(members) => members,
                    };
                    match members.get(ident_node.data()) {
                        None => return Err(no_such_msg(lexer, ident_node.span(), "member")),
                        Some(ast::Type::Var(_)) => {
                            return Err(wrap_error_msg(lexer, expr_node.span(), "right side of . is a field"))
                        }
                        Some(ast::Type::Fun(fun_type)) => {
                            let mut arg_nodes_with_self = arg_nodes.clone();
                            arg_nodes_with_self.insert(0, *(expr_node.clone())); // this
                            check_call(fun_type, ident_node, &arg_nodes_with_self, venv, cfienv, lexer)?
                        }
                    }
                }
                non_class_prim => {
                    return Err(wrap_error_msg(
                        lexer,
                        expr_node.span(),
                        &format!("left side of . is a {:?}", non_class_prim),
                    ))
                }
            }
        }
        ast::Expr::Fun(ident_node, arg_nodes) => {
            let fun_type = match cfienv.1.get(ident_node.data()) {
                None => return Err(wrap_error_msg(lexer, ident_node.span(), "use of undeclared function")),
                Some(ft) => ft,
            };
            check_call(fun_type, ident_node, arg_nodes, venv, cfienv, lexer)?
        }
        ast::Expr::Var(ident_node) => match venv_get(venv, ident_node.data()) {
            Some(prim) => prim,
            None => return Err(undeclared_var_msg(lexer, ident_node.span())),
        },
        ast::Expr::Int(_) => ast::Prim::Int,
        ast::Expr::Bool(_) => ast::Prim::Bool,
        ast::Expr::Str(_) => ast::Prim::Str,
        ast::Expr::Not(expr_node) => match check_expr(expr_node, venv, cfienv, lexer)? {
            ast::Prim::Bool => ast::Prim::Bool,
            prim => return Err(type_mismatch_msg(ast::Prim::Bool, &prim, lexer, expr_node.span())),
        },
        ast::Expr::Neg(expr_node) => match check_expr(expr_node, venv, cfienv, lexer)? {
            ast::Prim::Int => ast::Prim::Int,
            prim => return Err(type_mismatch_msg(ast::Prim::Int, &prim, lexer, expr_node.span())),
        },
        ast::Expr::And(expr1_node, expr2_node) | ast::Expr::Or(expr1_node, expr2_node) => {
            let acceptable_prims = vec![(ast::Prim::Bool, ast::Prim::Bool)];
            match (
                check_expr(expr1_node, venv, cfienv, lexer)?,
                check_expr(expr2_node, venv, cfienv, lexer)?,
            ) {
                (ast::Prim::Bool, ast::Prim::Bool) => ast::Prim::Bool,
                (prim1, prim2) => {
                    return Err(wrong_operator_arguments(
                        &acceptable_prims,
                        (prim1, prim2),
                        lexer,
                        expr.span(),
                    ))
                }
            }
        }
        ast::Expr::Add(expr1_node, expr2_node) => {
            let acceptable_prims = vec![(ast::Prim::Int, ast::Prim::Int), (ast::Prim::Str, ast::Prim::Str)];
            match (
                check_expr(expr1_node, venv, cfienv, lexer)?,
                check_expr(expr2_node, venv, cfienv, lexer)?,
            ) {
                (ast::Prim::Int, ast::Prim::Int) => ast::Prim::Int,
                (ast::Prim::Str, ast::Prim::Str) => ast::Prim::Str,
                (prim1, prim2) => {
                    return Err(wrong_operator_arguments(
                        &acceptable_prims,
                        (prim1, prim2),
                        lexer,
                        expr.span(),
                    ))
                }
            }
        }
        ast::Expr::Div(expr1_node, expr2_node)
        | ast::Expr::Sub(expr1_node, expr2_node)
        | ast::Expr::Mod(expr1_node, expr2_node)
        | ast::Expr::Mul(expr1_node, expr2_node) => {
            let acceptable_prims = vec![(ast::Prim::Int, ast::Prim::Int)];
            match (
                check_expr(expr1_node, venv, cfienv, lexer)?,
                check_expr(expr2_node, venv, cfienv, lexer)?,
            ) {
                (ast::Prim::Int, ast::Prim::Int) => ast::Prim::Int,
                (prim1, prim2) => {
                    return Err(wrong_operator_arguments(
                        &acceptable_prims,
                        (prim1, prim2),
                        lexer,
                        expr.span(),
                    ))
                }
            }
        }
        ast::Expr::LTH(expr1_node, expr2_node)
        | ast::Expr::LEQ(expr1_node, expr2_node)
        | ast::Expr::GTH(expr1_node, expr2_node)
        | ast::Expr::GEQ(expr1_node, expr2_node) => {
            let acceptable_prims = vec![(ast::Prim::Int, ast::Prim::Int)];
            match (
                check_expr(expr1_node, venv, cfienv, lexer)?,
                check_expr(expr2_node, venv, cfienv, lexer)?,
            ) {
                (ast::Prim::Int, ast::Prim::Int) => ast::Prim::Bool,
                (prim1, prim2) => {
                    return Err(wrong_operator_arguments(
                        &acceptable_prims,
                        (prim1, prim2),
                        lexer,
                        expr.span(),
                    ))
                }
            }
        }
        ast::Expr::NEQ(expr1_node, expr2_node) | ast::Expr::EQ(expr1_node, expr2_node) => {
            let acceptable_prims = vec![
                (ast::Prim::Bool, ast::Prim::Bool),
                (ast::Prim::Int, ast::Prim::Int),
                (ast::Prim::Class("C".to_string()), ast::Prim::Class("C".to_string())),
            ];
            match (
                check_expr(expr1_node, venv, cfienv, lexer)?,
                check_expr(expr2_node, venv, cfienv, lexer)?,
            ) {
                (ast::Prim::Class(c1), ast::Prim::Class(c2))
                    if c1 == c2 || is_subclass(&c1, &c2, &cfienv.2) || is_subclass(&c2, &c1, &cfienv.2) =>
                {
                    ast::Prim::Bool
                }
                (ast::Prim::Bool, ast::Prim::Bool) | (ast::Prim::Int, ast::Prim::Int) => ast::Prim::Bool,
                (prim1, prim2) => {
                    return Err(wrong_operator_arguments(
                        &acceptable_prims,
                        (prim1, prim2),
                        lexer,
                        expr.span(),
                    ))
                }
            }
        }
        ast::Expr::New(ident_node) | ast::Expr::Null(ident_node) => ast::Prim::Class(ident_node.data().clone()),
    };
    expr.set_prim(&expr_prim);
    Ok(expr_prim)
}

fn check_block(
    stmts: &Vec<ast::Node<ast::Stmt>>,
    fn_prim: &ast::Prim,
    mut venv: &mut VEnv,
    cfienv: &CFIEnv,
    lexer: &dyn Lexer<u32>,
) -> Result<bool, String> {
    let mut block_returns = false;

    venv_enter_scope(venv);
    for stmt in stmts {
        block_returns = check_stmt(&stmt, fn_prim, &mut venv, cfienv, lexer)? || block_returns;
    }
    venv_exit_scope(venv);

    Ok(block_returns)
}

fn check_stmt(
    stmt: &ast::Node<ast::Stmt>,
    fn_prim: &ast::Prim,
    mut venv: &mut VEnv,
    cfienv: &CFIEnv,
    lexer: &dyn Lexer<u32>,
) -> Result<bool, String> {
    match stmt.data() {
        ast::Stmt::Empty => Ok(false),
        ast::Stmt::Expr(expr_node) => {
            check_expr(&expr_node, &venv, cfienv, lexer)?;
            Ok(false)
        }
        ast::Stmt::Decl(prim_node, item_nodes) => {
            let decl_prim = prim_node.data();
            if let ast::Prim::Void = decl_prim {
                return Err(wrap_error_msg(lexer, stmt.span(), "declaration of void variables"));
            }
            for item_node in item_nodes {
                let (var_name, var_prim) = match item_node.data() {
                    ast::Item::NoInit(ident_node) => (ident_node.data().clone(), decl_prim.clone()),
                    ast::Item::Init(ident_node, expr_node) => {
                        (ident_node.data().clone(), check_expr(&expr_node, &venv, cfienv, lexer)?)
                    }
                };
                if !cmp_prims(&decl_prim, &var_prim, &cfienv.2) {
                    return Err(type_mismatch_msg(decl_prim.clone(), &var_prim, lexer, item_node.span()));
                }
                if venv_get_in_scope(venv, &var_name).is_some() {
                    return Err(wrap_error_msg(lexer, item_node.span(), "variable redeclared within a block"));
                }
                venv_insert(venv, var_name, decl_prim.clone());
            }
            Ok(false)
        }
        ast::Stmt::Asgn(lhs_node, expr_node) => {
            let expr_prim = check_expr(&expr_node, &venv, cfienv, lexer)?;
            match lhs_node.data() {
                ast::Expr::Var(ident_node) => match venv_get(venv, ident_node.data()) {
                    Some(var_prim) => {
                        lhs_node.set_prim(&var_prim);
                        if cmp_prims(&var_prim, &expr_prim, &cfienv.2) {
                            Ok(false)
                        } else {
                            Err(type_mismatch_msg(var_prim, &expr_prim, lexer, expr_node.span()))
                        }
                    }
                    None => Err(undeclared_var_msg(lexer, ident_node.span())),
                },
                ast::Expr::Dot(dot_lhs_node, field_name_node) => {
                    let field_name = field_name_node.data();
                    match check_expr(dot_lhs_node, venv, cfienv, lexer)? {
                        ast::Prim::Class(class_name) => {
                            let members_map = match cfienv.0.get(&class_name) {
                                Some(members_map) => members_map,
                                None => {
                                    return Err(wrap_error_msg(lexer, dot_lhs_node.span(), "cannot find class' members"));
                                }
                            };
                            match members_map.get(field_name) {
                                Some(ast::Type::Var(field_prim)) if cmp_prims(field_prim, &expr_prim, &cfienv.2) => {
                                    lhs_node.set_prim(field_prim);
                                    Ok(false)
                                }
                                Some(ast::Type::Var(field_prim)) => {
                                    return Err(type_mismatch_msg(field_prim.clone(), &expr_prim, lexer, expr_node.span()));
                                }
                                Some(ast::Type::Fun(_)) => {
                                    return Err(wrap_error_msg(lexer, lhs_node.span(), "assignment to method"));
                                }
                                None => {
                                    return Err(wrap_error_msg(lexer, lhs_node.span(), "assignment to nonexistent field"));
                                }
                            }
                        }
                        non_class_prim => {
                            return Err(wrap_error_msg(
                                lexer,
                                expr_node.span(),
                                &format!("left side of . is a {:?}", non_class_prim),
                            ))
                        }
                    }
                }
                _other_expr_data => {
                    return Err(wrap_error_msg(
                        lexer,
                        lhs_node.span(),
                        "left side of = is not a variable nor a field",
                    ))
                }
            }
        }
        ast::Stmt::Incr(ident_node) | ast::Stmt::Decr(ident_node) => {
            let ident = ident_node.data().clone();
            match venv_get(venv, &ident) {
                Some(ast::Prim::Int) => Ok(false),
                Some(prim) => Err(type_mismatch_msg(ast::Prim::Int, &prim, lexer, ident_node.span())),
                None => Err(undeclared_var_msg(lexer, ident_node.span())),
            }
        }
        ast::Stmt::Ret(expr_node) => {
            let expr_prim = check_expr(&expr_node, &venv, cfienv, lexer)?;
            if !cmp_prims(fn_prim, &expr_prim, &cfienv.2) {
                Err(wrong_return_msg(fn_prim.clone(), &expr_prim, lexer, stmt.span()))
            } else {
                Ok(true)
            }
        }
        ast::Stmt::VRet => {
            if let ast::Prim::Void = fn_prim {
                Ok(true)
            } else {
                Err(wrong_return_msg(fn_prim.clone(), &ast::Prim::Void, lexer, stmt.span()))
            }
        }
        ast::Stmt::Block(block_node) => check_block(block_node.data(), fn_prim, &mut venv, cfienv, lexer),
        ast::Stmt::If(expr_node, stmt_node) | ast::Stmt::While(expr_node, stmt_node) => {
            let expr_prim = check_expr(&expr_node, &venv, cfienv, lexer)?;
            if let ast::Prim::Bool = expr_prim {
                venv_enter_scope(venv);
                let stmt_returns = check_stmt(&stmt_node, fn_prim, &mut venv, cfienv, lexer)?;
                venv_exit_scope(venv);
                if let Some(true) = expr_bool(expr_node, lexer)? {
                    Ok(stmt_returns)
                } else {
                    Ok(false)
                }
            } else {
                Err(type_mismatch_msg(ast::Prim::Bool, &expr_prim, lexer, expr_node.span()))
            }
        }
        ast::Stmt::IfElse(expr_node, stmt_true_node, stmt_false_node) => {
            let expr_prim = check_expr(&expr_node, &venv, cfienv, lexer)?;
            if let ast::Prim::Bool = expr_prim {
                venv_enter_scope(venv);
                let stmt_true_returns = check_stmt(&stmt_true_node, fn_prim, &mut venv, cfienv, lexer)?;
                venv_exit_scope(venv);
                venv_enter_scope(venv);
                let stmt_false_returns = check_stmt(&stmt_false_node, fn_prim, &mut venv, cfienv, lexer)?;
                venv_exit_scope(venv);
                match expr_bool(expr_node, lexer)? {
                    Some(true) => Ok(stmt_true_returns),
                    Some(false) => Ok(stmt_false_returns),
                    None => Ok(stmt_true_returns && stmt_false_returns),
                }
            } else {
                Err(type_mismatch_msg(ast::Prim::Bool, &expr_prim, lexer, expr_node.span()))
            }
        }
    }
}

fn check_fn(fdef: &ast::Node<ast::FunDef>, venv: &mut VEnv, cfienv: &CFIEnv, lexer: &dyn Lexer<u32>) -> Result<(), String> {
    let (prim_node, _, arg_nodes, block_node) = fdef.data();
    for arg_node in arg_nodes {
        let (arg_prim_node, arg_ident_node) = arg_node.data();
        let (prim, ident) = (arg_prim_node.data(), arg_ident_node.data());
        if venv_insert(venv, ident.clone(), prim.clone()).is_some() {
            return Err(wrap_error_msg(lexer, fdef.span(), "argument name repeated"));
        }
    }

    let fn_prim = prim_node.data();

    let fn_always_returns = check_block(block_node.data(), fn_prim, venv, cfienv, lexer)?;
    if ast::Prim::Void != *fn_prim && !fn_always_returns {
        return Err(wrap_error_msg(lexer, fdef.span(), "nonvoid function does not return"));
    }

    Ok(())
}

fn register_superclasses_in_env(
    ident_node: &ast::Node<ast::Ident>,
    ienv: &mut IEnv,
    cdefs: &HashMap<ast::Ident, ast::Node<ast::ClassDef>>,
    lexer: &dyn Lexer<u32>,
) -> Result<(), String> {
    let ident = ident_node.data();
    let parent_ident_node_option = match cdefs.get(ident) {
        Some(cdef) => &cdef.data().1,
        None => return Err(wrap_error_msg(lexer, ident_node.span(), "nonexistent class")),
    };

    if ienv.get(ident).is_some() {
        return Ok(());
    }

    let superclasses = match parent_ident_node_option {
        Some(parent_ident_node) => {
            let parent_ident = parent_ident_node.data();
            register_superclasses_in_env(&parent_ident_node, ienv, cdefs, lexer)?;
            let mut superclasses = ienv.get(parent_ident).unwrap().clone();
            superclasses.insert(parent_ident.to_string());
            superclasses
        }
        None => HashSet::new(),
    };
    ienv.insert(ident.to_string(), superclasses);

    Ok(())
}

fn register_class_in_env(
    ident_node: &ast::Node<ast::Ident>,
    cenv: &mut CEnv,
    cdefs: &HashMap<ast::Ident, ast::Node<ast::ClassDef>>,
    ienv: &IEnv,
    lexer: &dyn Lexer<u32>,
) -> Result<(), String> {
    let cdef = cdefs.get(ident_node.data()).unwrap(); // unwrap, bo register_superclasses już sprawdziło
    let (self_ident_node, parent_ident_node_option, field_nodes, method_nodes) = cdef.data();

    if cenv.get(self_ident_node.data()).is_some() {
        return Ok(());
    }

    let mut members = match parent_ident_node_option {
        Some(parent_ident_node) => {
            register_class_in_env(parent_ident_node, cenv, cdefs, ienv, lexer)?;
            cenv.get(parent_ident_node.data()).unwrap().clone()
        }
        None => HashMap::new(),
    };

    for field_node in field_nodes {
        let (prim_node, ident_node) = field_node.data();
        if members
            .insert(ident_node.data().to_string(), ast::Type::Var(prim_node.data().clone()))
            .is_some()
        {
            return Err(wrap_error_msg(lexer, ident_node.span(), "field name not unique"));
        }
    }

    for method_node in method_nodes {
        let (prim_node, ident_node, arg_nodes, _) = method_node.data();
        let mut arg_types = arg_types(arg_nodes, lexer)?;
        arg_types.insert(0, ast::Prim::Class(self_ident_node.data().to_string())); // this

        let new_type = ast::Type::Fun((prim_node.data().clone(), arg_types));

        if let Some(old_type) = members.insert(ident_node.data().to_string(), new_type.clone()) {
            if !cmp_types(&old_type, &new_type, ienv) {
                return Err(wrap_error_msg(lexer, method_node.span(), "method override type mismatch"));
            }
        }
    }

    cenv.insert(self_ident_node.data().to_string(), members);
    Ok(())
}

fn class_env(cdefs: &Vec<ast::Node<ast::ClassDef>>, lexer: &dyn Lexer<u32>) -> Result<(CEnv, IEnv), String> {
    let mut cenv: CEnv = HashMap::new();
    let mut ienv: IEnv = HashMap::new();
    let mut cdefs_map: HashMap<ast::Ident, ast::Node<ast::ClassDef>> = HashMap::new();
    for cdef in cdefs {
        match cdefs_map.insert(cdef.data().0.data().clone(), cdef.clone()) {
            None => (),
            Some(class_def_node) => {
                return Err(wrap_error_msg(lexer, class_def_node.span(), "class name not unique"));
            }
        }
    }
    for cdef in cdefs {
        register_class_in_env(&cdef.data().0, &mut cenv, &cdefs_map, &ienv, lexer)?;
        register_superclasses_in_env(&cdef.data().0, &mut ienv, &cdefs_map, lexer)?;
    }
    Ok((cenv, ienv))
}

fn arg_types(arg_nodes: &Vec<ast::Node<ast::Arg>>, lexer: &dyn Lexer<u32>) -> Result<Vec<ast::Prim>, String> {
    let mut arg_types = Vec::new();
    for arg_node in arg_nodes {
        let (arg_prim_node, _) = arg_node.data();
        let arg_prim = arg_prim_node.data();
        if let ast::Prim::Void = arg_prim {
            return Err(wrap_error_msg(lexer, arg_node.span(), "function argument is void"));
        } else {
            arg_types.push(arg_prim.clone());
        }
    }
    Ok(arg_types)
}

fn fn_env(fdefs: &Vec<ast::Node<ast::FunDef>>, lexer: &dyn Lexer<u32>) -> Result<FEnv, String> {
    let mut fenv: FEnv = [
        ("printInt".to_string(), (ast::Prim::Void, vec![ast::Prim::Int])),
        ("printString".to_string(), (ast::Prim::Void, vec![ast::Prim::Str])),
        ("error".to_string(), (ast::Prim::Void, vec![])),
        ("readInt".to_string(), (ast::Prim::Int, vec![])),
        ("readString".to_string(), (ast::Prim::Str, vec![])),
    ]
    .iter()
    .cloned()
    .collect();

    for fdef in fdefs.iter() {
        let (prim_node, ident_node, arg_nodes, _) = fdef.data();

        let arg_types = arg_types(arg_nodes, lexer)?;
        let (fn_type, fn_name) = (prim_node.data(), ident_node.data());
        if fenv.insert(fn_name.clone(), (fn_type.clone(), arg_types)).is_some() {
            return Err(wrap_error_msg(lexer, ident_node.span(), "function name not unique"));
        }
    }

    match fenv.get("main") {
        None => return Err("no main()".to_string()),
        Some((prim, args)) => {
            if let ast::Prim::Int = prim {
                if !args.is_empty() {
                    return Err("main() has arguments".to_string());
                }
            } else {
                return Err("wrong main() type".to_string());
            }
        }
    }

    Ok(fenv)
}
