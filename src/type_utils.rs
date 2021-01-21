use crate::latte_y as ast;

type IEnv = HashMap<ast::Ident, HashSet<ast::Ident>>;

fn is_subclass(child: &ast::Ident, parent: &ast::Ident, ienv: &IEnv) -> bool {
    match ienv.get(child) {
        None => false,
        Some(superclasses) => superclasses.get(parent).is_some(),
    }
}

fn cmp_prims(expected: &ast::Prim, actual: &ast::Prim, ienv: &IEnv) -> bool {
    // TODO: czy matchowane typy się zgadzają
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
                    let cmps: Vec<bool> = exp_arg_prims
                        .iter()
                        .zip(act_arg_prims.iter())
                        .map(|(exp_arg_prim, act_arg_prim)| {
                            return cmp_prims(exp_arg_prim, act_arg_prim, ienv);
                        })
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
