%start TopDefs
%avoid_insert "INT"
%%

TopDefs -> Result<Vec<Node<TopDef>>, ()>:
      TopDef {
        Ok(vec![$1?])
      }
    |
      TopDefs TopDef {
        let mut tds = $1?;
        tds.push($2?);
        Ok(tds)
      }
    ;
TopDef -> Result<Node<TopDef>, ()>:
      Type Ident '(' ')' Block {
        let t = $1.map_err(|_| ())?;
        let ident = $2.map_err(|_| ())?;
        let block = $5.map_err(|_| ())?;
        Ok(Node::new(Span::new(t.span().start(), block.span().end()), (t, ident, vec![], block)))
      }
    |
      Type Ident '(' Args ')' Block {
        let t = $1.map_err(|_| ())?;
        let ident = $2.map_err(|_| ())?;
        let args = $4.map_err(|_| ())?;
        let block = $6.map_err(|_| ())?;
        Ok(Node::new(Span::new(t.span().start(), block.span().end()), (t, ident, args, block)))
      }
    ;

Ident -> Result<Node<Ident>, ()>:
      'IDENT' {
          let v = $1.map_err(|_| ())?;
          Ok(Node::new(v.span(), $lexer.span_str(v.span()).to_string()))
      }
    ;

Exprs -> Result<Vec<Node<Expr>>, ()>:
      Expr {
        Ok(vec![$1?])
      }
    |
      Exprs ',' Expr {
        let mut exprs = $1?;
        exprs.push($3?);
        Ok(exprs)
      }
    ;
Expr -> Result<Node<Expr>, ()>:
      Expr0 { $1 }
    ;

Expr0 -> Result<Node<Expr>, ()>:
      Expr1 '||' Expr0 {
        Ok(Node::new(join_ast_spans(&$1, &$3)?, Expr::Or(Box::new($1?), Box::new($3?))))
      }
    |
      Expr1 {
        $1
      }
    ;
Expr1 -> Result<Node<Expr>, ()>:
      Expr2 '&&' Expr1 {
        Ok(Node::new(join_ast_spans(&$1, &$3)?, Expr::And(Box::new($1?), Box::new($3?))))
      }
    |
      Expr2 {
        $1
      }
    ;
Expr2 -> Result<Node<Expr>, ()>:
      Expr2 '<=' Expr3 {
        Ok(Node::new(join_ast_spans(&$1, &$3)?, Expr::LEQ(Box::new($1?), Box::new($3?))))
      }
    |
      Expr2 '>=' Expr3 {
        Ok(Node::new(join_ast_spans(&$1, &$3)?, Expr::GEQ(Box::new($1?), Box::new($3?))))
      }
    |
      Expr2 '<' Expr3 {
        Ok(Node::new(join_ast_spans(&$1, &$3)?, Expr::LTH(Box::new($1?), Box::new($3?))))
      }
    |
      Expr2 '>' Expr3 {
        Ok(Node::new(join_ast_spans(&$1, &$3)?, Expr::GTH(Box::new($1?), Box::new($3?))))
      }
    |
      Expr2 '==' Expr3 {
        Ok(Node::new(join_ast_spans(&$1, &$3)?, Expr::EQ(Box::new($1?), Box::new($3?))))
      }
    |
      Expr2 '!=' Expr3 {
        Ok(Node::new(join_ast_spans(&$1, &$3)?, Expr::NEQ(Box::new($1?), Box::new($3?))))
      }
    |
      Expr3 {
        $1
      }
    ;
Expr3 -> Result<Node<Expr>, ()>:
      Expr3 '+' Expr4 {
        Ok(Node::new(join_ast_spans(&$1, &$3)?, Expr::Add(Box::new($1?), Box::new($3?))))
      }
    |
      Expr3 '-' Expr4 {
        Ok(Node::new(join_ast_spans(&$1, &$3)?, Expr::Sub(Box::new($1?), Box::new($3?))))
      }
    |
      Expr4 {
        $1
      }
    ;
Expr4 -> Result<Node<Expr>, ()>:
      Expr4 '*' Expr5 {
        Ok(Node::new(join_ast_spans(&$1, &$3)?, Expr::Mul(Box::new($1?), Box::new($3?))))
      }
    |
      Expr4 '/' Expr5 {
        Ok(Node::new(join_ast_spans(&$1, &$3)?, Expr::Div(Box::new($1?), Box::new($3?))))
      }
    |
      Expr4 '%' Expr5 {
        Ok(Node::new(join_ast_spans(&$1, &$3)?, Expr::Mod(Box::new($1?), Box::new($3?))))
      }
    |
      Expr5 {
        $1
      }
    ;
Expr5 -> Result<Node<Expr>, ()>:
      '-' Expr6 {
          let sign = $1.map_err(|_| ())?;
          let v = $2.map_err(|_| ())?;
          Ok(Node::new(Span::new(sign.span().start(), v.span().end()), Expr::Neg(Box::new(v))))
      }
    |
      '!' Expr6 {
          let sign = $1.map_err(|_| ())?;
          let v = $2.map_err(|_| ())?;
          Ok(Node::new(Span::new(sign.span().start(), v.span().end()), Expr::Not(Box::new(v))))
      }
    |
      Expr6 {
        $1
      }
    ;
Expr6 -> Result<Node<Expr>, ()>:
      'INTEGER' {
          let v = $1.map_err(|_| ())?;
          Ok(Node::new(v.span(), Expr::Int(parse_int($lexer.span_str(v.span()))?)))
      }
    |
      Ident '(' Exprs ')' {
        let ident = $1?;
        let rb = $4.map_err(|_| ())?;
        Ok(Node::new(Span::new(ident.span().start(), rb.span().end()), Expr::App(ident, $3?)))
      }
    |
      Ident '(' ')' {
        let ident = $1?;
        let rb = $3.map_err(|_| ())?;
        Ok(Node::new(Span::new(ident.span().start(), rb.span().end()), Expr::App(ident, vec![])))
      }
    |
      Ident {
        let v = $1.map_err(|_| ())?;
        Ok(Node::new(v.span().clone(), Expr::Var(v)))
      }
    |
      'STRING' {
        let v = $1.map_err(|_| ())?;
        Ok(Node::new(v.span(), Expr::Str($lexer.span_str(v.span()).to_string())))
      }
    |
      'TRUE' {
        let v = $1.map_err(|_| ())?;
        Ok(Node::new(v.span(), Expr::Bool(true)))
      }
    |
      'FALSE' {
        let v = $1.map_err(|_| ())?;
        Ok(Node::new(v.span(), Expr::Bool(false)))
      }
    |
      Expr7 {
        $1
      }
    ;
Expr7 -> Result<Node<Expr>, ()>:
      '(' Expr ')' {
        $2
      }
    ;

Type -> Result<Node<Type>, ()>:
      Prim {
        let p = $1?;
        Ok(Node::new(p.span().clone(), Type::VType(p.node().clone())))
      }
    |
      Prim '(' Prims ')' {
        let p = $1?;
        let ps = $3?.iter().map(|p| p.node().clone()).collect();
        let rb = $4.map_err(|_| ())?;

        Ok(Node::new(Span::new(p.span().start(), rb.span().end()), Type::FType(p.node().clone(), ps)))
      }
    |
      Prim '(' ')' {
        let p = $1?;
        let rb = $3.map_err(|_| ())?;
        Ok(Node::new(Span::new(p.span().start(), rb.span().end()), Type::FType(p.node.clone(), vec![])))
      }
    ;

Prims -> Result<Vec<Node<Prim>>, ()>:
      Prim {
        Ok(vec![$1?])
      }
    |
      Prims ',' Prim {
        let mut prims = $1?;
        prims.push($3?);
        Ok(prims)
      }
    ;
Prim -> Result<Node<Prim>, ()>:
      'INT' {
        let p = $1.map_err(|_| ())?;
        Ok(Node::new(p.span(), Prim::Int))
      }
    |
      'STR' {
        let p = $1.map_err(|_| ())?;
        Ok(Node::new(p.span(), Prim::Str))
      }
    |
      'BOOL' {
        let p = $1.map_err(|_| ())?;
        Ok(Node::new(p.span(), Prim::Bool))
      }
    |
      'VOID' {
        let p = $1.map_err(|_| ())?;
        Ok(Node::new(p.span(), Prim::Void))
      }
    ;

Args -> Result<Vec<Node<Arg>>, ()>:
      Arg {
        Ok(vec![$1?])
      }
    |
      Args ',' Arg {
        let mut args = $1?;
        args.push($3?);
        Ok(args)
      }
    ;
Arg -> Result<Node<Arg>, ()>:
      Type Ident {
        Ok(Node::new(join_ast_spans(&$1, &$2)?, ($1?, $2?)))
      }
    ;

Items -> Result<Vec<Node<Item>>, ()>:
      Item {
        Ok(vec![$1?])
      }
    |
      Items ',' Item {
        let mut items = $1?;
        items.push($3?);
        Ok(items)
      }
    ;
Item -> Result<Node<Item>, ()>:
      Ident {
        let ident = $1.map_err(|_| ())?;
        Ok(Node::new(ident.span().clone(), Item::NoInit(ident)))
      }
    |
      Ident '=' Expr {
        let ident = $1.map_err(|_| ())?;
        let expr = $3.map_err(|_| ())?;
        Ok(Node::new(Span::new(ident.span().start(), expr.span().end()), Item::Init(ident, expr)))
      }
  ;

Block -> Result<Node<Block>, ()>:
      '{' '}' {
        let lb = $1.map_err(|_| ())?;
        let rb = $2.map_err(|_| ())?;
        Ok(Node::new(Span::new(lb.span().start(), rb.span().end()), vec![]))
      }
    |
      '{' Stmts '}' {
        let lb = $1.map_err(|_| ())?;
        let rb = $3.map_err(|_| ())?;
        Ok(Node::new(Span::new(lb.span().start(), rb.span().end()), $2?))
      }
    ;

Stmts -> Result<Vec<Node<Stmt>>, ()>:
      Stmt {
        Ok(vec![$1?])
      }
    |
      Stmts Stmt {
        let mut stmts = $1?;
        stmts.push($2?);
        Ok(stmts)
      }
    ;

Stmt -> Result<Node<Stmt>, ()>: OpenStmt { $1 } | ClosedStmt { $1 };
OpenStmt -> Result<Node<Stmt>, ()>:
      'IF' '(' Expr ')' SimpleStmt {
        let if_ = $1.map_err(|_| ())?;
        let expr = $3.map_err(|_| ())?;
        let stmt = Box::new($5.map_err(|_| ())?);
        Ok(Node::new(Span::new(if_.span().start(), stmt.span().end()), Stmt::If(expr, stmt)))
      }
    |
      'IF' '(' Expr ')' OpenStmt {
        let if_ = $1.map_err(|_| ())?;
        let expr = $3.map_err(|_| ())?;
        let stmt = Box::new($5.map_err(|_| ())?);
        Ok(Node::new(Span::new(if_.span().start(), stmt.span().end()), Stmt::If(expr, stmt)))
      }
    |
      'IF' '(' Expr ')' ClosedStmt 'ELSE' OpenStmt {
        let if_ = $1.map_err(|_| ())?;
        let expr = $3.map_err(|_| ())?;
        let if_stmt = Box::new($5.map_err(|_| ())?);
        let else_stmt = Box::new($7.map_err(|_| ())?);
        Ok(Node::new(Span::new(if_.span().start(), else_stmt.span().end()), Stmt::IfElse(expr, if_stmt, else_stmt)))
      }
    |
      'WHILE' '(' Expr ')' OpenStmt {
        let while_ = $1.map_err(|_| ())?;
        let expr = $3.map_err(|_| ())?;
        let stmt = Box::new($5.map_err(|_| ())?);
        Ok(Node::new(Span::new(while_.span().start(), stmt.span().end()), Stmt::While(expr, stmt)))
      }
    ;
ClosedStmt -> Result<Node<Stmt>, ()>: SimpleStmt { $1 }
    |
      'IF' '(' Expr ')' ClosedStmt 'ELSE' ClosedStmt {
        let if_ = $1.map_err(|_| ())?;
        let expr = $3.map_err(|_| ())?;
        let if_stmt = Box::new($5.map_err(|_| ())?);
        let else_stmt = Box::new($7.map_err(|_| ())?);
        Ok(Node::new(Span::new(if_.span().start(), else_stmt.span().end()), Stmt::IfElse(expr, if_stmt, else_stmt)))
      }
    |
      'WHILE' '(' Expr ')' ClosedStmt {
        let while_ = $1.map_err(|_| ())?;
        let expr = $3.map_err(|_| ())?;
        let stmt = Box::new($5.map_err(|_| ())?);
        Ok(Node::new(Span::new(while_.span().start(), stmt.span().end()), Stmt::While(expr, stmt)))
      }
    ;

SimpleStmt -> Result<Node<Stmt>, ()>:
      ';' {
        let semi = $1.map_err(|_| ())?;
        Ok(Node::new(semi.span().clone(), Stmt::Empty))
      }
    |
      Block {
        let block = $1.map_err(|_| ())?;
        Ok(Node::new(block.span().clone(), Stmt::Block(block)))
      }
    |
      Type Items ';' {
        let t = $1.map_err(|_| ())?;
        let items = $2.map_err(|_| ())?;
        let semi = $3.map_err(|_| ())?;
        Ok(Node::new(Span::new(t.span().start(), semi.span().end()), Stmt::Decl(t, items)))
      }
    |
      Ident '=' Expr ';' {
        Ok(Node::new(join_ast_spans(&$1, &$3)?, Stmt::Asgn($1?, $3?)))
      }
    |
      Ident '++' ';' {
        let ident = $1.map_err(|_| ())?;
        Ok(Node::new(ident.span().clone(), Stmt::Incr(ident)))
      }
    |
      Ident '--' ';' {
        let ident = $1.map_err(|_| ())?;
        Ok(Node::new(ident.span().clone(), Stmt::Decr(ident)))
      }
    |
      'RETURN' ';' {
        let ret = $1.map_err(|_| ())?;
        let semi = $2.map_err(|_| ())?;
        Ok(Node::new(Span::new(ret.span().start(), semi.span().end()), Stmt::VRet))
      }
    |
      'RETURN' Expr ';' {
        let ret = $1.map_err(|_| ())?;
        let expr = $2.map_err(|_| ())?;
        let semi = $3.map_err(|_| ())?;
        Ok(Node::new(Span::new(ret.span().start(), semi.span().end()), Stmt::Ret(expr)))
      }
    |
      Expr ';' {
        let expr = $1.map_err(|_| ())?;
        let semi = $2.map_err(|_| ())?;
        Ok(Node::new(Span::new(expr.span().start(), semi.span().end()), Stmt::Expr(expr)))
      }
    ;

%%
// Any functions here are in scope for all the grammar actions above.

use std::fmt::Debug;
use ::lrpar::Span;

#[derive(Debug, Clone)]
pub struct Node<N: Debug + Clone> {
    span: Span,
    node: N,
}

impl<N: Debug + Clone> Node<N> {
    pub fn new(span: Span, node: N) -> Node<N> {
        Node{span, node}
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn node(&self) -> &N {
        &self.node
    }
}

fn join_ast_spans<N1: Debug + Clone, N2: Debug + Clone>(start: &Result<Node<N1>, ()>, end: &Result<Node<N2>, ()>) -> Result<Span, ()> {
    let start_ok = start.as_ref().map_err(|_| ())?;
    let end_ok = end.as_ref().map_err(|_| ())?;
    Ok(Span::new(start_ok.span().start(), end_ok.span().end()))
}

pub type Ident = String;

pub type TopDef = (Node<Type>, Node<Ident>, Vec<Node<Arg>>, Node<Block>);

pub type Block = Vec<Node<Stmt>>;

pub type Arg = (Node<Type>, Node<Ident>);

#[derive(Debug, Clone)]
pub enum Stmt {
    Empty,
    Block(Node<Block>),
    Decl(Node<Type>, Vec<Node<Item>>),
    Asgn(Node<Ident>, Node<Expr>),
    Incr(Node<Ident>),
    Decr(Node<Ident>),
    Ret(Node<Expr>),
    VRet,
    If(Node<Expr>, Box<Node<Stmt>>),
    IfElse(Node<Expr>, Box<Node<Stmt>>, Box<Node<Stmt>>),
    While(Node<Expr>, Box<Node<Stmt>>),
    Expr(Node<Expr>),
}

#[derive(Debug, Clone)]
pub enum Item {
    NoInit(Node<Ident>),
    Init(Node<Ident>, Node<Expr>),
}

#[derive(Debug, Clone)]
pub enum Type {
    VType(Prim),
    FType(Prim, Vec<Prim>),
}

#[derive(Debug, Clone)]
pub enum Prim {
    Int,
    Str,
    Void,
    Bool,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Var(Node<Ident>),
    Int(i64),
    Bool(bool),
    Str(String),

    App(Node<Ident>, Vec<Node<Expr>>),
    Neg(Box<Node<Expr>>),
    Not(Box<Node<Expr>>),

    Add(Box<Node<Expr>>, Box<Node<Expr>>),
    Sub(Box<Node<Expr>>, Box<Node<Expr>>),
    Mul(Box<Node<Expr>>, Box<Node<Expr>>),
    Div(Box<Node<Expr>>, Box<Node<Expr>>),
    Mod(Box<Node<Expr>>, Box<Node<Expr>>),

    And(Box<Node<Expr>>, Box<Node<Expr>>),
    Or (Box<Node<Expr>>, Box<Node<Expr>>),

    LTH(Box<Node<Expr>>, Box<Node<Expr>>),
    LEQ(Box<Node<Expr>>, Box<Node<Expr>>),
    GEQ(Box<Node<Expr>>, Box<Node<Expr>>),
    GTH(Box<Node<Expr>>, Box<Node<Expr>>),
    EQ (Box<Node<Expr>>, Box<Node<Expr>>),
    NEQ(Box<Node<Expr>>, Box<Node<Expr>>),
}

fn parse_int(s: &str) -> Result<i64, ()> {
    match s.parse::<i64>() {
        Ok(val) => Ok(val),
        Err(_) => {
            eprintln!("{} cannot be represented as a i64", s);
            Err(())
        }
    }
}