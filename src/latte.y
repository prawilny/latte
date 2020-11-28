%start Program
%avoid_insert "INT"
%%
Program -> Result<Vec<Node<Stmt>>, ()>:
      Stmt {
        Ok(vec![$1?])
      }
    |
      Program ';' Stmt {
        let mut stmts = $1?;
        stmts.push($3?);
        Ok(stmts)
      }
    ;

Ident -> Result<Node<Ident>, ()>:
      'IDENT' {
          let v = $1.map_err(|_| ())?;
          Ok(Node::new(v.span(), $lexer.span_str(v.span()).to_string()))
      }
    ;

Stmt -> Result<Node<Stmt>, ()>:
      Ident '=' Expr {
          let ident = $1?;
          let expr = $3.map_err(|_| ())?;
          Ok(Node::new(Span::new(ident.span().start(), expr.span().end()), Stmt::Assignment(ident, expr)))
      }
    |
      Expr {
          let expr = $1?;
          Ok(Node::new(Span::new(expr.span().start(), expr.span().end()), Stmt::Expression(expr)))
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
        Ok(Node::new(Span::new(ident.span().start(), rb.span().end()), Expr::App($1?, $3?)))
      }
    |
      Ident '(' ')' {
        let ident = $1?;
        let rb = $3.map_err(|_| ())?;
        Ok(Node::new(Span::new(ident.span().start(), rb.span().end()), Expr::App($1?, vec![])))
      }
    |
      Ident {
        Ok(Node::new($1.clone()?.span().clone(), Expr::Var($1?)))
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
      "INT" {
        let p = $1.map_err(|_| ())?;
        Ok(Node::new(p.span(), Prim::Int))
      }
    |
      "STR" {
        let p = $1.map_err(|_| ())?;
        Ok(Node::new(p.span(), Prim::Str))
      }
    |
      "BOOL" {
        let p = $1.map_err(|_| ())?;
        Ok(Node::new(p.span(), Prim::Bool))
      }
    |
      "VOID" {
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
        Ok(Node::new($1?.span().clone(), Item::NoInit($1?)))
      }
    |
      Ident '==' Expr {
        Ok(Node::new(join_ast_spans(&$1, &$3)?, Item::Init($1?, $3?)))
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