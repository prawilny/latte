%start Program
%avoid_insert "INT"
%%
Program -> Result<Vec<Node<Statement>>, ()>:
      Stmt {
        Ok(vec![$1?])
      }
    |
      Program ';' Stmt {
        let mut statements = $1?;
        statements.push($3?);
        Ok(statements)
      }
    ;

Ident -> Result<Node<Ident>, ()>:
      'IDENT' {
          let v = $1.map_err(|_| ())?;
          Ok(Node::new(v.span(), $lexer.span_str(v.span()).to_string()))
      }
    ;

Stmt -> Result<Node<Statement>, ()>:
      Ident '=' Expr {
          let ident = $1?;
          let expr = $3.map_err(|_| ())?;
          Ok(Node::new(Span::new(ident.span().start(), expr.span().end()), Statement::Assignment(ident, expr)))
      }
    |
      Expr {
          let expr = $1?;
          Ok(Node::new(Span::new(expr.span().start(), expr.span().end()), Statement::Expression(expr)))
      }
    ;

Expr -> Result<Node<Expr>, ()>:
      Expr1 { $1 }
    ;
Expr1 -> Result<Node<Expr>, ()>:
      Expr2 '+' Expr1 {
        Ok(Node::new(join_ast_spans(&$1, &$3)?, Expr::Add(Box::new($1?), Box::new($3?))))
      }
    |
      Expr2 {
        $1
      }
    ;
Expr2 -> Result<Node<Expr>, ()>:
      Expr2 '-' Expr3 {
        Ok(Node::new(join_ast_spans(&$1, &$3)?, Expr::Sub(Box::new($1?), Box::new($3?))))
      }
    |
      Expr3 {
        $1
      }
    ;
Expr3 -> Result<Node<Expr>, ()>:
      Expr3 '*' Expr4 {
        Ok(Node::new(join_ast_spans(&$1, &$3)?, Expr::Mul(Box::new($1?), Box::new($3?))))
      }
    |
      Expr3 '/' Expr4 {
        Ok(Node::new(join_ast_spans(&$1, &$3)?, Expr::Div(Box::new($1?), Box::new($3?))))
      }
    |
      Expr4 {
        $1
      }
    ;
Expr4 -> Result<Node<Expr>, ()>:
      'INT' {
          let v = $1.map_err(|_| ())?;
          Ok(Node::new(v.span(), Expr::Const(parse_int($lexer.span_str(v.span()))?)))
      }
    |
      '-' 'INT' {
          let sign = $1.map_err(|_| ())?;
          let v = $2.map_err(|_| ())?;
          Ok(Node::new(Span::new(sign.span().start(), v.span().end()), Expr::Const(-1 * parse_int($lexer.span_str(v.span()))?)))
      }
    |
      Ident {
        Ok(Node::new($1.clone()?.span().clone(), Expr::Var($1?)))
      }
    |
      Expr5 {
        $1
      }
    ;
Expr5 -> Result<Node<Expr>, ()>:
      '(' Expr ')' {
        $2
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

#[derive(Debug, Clone)]
pub enum Statement {
    Assignment(Node<Ident>, Node<Expr>),
    Expression(Node<Expr>),
}

pub type Ident = String;

#[derive(Debug, Clone)]
pub enum Expr {
    Var(Node<Ident>),
    Int(i64),
    Bool(bool),
    Str(String),

    App(Node<Ident>, Vec<Node<Ident>>)
    Neg(Box<Node<Expr>>),
    Not(Box<Node<Expr>>),

    Add(Box<Node<Expr>>, Box<Node<Expr>>),
    Sub(Box<Node<Expr>>, Box<Node<Expr>>),
    Mul(Box<Node<Expr>>, Box<Node<Expr>>),
    Div(Box<Node<Expr>>, Box<Node<Expr>>),
    Mod(Box<Node<Expr>>, Box<Node<Expr>>),

    And(Box<Node<Expr>>, Box<Node<Expr>>),
    Or(Box<Node<Expr>>, Box<Node<Expr>>),
    LTH(Box<Node<Expr>>, Box<Node<Expr>>),
    LEQ(Box<Node<Expr>>, Box<Node<Expr>>),
    GEQ(Box<Node<Expr>>, Box<Node<Expr>>),
    GTH(Box<Node<Expr>>, Box<Node<Expr>>),
    EQ(Box<Node<Expr>>, Box<Node<Expr>>),
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