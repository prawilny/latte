%start TopDefs
%%

// let sign = $1.map_err(|_| ())?;
// let v = $2.map_err(|_| ())?;
// Ok(Node::new(Span::new(sign.span().start(), v.span().end()), Expr::Neg(Box::new(v))))

TopDefs -> Result<Vec<TopDef>, ()>:
      { Ok(vec![]) }
    |
      TopDefs TopDef {
        let mut defs = $1?;
        defs.push($2?);
        Ok(defs)
      }
    ;
TopDef -> Result<TopDef, ()>:
      ClassDef {
          Ok(TopDef::Class($1?))
      }
    |
      FunDef {
          Ok(TopDef::Function($1?))
      }
    ;

ClassDef -> Result<Node<ClassDef>, ()>:
      'CLASS' Ident '{' Members FunDefs '}' {
          let class = $1.map_err(|_| ())?;
          let rb = $6.map_err(|_| ())?;
          Ok(Node::new(Span::new(class.span().start(), rb.span().end()), ($2?, None, $4?, $5?)))
      }
    |
      'CLASS' Ident 'EXTENDS' Ident '{' Members FunDefs '}' {
          let class = $1.map_err(|_| ())?;
          let rb = $8.map_err(|_| ())?;
          Ok(Node::new(Span::new(class.span().start(), rb.span().end()), ($2?, Some($4?), $6?, $7?)))
      }
    ;
//      'CLASS' Ident 'EXTENDS'

FunDefs -> Result<Vec<Node<FunDef>>, ()>:
      { Ok(vec![]) }
    |
      FunDefs FunDef {
        let mut funs = $1?;
        funs.push($2?);
        Ok(funs)
      }
    ;
FunDef -> Result<Node<FunDef>, ()>:
      PrimIdent '(' Args ')' Block {
        let prim_ident = $1.map_err(|_| ())?;
        let block = $6.map_err(|_| ())?;
        Ok(Node::new(Span::new(prim_ident.span().start(), block.span().end()), (prim_ident.0, prim_ident.1, $4?, block)))
      }
    ;

Ident -> Result<Node<Ident>, ()>:
      'IDENT' {
          let v = $1.map_err(|_| ())?;
          Ok(Node::new(v.span(), $lexer.span_str(v.span()).to_string()))
      }
    ;

Exprs -> Result<Vec<Node<Expr>>, ()>:
      { Ok(vec![]) }
    |
      Exprs2 { $1 }
    ;
Exprs2 -> Result<Vec<Node<Expr>>, ()>:
      Expr { Ok(vec![$1?]) }
    |
      Exprs2 ',' Expr {
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
        Ok(Node::new(Span::new(ident.span().start(), rb.span().end()), Expr::Fun(ident, $3?)))
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

Members -> Result<Vec<Node<Member>>, ()>:
      { Ok(vec![]) }
    |
      Members PrimIdent ';' {
        let mut members = $1?;
        members.push($2?);
        Ok(members)
      }
    ;
Args -> Result<Vec<Node<Arg>>, ()>:
      { Ok(vec![]) }
    |
      Args2 { $1 }
    ;
Args2 -> Result<Vec<Node<Arg>>, ()>:
      PrimIdent { Ok(vec![$1?]) }
    |
      Args2 ',' PrimIdent {
        let mut args = $1?;
        args.push($3?);
        Ok(args)
      }
    ;
PrimIdent -> Result<Node<(Node<Prim>, Node<Ident>)>, ()>:
      Prim Ident {
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
      '{' Stmts '}' {
        let lb = $1.map_err(|_| ())?;
        let rb = $3.map_err(|_| ())?;
        Ok(Node::new(Span::new(lb.span().start(), rb.span().end()), $2?))
      }
    ;

Stmts -> Result<Vec<Node<Stmt>>, ()>:
      { Ok(vec![]) }
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
      Prim Items ';' {
        let prim = $1.map_err(|_| ())?;
        let items = $2.map_err(|_| ())?;
        let semi = $3.map_err(|_| ())?;
        Ok(Node::new(Span::new(prim.span().start(), semi.span().end()), Stmt::Decl(prim, items)))
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

use ::lrpar::Span;
use std::fmt::Debug;
use std::cell::RefCell;
use std::option::Option;

#[derive(Debug, Clone)]
pub struct Node<N: Debug + Clone> {
    span: Span,
    data: N,
    static_type: RefCell<Option<Type>>,
}

impl<N: Debug + Clone> Node<N> {
    pub fn new(span: Span, data: N) -> Node<N> {
        Node{
          span,
          data,
          static_type: RefCell::new(None),
        }
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn data(&self) -> &N {
        &self.data
    }

    pub fn set_type(&self, t: &Type) {
        self.static_type.replace(Some(t.clone()));
    }

    pub fn get_type(&self) -> Type {
        self.static_type.borrow().clone().unwrap()
    }
}

fn join_ast_spans<N1: Debug + Clone, N2: Debug + Clone>(start: &Result<Node<N1>, ()>, end: &Result<Node<N2>, ()>) -> Result<Span, ()> {
    let start_ok = start.as_ref().map_err(|_| ())?;
    let end_ok = end.as_ref().map_err(|_| ())?;
    Ok(Span::new(start_ok.span().start(), end_ok.span().end()))
}

pub type Ident = String;

pub type FunDef = (Node<Prim>, Node<Ident>, Vec<Node<Arg>>, Node<Block>);

pub type ClassDef = (Node<Ident>, Option<Node<Ident>>, Vec<Node<Member>>, Vec<Node<FunDef>>);

pub type FunType = (Prim, Vec<Prim>);

pub type Block = Vec<Node<Stmt>>;

pub type Arg = (Node<Prim>, Node<Ident>);

pub type Member = Arg;

pub type IntType = i64;

#[derive(Debug, Clone)]
pub enum TopDef {
  Function(Node<FunDef>),
  Class(Node<ClassDef>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Empty,
    Block(Node<Block>),
    Decl(Node<Prim>, Vec<Node<Item>>),
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

impl std::fmt::Display for Prim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      let prim = match self {
        Prim::Int => "int".to_string(),
        Prim::Str => "string".to_string(),
        Prim::Void => "void".to_string(),
        Prim::Bool => "boolean".to_string(),
        Prim::Class(class_name) => format!("class {}", &class_name),
      };
      write!(f, "{}", prim)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Prim {
    Int,
    Str,
    Void,
    Bool,
    Class(Ident),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Var(Prim),
//    Fun(FunType)
}

#[derive(Debug, Clone)]
pub enum Expr {
    Var(Node<Ident>),
    Int(IntType),
    Bool(bool),
    Str(String),

    New(Node<Ident>),
    Null(Node<Ident>),
    Dot(Box<Node<Expr>>, Node<Ident>),
    Mthd(Box<Node<Expr>>, Node<Ident>, Vec<Node<Expr>>),
    Fun(Node<Ident>, Vec<Node<Expr>>),

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

fn parse_int(s: &str) -> Result<IntType, ()> {
    match s.parse::<IntType>() {
        Ok(val) => Ok(val),
        Err(_) => {
            Err(())
        }
    }
}