use crate::parser::{calc_offset, GetSpan};
use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TypeDecl {
  Any,
  F64,
  I64,
  Str,
  Coro,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprEnum<'src> {
  Ident(Span<'src>),
  NumLiteral(f64),
  StrLiteral(String),
  FnInvoke(Span<'src>, Vec<Expression<'src>>),
  Add(Box<Expression<'src>>, Box<Expression<'src>>),
  Sub(Box<Expression<'src>>, Box<Expression<'src>>),
  Mul(Box<Expression<'src>>, Box<Expression<'src>>),
  Div(Box<Expression<'src>>, Box<Expression<'src>>),
  Gt(Box<Expression<'src>>, Box<Expression<'src>>),
  Lt(Box<Expression<'src>>, Box<Expression<'src>>),
  If(
    Box<Expression<'src>>,
    Box<Statements<'src>>,
    Option<Box<Statements<'src>>>,
  ),
  Await(Box<Expression<'src>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expression<'a> {
  pub(crate) expr: ExprEnum<'a>,
  pub(crate) span: Span<'a>,
}

impl<'a> Expression<'a> {
  pub fn new(expr: ExprEnum<'a>, span: Span<'a>) -> Self {
    Self { expr, span }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'src> {
  Expression(Expression<'src>),
  VarDef {
    span: Span<'src>,
    name: Span<'src>,
    td: TypeDecl,
    ex: Expression<'src>,
  },
  VarAssign {
    span: Span<'src>,
    name: Span<'src>,
    ex: Expression<'src>,
  },
  For {
    span: Span<'src>,
    loop_var: Span<'src>,
    start: Expression<'src>,
    end: Expression<'src>,
    stmts: Statements<'src>,
  },
  Break,
  Continue,
  FnDef {
    name: Span<'src>,
    args: Vec<(Span<'src>, TypeDecl)>,
    ret_type: TypeDecl,
    stmts: Statements<'src>,
    cofn: bool,
  },
  Return(Expression<'src>),
  Yield(Expression<'src>),
}

impl<'src> Statement<'src> {
  pub fn span(&self) -> Option<Span<'src>> {
    use Statement::*;
    Some(match self {
      Expression(ex) => ex.span,
      VarDef { span, .. } => *span,
      VarAssign { span, .. } => *span,
      For { span, .. } => *span,
      FnDef { name, stmts, .. } => {
        calc_offset(*name, stmts.span())
      }
      Return(ex) => ex.span,
      Break => return None,
      Continue => return None,
      Yield(ex) => ex.span,
    })
  }
}

pub type Statements<'a> = Vec<Statement<'a>>;
