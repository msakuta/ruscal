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

impl std::fmt::Display for TypeDecl {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    match self {
      TypeDecl::Any => write!(f, "any"),
      TypeDecl::F64 => write!(f, "f64"),
      TypeDecl::I64 => write!(f, "i64"),
      TypeDecl::Str => write!(f, "str"),
      TypeDecl::Coro => write!(f, "coro"),
    }
  }
}

impl<'a> Expression<'a> {
  fn format(
    &self,
    level: usize,
    f: &mut impl std::io::Write,
  ) -> std::io::Result<()> {
    let indent = "  ".repeat(level);
    match &self.expr {
      ExprEnum::Ident(span) => write!(f, "{span}"),
      ExprEnum::NumLiteral(val) => write!(f, "{val}"),
      ExprEnum::StrLiteral(val) => write!(f, "{val:?}"),
      ExprEnum::FnInvoke(name, args) => {
        write!(f, "{name}(")?;
        for (i, arg) in args.iter().enumerate() {
          if i == 0 {
            arg.format(level, f)?;
          } else {
            write!(f, ", ")?;
            arg.format(level, f)?;
          }
        }
        write!(f, ")")
      }
      ExprEnum::Add(lhs, rhs) => {
        format_bin_op(level, "+", lhs, rhs, f)
      }
      ExprEnum::Sub(lhs, rhs) => {
        format_bin_op(level, "-", lhs, rhs, f)
      }
      ExprEnum::Mul(lhs, rhs) => {
        format_bin_op(level, "*", lhs, rhs, f)
      }
      ExprEnum::Div(lhs, rhs) => {
        format_bin_op(level, "/", lhs, rhs, f)
      }
      ExprEnum::Gt(lhs, rhs) => {
        format_bin_op(level, ">", lhs, rhs, f)
      }
      ExprEnum::Lt(lhs, rhs) => {
        format_bin_op(level, "<", lhs, rhs, f)
      }
      ExprEnum::If(cond, t_branch, f_branch) => {
        write!(f, "if ")?;
        cond.format(level, f)?;
        write!(f, " {{ ")?;
        for stmt in t_branch.iter() {
          write!(f, "{indent}  ")?;
          stmt.format(level + 1, f)?;
        }
        write!(f, "}}")?;
        if let Some(f_branch) = f_branch {
          write!(f, " else {{")?;
          for stmt in f_branch.iter() {
            write!(f, "{indent}  ")?;
            stmt.format(level + 1, f)?;
          }
          write!(f, "}}")?;
        }
        Ok(())
      }
      ExprEnum::Await(ex) => {
        write!(f, "await ")?;
        ex.format(level, f)?;
        Ok(())
      }
    }
  }
}

fn format_bin_op(
  level: usize,
  op: &str,
  lhs: &Expression,
  rhs: &Expression,
  f: &mut impl std::io::Write,
) -> std::io::Result<()> {
  write!(f, "(")?;
  lhs.format(level, f)?;
  write!(f, "{op}")?;
  rhs.format(level, f)?;
  write!(f, ")")?;
  Ok(())
}

impl<'a> Statement<'a> {
  fn format(
    &self,
    level: usize,
    f: &mut impl std::io::Write,
  ) -> std::io::Result<()> {
    use Statement::*;
    let indent = "  ".repeat(level);
    match self {
      Expression(ex) => {
        write!(f, "{indent}")?;
        ex.format(level, f)?;
        writeln!(f, ";")
      }
      VarDef { name, td, ex, .. } => {
        write!(f, "{indent}var {name}: {td} = ")?;
        ex.format(level, f)?;
        writeln!(f, ";")
      }
      VarAssign { name, ex, .. } => {
        write!(f, "{indent}{name} = ")?;
        ex.format(level, f)?;
        writeln!(f, ";")
      }
      For {
        loop_var,
        start,
        end,
        stmts,
        ..
      } => {
        write!(f, "{indent}for {loop_var} in ")?;
        start.format(level, f)?;
        write!(f, "..")?;
        end.format(level, f)?;
        writeln!(f, " {{")?;
        for stmt in stmts {
          stmt.format(level + 1, f)?;
        }
        writeln!(f, "{indent}}}")?;
        Ok(())
      }
      Break => write!(f, "{indent}break;"),
      Continue => write!(f, "{indent}continue;"),
      FnDef {
        name,
        args,
        ret_type,
        stmts,
        cofn,
      } => {
        writeln!(f, "")?; // Put an extra line for readability
        write!(
          f,
          "{indent}{} {name}(",
          if *cofn { "cofn" } else { "fn" }
        )?;
        for (i, (_, arg)) in args.iter().enumerate() {
          if i == 0 {
            write!(f, "{arg}")?;
          } else {
            write!(f, ", {arg}")?;
          }
        }
        writeln!(f, ") -> {ret_type} {{")?;
        for stmt in stmts {
          stmt.format(level + 1, f)?;
        }
        writeln!(f, "{indent}}}")?;
        writeln!(f, "")?; // Put an extra line for readability
        Ok(())
      }
      Return(ex) => {
        write!(f, "{indent}return ")?;
        ex.format(level, f)?;
        writeln!(f, ";")
      }
      Yield(ex) => {
        write!(f, "{indent}yield ")?;
        ex.format(level, f)?;
        writeln!(f, ";")
      }
    }
  }
}

pub fn print_stmts(
  stmts: &[Statement],
  f: &mut impl std::io::Write,
) -> std::io::Result<()> {
  for stmt in stmts {
    stmt.format(1, f)?;
  }
  Ok(())
}
