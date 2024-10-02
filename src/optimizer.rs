use std::collections::HashMap;

use crate::ast::{
  ExprEnum, Expression, Span, Statement, Statements,
};

type Constants<'a> = HashMap<String, Expression<'a>>;

pub fn optimize(ast: &mut Statements) -> Result<(), String> {
  use Statement::*;
  let mut constants = Constants::new();
  for stmt in ast {
    match stmt {
      Expression(ex) => optim_expr(ex, &constants)?,
      VarDef { name, ex, .. } => {
        optim_expr(ex, &constants)?;
        if let Some(ex) = const_expr(ex, &constants) {
          constants.insert(name.to_string(), ex);
        }
      }
      VarAssign { name, ex, .. } => {
        optim_expr(ex, &constants)?;
        if let Some(ex) = const_expr(ex, &constants) {
          constants.insert(name.to_string(), ex);
        } else {
          // If the variable was assigned a non-constant, it should be removed from known constants.
          constants.remove(**name);
        }
      }
      For { stmts, .. } => {
        // Variables assigned in a loop is generally not a constant, so let's remove them.
        check_assigned(stmts, &mut constants);
      }
      FnDef { stmts, .. } => optimize(stmts)?,
      Return(ex) | Statement::Yield(ex) => {
        optim_expr(ex, &constants)?;
      }
      Break | Continue => {}
    }
  }
  Ok(())
}

fn check_assigned(
  stmts: &[Statement],
  constants: &mut Constants,
) {
  for stmt in stmts {
    match stmt {
      Statement::VarAssign { name, ex, .. } => {
        if const_expr(ex, constants).is_none() {
          constants.remove(**name);
        }
      }
      _ => {}
    }
  }
}

fn optim_expr<'a>(
  expr: &mut Expression<'a>,
  constants: &Constants<'a>,
) -> Result<(), String> {
  if let Some(val) = const_expr(expr, constants) {
    *expr = val;
  }
  Ok(())
}

fn const_expr<'a>(
  expr: &Expression<'a>,
  constants: &HashMap<String, Expression<'a>>,
) -> Option<Expression<'a>> {
  match &expr.expr {
    ExprEnum::Ident(name) => constants.get(**name).cloned(),
    ExprEnum::NumLiteral(_) => Some(expr.clone()),
    ExprEnum::StrLiteral(_) => Some(expr.clone()),
    ExprEnum::Add(lhs, rhs) => optim_bin_op(
      |lhs, rhs| lhs + rhs,
      |lhs: &str, rhs: &str| Some(lhs.to_owned() + rhs),
      lhs,
      rhs,
      expr.span,
      constants,
    ),
    ExprEnum::Sub(lhs, rhs) => optim_bin_op(
      |lhs, rhs| lhs - rhs,
      |_, _| None,
      lhs,
      rhs,
      expr.span,
      constants,
    ),
    ExprEnum::Mul(lhs, rhs) => optim_bin_op(
      |lhs, rhs| lhs * rhs,
      |_, _| None,
      lhs,
      rhs,
      expr.span,
      constants,
    ),
    ExprEnum::Div(lhs, rhs) => optim_bin_op(
      |lhs, rhs| lhs * rhs,
      |_, _| None,
      lhs,
      rhs,
      expr.span,
      constants,
    ),
    ExprEnum::Gt(lhs, rhs) => optim_bin_op(
      |lhs, rhs| (lhs > rhs) as i32 as f64,
      |_, _| None,
      lhs,
      rhs,
      expr.span,
      constants,
    ),
    ExprEnum::Lt(lhs, rhs) => optim_bin_op(
      |lhs, rhs| (lhs < rhs) as i32 as f64,
      |_, _| None,
      lhs,
      rhs,
      expr.span,
      constants,
    ),
    ExprEnum::FnInvoke(span, args) => {
      let args = args
        .iter()
        .map(|arg| {
          const_expr(arg, constants)
            .unwrap_or_else(|| arg.clone())
        })
        .collect();
      Some(Expression::new(
        ExprEnum::FnInvoke(*span, args),
        expr.span,
      ))
    }
    _ => None,
  }
}

fn optim_bin_op<'a>(
  op_num: impl Fn(f64, f64) -> f64,
  op_str: impl Fn(&str, &str) -> Option<String>,
  lhs: &Expression<'a>,
  rhs: &Expression<'a>,
  span: Span<'a>,
  constants: &Constants<'a>,
) -> Option<Expression<'a>> {
  use ExprEnum::*;
  if let Some((lhs, rhs)) =
    const_expr(&lhs, constants).zip(const_expr(&rhs, constants))
  {
    match (&lhs.expr, &rhs.expr) {
      (NumLiteral(lhs), NumLiteral(rhs)) => Some(
        Expression::new(NumLiteral(op_num(*lhs, *rhs)), span),
      ),
      (StrLiteral(lhs), StrLiteral(rhs)) => op_str(lhs, rhs)
        .map(|ex| Expression::new(StrLiteral(ex), span)),
      _ => None,
    }
  } else {
    None
  }
}
