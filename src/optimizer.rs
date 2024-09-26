use crate::ast::{
  ExprEnum, Expression, Span, Statement, Statements,
};

pub fn optimize(ast: &mut Statements) -> Result<(), String> {
  for stmt in ast {
    match stmt {
      Statement::Expression(ex) => optim_expr(ex)?,
      Statement::VarAssign { ex, .. } => optim_expr(ex)?,
      _ => {}
    }
  }
  Ok(())
}

fn optim_expr(expr: &mut Expression) -> Result<(), String> {
  if let Some(val) = const_expr(expr) {
    println!("optim const_expr: {val:?}");
    *expr = val;
  }
  Ok(())
}

fn const_expr<'a>(
  expr: &Expression<'a>,
) -> Option<Expression<'a>> {
  Some(match &expr.expr {
    ExprEnum::NumLiteral(_) => expr.clone(),
    ExprEnum::StrLiteral(_) => expr.clone(),
    ExprEnum::Add(lhs, rhs) => optim_bin_op(
      |lhs, rhs| lhs + rhs,
      |lhs, rhs| lhs.to_owned() + rhs,
      lhs,
      rhs,
      expr.span,
    )?,
    ExprEnum::FnInvoke(span, args) => {
      let args = args
        .iter()
        .map(|arg| {
          const_expr(arg).unwrap_or_else(|| arg.clone())
        })
        .collect();
      Expression::new(
        ExprEnum::FnInvoke(*span, args),
        expr.span,
      )
    }
    _ => return None,
  })
}

fn optim_bin_op<'a>(
  op_num: impl Fn(f64, f64) -> f64,
  op_str: impl Fn(&str, &str) -> String,
  lhs: &Expression<'a>,
  rhs: &Expression<'a>,
  span: Span<'a>,
) -> Option<Expression<'a>> {
  use ExprEnum::*;
  if let Some((lhs, rhs)) =
    const_expr(&lhs).zip(const_expr(&rhs))
  {
    match (&lhs.expr, &rhs.expr) {
      (NumLiteral(lhs), NumLiteral(rhs)) => Some(
        Expression::new(NumLiteral(op_num(*lhs, *rhs)), span),
      ),
      (StrLiteral(lhs), StrLiteral(rhs)) => Some(
        Expression::new(StrLiteral(op_str(lhs, rhs)), span),
      ),
      _ => None,
    }
  } else {
    None
  }
}
