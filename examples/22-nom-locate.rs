use std::{
  cmp::Ordering, collections::HashMap, ops::ControlFlow,
};

use ::ruscal::{parse_args, RunMode};
use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::{
    alpha1, alphanumeric1, char, multispace0, multispace1,
    none_of,
  },
  combinator::{cut, map_res, opt, recognize},
  error::ParseError,
  multi::{fold_many0, many0, separated_list0},
  number::complete::recognize_float,
  sequence::{delimited, pair, preceded, terminated},
  Finish, IResult, InputTake, Offset, Parser,
};
use nom_locate::LocatedSpan;

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let Some(args) = parse_args(false) else { return Ok(()) };

  let src_file = args.source.as_ref().ok_or_else(|| {
    "Please specify source file to compile after -c"
  })?;
  let source = std::fs::read_to_string(src_file)?;

  let parsed_statements =
    match statements_finish(Span::new(&source)) {
      Ok(stmts) => stmts,
      Err(err) => {
        eprintln!(
          "Parse error: {src_file}:{}:{}: {err}",
          err.input.location_line(),
          err.input.get_column()
        );
        return Ok(());
      }
    };

  if args.show_ast {
    println!("AST: {parsed_statements:#?}");
  }

  let mut tc_ctx = TypeCheckContext::new();

  if let Err(err) = type_check(&parsed_statements, &mut tc_ctx)
  {
    println!(
      "Type check error: {src_file}:{}:{}: {err}",
      err.span.location_line(),
      err.span.get_column()
    );
    return Ok(());
  }
  println!("Type check OK");

  if !matches!(args.run_mode, RunMode::TypeCheck) {
    let mut frame = StackFrame::new();

    eval_stmts(&parsed_statements, &mut frame);
  }

  Ok(())
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
  F64(f64),
  I64(i64),
  Str(String),
}

impl std::fmt::Display for Value {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    match self {
      Self::F64(v) => write!(f, "{v}"),
      Self::I64(v) => write!(f, "{v}"),
      Self::Str(v) => write!(f, "{v}"),
    }
  }
}

impl PartialOrd for Value {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    use Value::*;
    match (self, other) {
      (F64(lhs), F64(rhs)) => lhs.partial_cmp(rhs),
      (I64(lhs), I64(rhs)) => lhs.partial_cmp(rhs),
      (F64(lhs), I64(rhs)) => lhs.partial_cmp(&(*rhs as f64)),
      (I64(lhs), F64(rhs)) => (*lhs as f64).partial_cmp(rhs),
      (Str(lhs), Str(rhs)) => lhs.partial_cmp(rhs),
      _ => None,
    }
  }
}

impl Value {
  fn as_i64(&self) -> Option<i64> {
    match self {
      Self::F64(val) => Some(*val as i64),
      Self::I64(val) => Some(*val),
      Self::Str(val) => val.parse().ok(),
    }
  }
}

fn coerce_f64(a: &Value) -> f64 {
  match a {
    Value::F64(v) => *v as f64,
    Value::I64(v) => *v as f64,
    _ => panic!("The string could not be parsed as f64"),
  }
}

fn coerce_i64(a: &Value) -> i64 {
  match a {
    Value::F64(v) => *v as i64,
    Value::I64(v) => *v as i64,
    _ => panic!("The string could not be parsed as i64"),
  }
}

fn coerce_str(a: &Value) -> String {
  match a {
    Value::F64(v) => v.to_string(),
    Value::I64(v) => v.to_string(),
    Value::Str(v) => v.clone(),
  }
}

pub(crate) fn binary_op_str(
  lhs: &Value,
  rhs: &Value,
  d: impl Fn(f64, f64) -> f64,
  i: impl Fn(i64, i64) -> i64,
  s: impl Fn(&str, &str) -> String,
) -> Value {
  use Value::*;
  match (lhs, rhs) {
    (F64(lhs), rhs) => F64(d(*lhs, coerce_f64(&rhs))),
    (lhs, F64(rhs)) => F64(d(coerce_f64(&lhs), *rhs)),
    (I64(lhs), I64(rhs)) => I64(i(*lhs, *rhs)),
    (Str(lhs), Str(rhs)) => Str(s(lhs, rhs)),
    _ => {
      panic!(
        "Unsupported operator between {:?} and {:?}",
        lhs, rhs
      )
    }
  }
}

impl std::ops::Add for Value {
  type Output = Value;

  fn add(self, rhs: Self) -> Self::Output {
    binary_op_str(
      &self,
      &rhs,
      |lhs, rhs| lhs + rhs,
      |lhs, rhs| lhs + rhs,
      |lhs, rhs| lhs.to_owned() + rhs,
    )
  }
}

impl std::ops::Sub for Value {
  type Output = Value;

  fn sub(self, rhs: Self) -> Self::Output {
    binary_op_str(
      &self,
      &rhs,
      |lhs, rhs| lhs - rhs,
      |lhs, rhs| lhs - rhs,
      |_, _| panic!("Strings cannot be subtracted"),
    )
  }
}

impl std::ops::Mul for Value {
  type Output = Value;

  fn mul(self, rhs: Self) -> Self::Output {
    binary_op_str(
      &self,
      &rhs,
      |lhs, rhs| lhs * rhs,
      |lhs, rhs| lhs * rhs,
      |_, _| panic!("Strings cannot be multiplied"),
    )
  }
}

impl std::ops::Div for Value {
  type Output = Value;

  fn div(self, rhs: Self) -> Self::Output {
    binary_op_str(
      &self,
      &rhs,
      |lhs, rhs| lhs / rhs,
      |lhs, rhs| lhs / rhs,
      |_, _| panic!("Strings cannot be divided"),
    )
  }
}

type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TypeDecl {
  Any,
  F64,
  I64,
  Str,
}

fn tc_coerce_type<'src>(
  value: &TypeDecl,
  target: &TypeDecl,
  span: Span<'src>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
  use TypeDecl::*;
  Ok(match (value, target) {
    (_, Any) => value.clone(),
    (Any, _) => target.clone(),
    (F64 | I64, F64) => F64,
    (F64, I64) => F64,
    (I64, I64) => I64,
    (Str, Str) => Str,
    _ => {
      return Err(TypeCheckError::new(
        format!(
          "{:?} cannot be assigned to {:?}",
          value, target
        ),
        span,
      ))
    }
  })
}

pub struct TypeCheckContext<'src, 'ctx> {
  /// Variables table for type checking.
  vars: HashMap<&'src str, TypeDecl>,
  /// Function names are owned strings because it can be either from source or native.
  funcs: HashMap<String, FnDef<'src>>,
  super_context: Option<&'ctx TypeCheckContext<'src, 'ctx>>,
}

impl<'src, 'ctx> TypeCheckContext<'src, 'ctx> {
  pub fn new() -> Self {
    Self {
      vars: HashMap::new(),
      funcs: standard_functions(),
      super_context: None,
    }
  }

  fn get_var(&self, name: &str) -> Option<TypeDecl> {
    if let Some(val) = self.vars.get(name) {
      Some(val.clone())
    } else {
      None
    }
  }

  fn get_fn(&self, name: &str) -> Option<&FnDef<'src>> {
    if let Some(val) = self.funcs.get(name) {
      Some(val)
    } else if let Some(super_ctx) = self.super_context {
      super_ctx.get_fn(name)
    } else {
      None
    }
  }

  fn push_stack(super_ctx: &'ctx Self) -> Self {
    Self {
      vars: HashMap::new(),
      funcs: HashMap::new(),
      super_context: Some(super_ctx),
    }
  }
}

#[derive(Debug)]
pub struct TypeCheckError<'src> {
  msg: String,
  span: Span<'src>,
}

impl<'src> std::fmt::Display for TypeCheckError<'src> {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    write!(
      f,
      "{}\nlocation: {}:{}: {}",
      self.msg,
      self.span.location_line(),
      self.span.get_utf8_column(),
      self.span.fragment()
    )
  }
}

impl<'src> TypeCheckError<'src> {
  fn new(msg: String, span: Span<'src>) -> Self {
    Self { msg, span }
  }
}

fn tc_binary_op<'src>(
  lhs: &Expression<'src>,
  rhs: &Expression<'src>,
  ctx: &mut TypeCheckContext<'src, '_>,
  op: &str,
) -> Result<TypeDecl, TypeCheckError<'src>> {
  let lhst = tc_expr(lhs, ctx)?;
  let rhst = tc_expr(rhs, ctx)?;
  binary_op_type(&lhst, &rhst).map_err(|_| {
    TypeCheckError::new(
      format!(
      "Operation {op} between incompatible type: {:?} and {:?}",
      lhst, rhst,
    ),
      lhs.span,
    )
  })
}

fn binary_op_type(
  lhs: &TypeDecl,
  rhs: &TypeDecl,
) -> Result<TypeDecl, ()> {
  use TypeDecl::*;
  Ok(match (lhs, rhs) {
    (Any, _) => Any,
    (_, Any) => Any,
    (I64, I64) => I64,
    (F64 | I64, F64 | I64) => F64,
    (Str, Str) => Str,
    _ => return Err(()),
  })
}

fn tc_binary_cmp<'src>(
  lhs: &Expression<'src>,
  rhs: &Expression<'src>,
  ctx: &mut TypeCheckContext<'src, '_>,
  op: &str,
) -> Result<TypeDecl, TypeCheckError<'src>> {
  use TypeDecl::*;
  let lhst = tc_expr(lhs, ctx)?;
  let rhst = tc_expr(rhs, ctx)?;
  Ok(match (&lhst, &rhst) {
    (Any, _) => I64,
    (_, Any) => I64,
    (F64, F64) => I64,
    (I64, I64) => I64,
    (Str, Str) => I64,
    _ => {
      return Err(TypeCheckError::new(
        format!(
      "Operation {op} between incompatible type: {:?} and {:?}",
      lhst, rhst,
    ),
        lhs.span,
      ))
    }
  })
}

fn tc_expr<'src>(
  e: &Expression<'src>,
  ctx: &mut TypeCheckContext<'src, '_>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
  use ExprEnum::*;
  Ok(match &e.expr {
    NumLiteral(_val) => TypeDecl::F64,
    StrLiteral(_val) => TypeDecl::Str,
    Ident(str) => ctx.get_var(str).ok_or_else(|| {
      TypeCheckError::new(
        format!("Variable \"{}\" not found in scope", str),
        e.span,
      )
    })?,
    FnInvoke(str, args) => {
      let args_ty = args
        .iter()
        .map(|v| Ok((tc_expr(v, ctx)?, v.span)))
        .collect::<Result<Vec<_>, _>>()?;
      let func = ctx.get_fn(**str).ok_or_else(|| {
        TypeCheckError::new(
          format!("function {} is not defined", str),
          *str,
        )
      })?;
      let args_decl = func.args();
      for ((arg_ty, arg_span), decl) in
        args_ty.iter().zip(args_decl.iter())
      {
        tc_coerce_type(&arg_ty, &decl.1, *arg_span)?;
      }
      func.ret_type()
    }
    Add(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Add")?,
    Sub(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Sub")?,
    Mul(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Mult")?,
    Div(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Div")?,
    Lt(lhs, rhs) => tc_binary_cmp(&lhs, &rhs, ctx, "LT")?,
    Gt(lhs, rhs) => tc_binary_cmp(&lhs, &rhs, ctx, "GT")?,
    If(cond, true_branch, false_branch) => {
      tc_coerce_type(
        &tc_expr(cond, ctx)?,
        &TypeDecl::I64,
        cond.span,
      )?;
      let true_type = type_check(true_branch, ctx)?;
      if let Some(false_branch) = false_branch {
        let false_type = type_check(false_branch, ctx)?;
        binary_op_type(&true_type, &false_type).map_err(
          |_| {
            let true_span = true_branch.span();
            let false_span = false_branch.span();
            TypeCheckError::new(
              format!(
                "Conditional expression doesn't have the \
            compatible types in true and false branch: \
            {:?} and {:?}",
                true_type, false_type
              ),
              calc_offset(true_span, false_span),
            )
          },
        )?
      } else {
        true_type
      }
    }
  })
}

fn type_check<'src>(
  stmts: &Vec<Statement<'src>>,
  ctx: &mut TypeCheckContext<'src, '_>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
  let mut res = TypeDecl::Any;
  for stmt in stmts {
    match stmt {
      Statement::VarDef { name, td, ex, .. } => {
        let init_type = tc_expr(ex, ctx)?;
        let init_type =
          tc_coerce_type(&init_type, td, ex.span)?;
        ctx.vars.insert(**name, init_type);
      }
      Statement::VarAssign { name, ex, .. } => {
        let init_type = tc_expr(ex, ctx)?;
        let target =
          ctx.vars.get(**name).expect("Variable not found");
        tc_coerce_type(&init_type, target, ex.span)?;
      }
      Statement::FnDef {
        name,
        args,
        ret_type,
        stmts,
      } => {
        // Function declaration needs to be added first to allow recursive calls
        ctx.funcs.insert(
          name.to_string(),
          FnDef::User(UserFn {
            args: args.clone(),
            ret_type: *ret_type,
            stmts: stmts.clone(),
          }),
        );
        let mut subctx = TypeCheckContext::push_stack(ctx);
        for (arg, ty) in args.iter() {
          subctx.vars.insert(arg, *ty);
        }
        let last_stmt = type_check(stmts, &mut subctx)?;
        tc_coerce_type(&last_stmt, &ret_type, stmts.span())?;
      }
      Statement::Expression(e) => {
        res = tc_expr(&e, ctx)?;
      }
      Statement::For {
        loop_var,
        start,
        end,
        stmts,
        ..
      } => {
        tc_coerce_type(
          &tc_expr(start, ctx)?,
          &TypeDecl::I64,
          start.span,
        )?;
        tc_coerce_type(
          &tc_expr(end, ctx)?,
          &TypeDecl::I64,
          end.span,
        )?;
        ctx.vars.insert(loop_var, TypeDecl::I64);
        res = type_check(stmts, ctx)?;
      }
      Statement::Return(e) => {
        return tc_expr(e, ctx);
      }
      Statement::Break => {
        // TODO: check types in break out site. For now we disallow break with values like Rust.
      }
      Statement::Continue => (),
    }
  }
  Ok(res)
}

enum FnDef<'src> {
  User(UserFn<'src>),
  Native(NativeFn<'src>),
}

impl<'src> FnDef<'src> {
  fn call(&self, args: &[Value], frame: &StackFrame) -> Value {
    match self {
      Self::User(code) => {
        let mut new_frame = StackFrame::push_stack(frame);
        new_frame.vars = args
          .iter()
          .zip(code.args.iter())
          .map(|(arg, decl)| (decl.0.to_string(), arg.clone()))
          .collect();
        match eval_stmts(&code.stmts, &mut new_frame) {
          EvalResult::Continue(val)
          | EvalResult::Break(BreakResult::Return(val)) => val,
          EvalResult::Break(BreakResult::Break) => {
            panic!("Breaking outside loop is prohibited")
          }
          EvalResult::Break(BreakResult::Continue) => {
            panic!("Continuing outside loop is prohibited")
          }
        }
      }
      Self::Native(code) => (code.code)(args),
    }
  }

  fn args(&self) -> Vec<(&'src str, TypeDecl)> {
    match self {
      Self::User(user) => {
        user.args.iter().map(|arg| (&**arg.0, arg.1)).collect()
      }
      Self::Native(code) => code.args.clone(),
    }
  }

  fn ret_type(&self) -> TypeDecl {
    match self {
      Self::User(user) => user.ret_type,
      Self::Native(native) => native.ret_type,
    }
  }
}

struct UserFn<'src> {
  args: Vec<(Span<'src>, TypeDecl)>,
  ret_type: TypeDecl,
  stmts: Statements<'src>,
}

struct NativeFn<'src> {
  args: Vec<(&'src str, TypeDecl)>,
  ret_type: TypeDecl,
  code: Box<dyn Fn(&[Value]) -> Value>,
}

type Variables = HashMap<String, Value>;
type Functions<'src> = HashMap<String, FnDef<'src>>;

struct StackFrame<'src> {
  vars: Variables,
  funcs: Functions<'src>,
  uplevel: Option<&'src StackFrame<'src>>,
}

impl<'src> StackFrame<'src> {
  fn new() -> Self {
    Self {
      vars: Variables::new(),
      funcs: standard_functions(),
      uplevel: None,
    }
  }

  fn push_stack(uplevel: &'src Self) -> Self {
    Self {
      vars: HashMap::new(),
      funcs: HashMap::new(),
      uplevel: Some(uplevel),
    }
  }

  fn get_fn(&self, name: &str) -> Option<&FnDef<'src>> {
    let mut next_frame = Some(self);
    while let Some(frame) = next_frame {
      if let Some(func) = frame.funcs.get(name) {
        return Some(func);
      }
      next_frame = frame.uplevel;
    }
    None
  }
}

fn standard_functions<'src>() -> Functions<'src> {
  let mut funcs = Functions::new();
  funcs.insert("sqrt".to_string(), unary_fn(f64::sqrt));
  funcs.insert("sin".to_string(), unary_fn(f64::sin));
  funcs.insert("cos".to_string(), unary_fn(f64::cos));
  funcs.insert("tan".to_string(), unary_fn(f64::tan));
  funcs.insert("asin".to_string(), unary_fn(f64::asin));
  funcs.insert("acos".to_string(), unary_fn(f64::acos));
  funcs.insert("atan".to_string(), unary_fn(f64::atan));
  funcs.insert("atan2".to_string(), binary_fn(f64::atan2));
  funcs.insert("pow".to_string(), binary_fn(f64::powf));
  funcs.insert("exp".to_string(), unary_fn(f64::exp));
  funcs.insert("log".to_string(), binary_fn(f64::log));
  funcs.insert("log10".to_string(), unary_fn(f64::log10));
  funcs.insert(
    "print".to_string(),
    FnDef::Native(NativeFn {
      args: vec![("arg", TypeDecl::Any)],
      ret_type: TypeDecl::Any,
      code: Box::new(print_fn),
    }),
  );
  funcs.insert(
    "dbg".to_string(),
    FnDef::Native(NativeFn {
      args: vec![("arg", TypeDecl::Any)],
      ret_type: TypeDecl::Any,
      code: Box::new(p_dbg),
    }),
  );
  funcs.insert(
    "puts".to_string(),
    FnDef::Native(NativeFn {
      args: vec![("arg", TypeDecl::Any)],
      ret_type: TypeDecl::Any,
      code: Box::new(puts_fn),
    }),
  );
  funcs.insert(
    "i64".to_string(),
    FnDef::Native(NativeFn {
      args: vec![("arg", TypeDecl::Any)],
      ret_type: TypeDecl::I64,
      code: Box::new(move |args| {
        Value::I64(coerce_i64(
          args.first().expect("function missing argument"),
        ))
      }),
    }),
  );
  funcs.insert(
    "f64".to_string(),
    FnDef::Native(NativeFn {
      args: vec![("arg", TypeDecl::Any)],
      ret_type: TypeDecl::F64,
      code: Box::new(move |args| {
        Value::F64(coerce_f64(
          args.first().expect("function missing argument"),
        ))
      }),
    }),
  );
  funcs.insert(
    "str".to_string(),
    FnDef::Native(NativeFn {
      args: vec![("arg", TypeDecl::Any)],
      ret_type: TypeDecl::Str,
      code: Box::new(move |args| {
        Value::Str(coerce_str(
          args.first().expect("function missing argument"),
        ))
      }),
    }),
  );
  funcs
}

fn print_fn(args: &[Value]) -> Value {
  for arg in args {
    print!("{:?} ", arg);
  }
  println!("");
  Value::F64(0.)
}

fn p_dbg(values: &[Value]) -> Value {
  println!("dbg: {:?}", values[0]);
  Value::I64(0)
}

fn puts_fn(args: &[Value]) -> Value {
  for arg in args {
    print!("{}", arg);
  }
  Value::F64(0.)
}

fn eval_stmts<'src>(
  stmts: &[Statement<'src>],
  frame: &mut StackFrame<'src>,
) -> EvalResult {
  let mut last_result = EvalResult::Continue(Value::I64(0));
  for statement in stmts {
    match statement {
      Statement::Expression(expr) => {
        last_result = EvalResult::Continue(eval(expr, frame)?);
      }
      Statement::VarDef { name, ex, .. } => {
        let value = eval(ex, frame)?;
        frame.vars.insert(name.to_string(), value);
      }
      Statement::VarAssign { name, ex, .. } => {
        if !frame.vars.contains_key(**name) {
          panic!("Variable is not defined");
        }
        let value = eval(ex, frame)?;
        frame.vars.insert(name.to_string(), value);
      }
      Statement::For {
        loop_var,
        start,
        end,
        stmts,
        ..
      } => {
        let start = eval(start, frame)?
          .as_i64()
          .expect("Start needs to be an integer");
        let end = eval(end, frame)?
          .as_i64()
          .expect("End needs to be an integer");
        for i in start..end {
          frame
            .vars
            .insert(loop_var.to_string(), Value::I64(i));
          match eval_stmts(stmts, frame) {
            EvalResult::Continue(val) => {
              last_result = EvalResult::Continue(val)
            }
            EvalResult::Break(BreakResult::Return(val)) => {
              return EvalResult::Break(BreakResult::Return(
                val,
              ))
            }
            EvalResult::Break(BreakResult::Break) => break,
            EvalResult::Break(BreakResult::Continue) => {
              continue
            }
          };
        }
      }
      Statement::FnDef {
        name,
        args,
        ret_type,
        stmts,
      } => {
        frame.funcs.insert(
          name.to_string(),
          FnDef::User(UserFn {
            args: args.clone(),
            ret_type: *ret_type,
            stmts: stmts.clone(),
          }),
        );
      }
      Statement::Return(expr) => {
        return EvalResult::Break(BreakResult::Return(eval(
          expr, frame,
        )?));
      }
      Statement::Break => {
        return EvalResult::Break(BreakResult::Break);
      }
      Statement::Continue => {
        return EvalResult::Break(BreakResult::Continue);
      }
    }
  }
  last_result
}

#[derive(Debug, PartialEq, Clone)]
enum ExprEnum<'src> {
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
}

#[derive(Debug, PartialEq, Clone)]
struct Expression<'a> {
  pub(crate) expr: ExprEnum<'a>,
  pub(crate) span: Span<'a>,
}

impl<'a> Expression<'a> {
  fn new(expr: ExprEnum<'a>, span: Span<'a>) -> Self {
    Self { expr, span }
  }
}

#[derive(Debug, PartialEq, Clone)]
enum Statement<'src> {
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
  FnDef {
    name: Span<'src>,
    args: Vec<(Span<'src>, TypeDecl)>,
    ret_type: TypeDecl,
    stmts: Statements<'src>,
  },
  Return(Expression<'src>),
  Break,
  Continue,
}

impl<'src> Statement<'src> {
  fn span(&self) -> Option<Span<'src>> {
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
      Break | Continue => return None,
    })
  }
}

trait GetSpan<'a> {
  fn span(&self) -> Span<'a>;
}

type Statements<'a> = Vec<Statement<'a>>;

impl<'a> GetSpan<'a> for Statements<'a> {
  fn span(&self) -> Span<'a> {
    self.iter().find_map(|stmt| stmt.span()).unwrap()
  }
}

fn unary_fn<'a>(f: fn(f64) -> f64) -> FnDef<'a> {
  FnDef::Native(NativeFn {
    args: vec![("lhs", TypeDecl::F64), ("rhs", TypeDecl::F64)],
    ret_type: TypeDecl::F64,
    code: Box::new(move |args| {
      Value::F64(f(coerce_f64(
        args
          .into_iter()
          .next()
          .expect("function missing argument"),
      )))
    }),
  })
}

fn binary_fn<'a>(f: fn(f64, f64) -> f64) -> FnDef<'a> {
  FnDef::Native(NativeFn {
    args: vec![("lhs", TypeDecl::F64), ("rhs", TypeDecl::F64)],
    ret_type: TypeDecl::F64,
    code: Box::new(move |args| {
      let mut args = args.into_iter();
      let lhs = coerce_f64(
        args
          .next()
          .expect("function missing the first argument"),
      );
      let rhs = coerce_f64(
        args
          .next()
          .expect("function missing the second argument"),
      );
      Value::F64(f(lhs, rhs))
    }),
  })
}

#[derive(Debug)]
enum BreakResult {
  Return(Value),
  Break,
  Continue,
}

type EvalResult = ControlFlow<BreakResult, Value>;

fn eval<'src>(
  expr: &Expression<'src>,
  frame: &mut StackFrame<'src>,
) -> EvalResult {
  use ExprEnum::*;
  let res = match &expr.expr {
    Ident(id) => {
      if **id == "pi" {
        Value::F64(std::f64::consts::PI)
      } else {
        frame
          .vars
          .get(**id)
          .cloned()
          .expect(&format!("Variable {id:?} not found"))
      }
    }
    NumLiteral(n) => Value::F64(*n),
    StrLiteral(s) => Value::Str(s.clone()),
    FnInvoke(name, args) => {
      let mut arg_vals = vec![];
      for arg in args.iter() {
        arg_vals.push(eval(arg, frame)?);
      }

      if let Some(func) = frame.get_fn(**name) {
        func.call(&arg_vals, frame)
      } else {
        panic!("Unknown function {name:?}");
      }
    }
    Add(lhs, rhs) => eval(lhs, frame)? + eval(rhs, frame)?,
    Sub(lhs, rhs) => eval(lhs, frame)? - eval(rhs, frame)?,
    Mul(lhs, rhs) => eval(lhs, frame)? * eval(rhs, frame)?,
    Div(lhs, rhs) => eval(lhs, frame)? / eval(rhs, frame)?,
    Gt(lhs, rhs) => {
      if eval(lhs, frame)? > eval(rhs, frame)? {
        Value::I64(1)
      } else {
        Value::I64(0)
      }
    }
    Lt(lhs, rhs) => {
      if eval(lhs, frame)? < eval(rhs, frame)? {
        Value::I64(1)
      } else {
        Value::I64(0)
      }
    }
    If(cond, t_case, f_case) => {
      if coerce_i64(&eval(cond, frame)?) != 0 {
        eval_stmts(t_case, frame)?
      } else if let Some(f_case) = f_case {
        eval_stmts(f_case, frame)?
      } else {
        Value::I64(0)
      }
    }
  };
  EvalResult::Continue(res)
}

fn space_delimited<'src, O, E>(
  f: impl Parser<Span<'src>, O, E>,
) -> impl FnMut(Span<'src>) -> IResult<Span<'src>, O, E>
where
  E: ParseError<Span<'src>>,
{
  delimited(multispace0, f, multispace0)
}

/// Calculate offset between the start positions of the input spans and return a span between them.
///
/// Note: `i` shall start earlier than `r`, otherwise wrapping would occur.
fn calc_offset<'a>(i: Span<'a>, r: Span<'a>) -> Span<'a> {
  i.take(i.offset(&r))
}

fn factor(i: Span) -> IResult<Span, Expression> {
  alt((str_literal, num_literal, func_call, ident, parens))(i)
}

fn func_call(i: Span) -> IResult<Span, Expression> {
  let (r, ident) = space_delimited(identifier)(i)?;
  let (r, args) = space_delimited(delimited(
    tag("("),
    many0(delimited(
      multispace0,
      expr,
      space_delimited(opt(tag(","))),
    )),
    tag(")"),
  ))(r)?;
  Ok((
    r,
    Expression {
      expr: ExprEnum::FnInvoke(ident, args),
      span: i,
    },
  ))
}

fn ident(input: Span) -> IResult<Span, Expression> {
  let (r, res) = space_delimited(identifier)(input)?;
  Ok((
    r,
    Expression {
      expr: ExprEnum::Ident(res),
      span: input,
    },
  ))
}

fn identifier(input: Span) -> IResult<Span, Span> {
  recognize(pair(
    alt((alpha1, tag("_"))),
    many0(alt((alphanumeric1, tag("_")))),
  ))(input)
}

fn str_literal(i: Span) -> IResult<Span, Expression> {
  let (r0, _) = preceded(multispace0, char('\"'))(i)?;
  let (r, val) = many0(none_of("\""))(r0)?;
  let (r, _) = terminated(char('"'), multispace0)(r)?;
  Ok((
    r,
    Expression::new(
      ExprEnum::StrLiteral(
        val
          .iter()
          .collect::<String>()
          .replace("\\\\", "\\")
          .replace("\\n", "\n"),
      ),
      i,
    ),
  ))
}

fn num_literal(input: Span) -> IResult<Span, Expression> {
  let (r, v) = space_delimited(recognize_float)(input)?;
  Ok((
    r,
    Expression::new(
      ExprEnum::NumLiteral(v.parse().map_err(|_| {
        nom::Err::Error(nom::error::Error {
          input,
          code: nom::error::ErrorKind::Digit,
        })
      })?),
      v,
    ),
  ))
}

fn parens(i: Span) -> IResult<Span, Expression> {
  space_delimited(delimited(tag("("), expr, tag(")")))(i)
}

fn term(i: Span) -> IResult<Span, Expression> {
  let (r, init) = factor(i)?;

  let res = fold_many0(
    pair(space_delimited(alt((char('*'), char('/')))), factor),
    move || init.clone(),
    |acc, (op, val): (char, Expression)| {
      let span = calc_offset(i, acc.span);
      match op {
        '*' => Expression::new(
          ExprEnum::Mul(Box::new(acc), Box::new(val)),
          span,
        ),
        '/' => Expression::new(
          ExprEnum::Div(Box::new(acc), Box::new(val)),
          span,
        ),
        _ => panic!(
          "Multiplicative expression should have '*' \
            or '/' operator"
        ),
      }
    },
  )(r);
  res
}

fn num_expr(i: Span) -> IResult<Span, Expression> {
  let (r, init) = term(i)?;

  let res = fold_many0(
    pair(space_delimited(alt((char('+'), char('-')))), term),
    move || init.clone(),
    |acc, (op, val): (char, Expression)| {
      let span = calc_offset(i, acc.span);
      match op {
        '+' => Expression::new(
          ExprEnum::Add(Box::new(acc), Box::new(val)),
          span,
        ),
        '-' => Expression::new(
          ExprEnum::Sub(Box::new(acc), Box::new(val)),
          span,
        ),
        _ => panic!(
          "Additive expression should have '+' or '-' operator"
        ),
      }
    },
  )(r);
  res
}

fn cond_expr(i0: Span) -> IResult<Span, Expression> {
  let (i, first) = num_expr(i0)?;
  let (i, cond) =
    space_delimited(alt((char('<'), char('>'))))(i)?;
  let (i, second) = num_expr(i)?;
  let span = calc_offset(i0, i);
  Ok((
    i,
    match cond {
      '<' => Expression::new(
        ExprEnum::Lt(Box::new(first), Box::new(second)),
        span,
      ),
      '>' => Expression::new(
        ExprEnum::Gt(Box::new(first), Box::new(second)),
        span,
      ),
      _ => unreachable!(),
    },
  ))
}

fn open_brace(i: Span) -> IResult<Span, ()> {
  let (i, _) = space_delimited(char('{'))(i)?;
  Ok((i, ()))
}

fn close_brace(i: Span) -> IResult<Span, ()> {
  let (i, _) = space_delimited(char('}'))(i)?;
  Ok((i, ()))
}

fn if_expr(i0: Span) -> IResult<Span, Expression> {
  let (i, _) = space_delimited(tag("if"))(i0)?;
  let (i, cond) = expr(i)?;
  let (i, t_case) =
    delimited(open_brace, statements, close_brace)(i)?;
  let (i, f_case) = opt(preceded(
    space_delimited(tag("else")),
    alt((
      delimited(open_brace, statements, close_brace),
      map_res(
        if_expr,
        |v| -> Result<Vec<Statement>, nom::error::Error<&str>> {
          Ok(vec![Statement::Expression(v)])
        },
      ),
    )),
  ))(i)?;

  Ok((
    i,
    Expression::new(
      ExprEnum::If(
        Box::new(cond),
        Box::new(t_case),
        f_case.map(Box::new),
      ),
      calc_offset(i0, i),
    ),
  ))
}

fn expr(i: Span) -> IResult<Span, Expression> {
  alt((if_expr, cond_expr, num_expr))(i)
}

fn var_def(i: Span) -> IResult<Span, Statement> {
  let span = i;
  let (i, _) =
    delimited(multispace0, tag("var"), multispace1)(i)?;
  let (i, (name, td, ex)) = cut(|i| {
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(char(':'))(i)?;
    let (i, td) = type_decl(i)?;
    let (i, _) = space_delimited(char('='))(i)?;
    let (i, ex) = space_delimited(expr)(i)?;
    let (i, _) = space_delimited(char(';'))(i)?;
    Ok((i, (name, td, ex)))
  })(i)?;
  Ok((
    i,
    Statement::VarDef {
      span: calc_offset(span, i),
      name,
      td,
      ex,
    },
  ))
}

fn var_assign(i: Span) -> IResult<Span, Statement> {
  let span = i;
  let (i, name) = space_delimited(identifier)(i)?;
  let (i, _) = space_delimited(char('='))(i)?;
  let (i, ex) = space_delimited(expr)(i)?;
  let (i, _) = space_delimited(char(';'))(i)?;
  Ok((
    i,
    Statement::VarAssign {
      span: calc_offset(span, i),
      name,
      ex,
    },
  ))
}

fn expr_statement(i: Span) -> IResult<Span, Statement> {
  let (i, res) = expr(i)?;
  Ok((i, Statement::Expression(res)))
}

fn for_statement(i: Span) -> IResult<Span, Statement> {
  let i0 = i;
  let (i, _) = space_delimited(tag("for"))(i)?;
  let (i, (loop_var, start, end, stmts)) = cut(|i| {
    let (i, loop_var) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(tag("in"))(i)?;
    let (i, start) = space_delimited(expr)(i)?;
    let (i, _) = space_delimited(tag("to"))(i)?;
    let (i, end) = space_delimited(expr)(i)?;
    let (i, stmts) =
      delimited(open_brace, statements, close_brace)(i)?;
    Ok((i, (loop_var, start, end, stmts)))
  })(i)?;
  Ok((
    i,
    Statement::For {
      span: calc_offset(i0, i),
      loop_var,
      start,
      end,
      stmts,
    },
  ))
}

fn type_decl(i: Span) -> IResult<Span, TypeDecl> {
  let (i, td) = space_delimited(identifier)(i)?;
  Ok((
    i,
    match *td.fragment() {
      "i64" => TypeDecl::I64,
      "f64" => TypeDecl::F64,
      "str" => TypeDecl::Str,
      _ => {
        return Err(nom::Err::Failure(nom::error::Error::new(
          td,
          nom::error::ErrorKind::Verify,
        )));
      }
    },
  ))
}

fn argument(i: Span) -> IResult<Span, (Span, TypeDecl)> {
  let (i, ident) = space_delimited(identifier)(i)?;
  let (i, _) = char(':')(i)?;
  let (i, td) = type_decl(i)?;

  Ok((i, (ident, td)))
}

fn fn_def_statement(i: Span) -> IResult<Span, Statement> {
  let (i, _) = space_delimited(tag("fn"))(i)?;
  let (i, (name, args, ret_type, stmts)) = cut(|i| {
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(tag("("))(i)?;
    let (i, args) =
      separated_list0(char(','), space_delimited(argument))(i)?;
    let (i, _) = space_delimited(tag(")"))(i)?;
    let (i, _) = space_delimited(tag("->"))(i)?;
    let (i, ret_type) = type_decl(i)?;
    let (i, stmts) =
      delimited(open_brace, statements, close_brace)(i)?;
    Ok((i, (name, args, ret_type, stmts)))
  })(i)?;
  Ok((
    i,
    Statement::FnDef {
      name,
      args,
      ret_type,
      stmts,
    },
  ))
}

fn return_statement(i: Span) -> IResult<Span, Statement> {
  let (i, _) = space_delimited(tag("return"))(i)?;
  let (i, ex) = space_delimited(expr)(i)?;
  Ok((i, Statement::Return(ex)))
}

fn break_statement(i: Span) -> IResult<Span, Statement> {
  let (i, _) = space_delimited(tag("break"))(i)?;
  Ok((i, Statement::Break))
}

fn continue_statement(i: Span) -> IResult<Span, Statement> {
  let (i, _) = space_delimited(tag("continue"))(i)?;
  Ok((i, Statement::Continue))
}

fn general_statement<'a>(
  last: bool,
) -> impl Fn(Span<'a>) -> IResult<Span<'a>, Statement> {
  let terminator = move |i| -> IResult<Span, ()> {
    let mut semicolon = pair(tag(";"), multispace0);
    if last {
      Ok((opt(semicolon)(i)?.0, ()))
    } else {
      Ok((semicolon(i)?.0, ()))
    }
  };
  move |input| {
    alt((
      var_def,
      var_assign,
      fn_def_statement,
      for_statement,
      terminated(return_statement, terminator),
      terminated(break_statement, terminator),
      terminated(continue_statement, terminator),
      terminated(expr_statement, terminator),
    ))(input)
  }
}

pub(crate) fn last_statement(
  input: Span,
) -> IResult<Span, Statement> {
  general_statement(true)(input)
}

pub(crate) fn statement(
  input: Span,
) -> IResult<Span, Statement> {
  general_statement(false)(input)
}

fn statements(i: Span) -> IResult<Span, Statements> {
  let (i, mut stmts) = many0(statement)(i)?;
  let (i, last) = opt(last_statement)(i)?;
  let (i, _) = opt(multispace0)(i)?;
  if let Some(last) = last {
    stmts.push(last);
  }
  Ok((i, stmts))
}

fn statements_finish(
  i: Span,
) -> Result<Statements, nom::error::Error<Span>> {
  let (_, res) = statements(i).finish()?;
  Ok(res)
}
