use std::{collections::HashMap, io::Read, ops::ControlFlow};

use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::{
    alpha1, alphanumeric1, char, multispace0, multispace1,
    none_of,
  },
  combinator::{opt, recognize},
  multi::{fold_many0, many0, separated_list0},
  number::complete::recognize_float,
  sequence::{delimited, pair, preceded, terminated},
  Finish, IResult,
};

fn main() {
  let mut buf = String::new();
  if !std::io::stdin().read_to_string(&mut buf).is_ok() {
    panic!("Failed to read from stdin");
  }
  let parsed_statements = match statements_finish(&buf) {
    Ok(parsed_statements) => parsed_statements,
    Err(e) => {
      eprintln!("Parse error: {e:?}");
      return;
    }
  };

  let mut frame = StackFrame::new();

  eval_stmts(&parsed_statements, &mut frame);
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum TypeDecl {
  Any,
  F64,
  I64,
  Str,
}

fn tc_coerce_type<'src>(
  value: &TypeDecl,
  target: &TypeDecl,
  ctx: &TypeCheckContext<'src>,
) -> Result<TypeDecl, TypeCheckError> {
  use TypeDecl::*;
  Ok(match (value, target) {
    (_, Any) => value.clone(),
    (Any, _) => target.clone(),
    (F64 | I64, F64) => F64,
    (F64, F64 | I64) => F64,
    (I64, I64) => I64,
    (Str, Str) => Str,
    _ => {
      return Err(TypeCheckError::new(format!(
        "Type check error! {:?} cannot be assigned to {:?}",
        value, target
      )))
    }
  })
}

pub struct TypeCheckContext<'src> {
  /// Variables table for type checking.
  vars: HashMap<&'src str, TypeDecl>,
  /// Function names are owned strings because it can be either from source or native.
  funcs: HashMap<String, FnDef<'src>>,
  super_context: Option<&'src TypeCheckContext<'src>>,
}

impl<'src> TypeCheckContext<'src> {
  pub fn new(source_file: Option<&'src str>) -> Self {
    Self {
      vars: HashMap::new(),
      funcs: HashMap::new(),
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
}

#[derive(Debug)]
pub struct TypeCheckError {
  msg: String,
}

impl<'src> std::fmt::Display for TypeCheckError {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    write!(f, "{}", self.msg,)
  }
}

impl TypeCheckError {
  fn new(msg: String) -> Self {
    Self { msg }
  }
}

fn tc_expr<'src, 'b>(
  e: &'b Expression<'src>,
  ctx: &mut TypeCheckContext<'src>,
) -> Result<TypeDecl, TypeCheckError> {
  Ok(match &e {
    Expression::NumLiteral(_val) => TypeDecl::F64,
    Expression::StrLiteral(_val) => TypeDecl::Str,
    Expression::Ident(str) => {
      ctx.get_var(str).ok_or_else(|| {
        TypeCheckError::new(format!(
          "Variable {} not found in scope",
          str
        ))
      })?
    }
    _ => todo!(),
  })
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

  fn args(&self) -> &Vec<(&'src str, TypeDecl)> {
    match self {
      Self::User(user) => &user.args,
      Self::Native(code) => &code.args,
    }
  }

  fn ret_type(&self) -> &TypeDecl {
    match self {
      Self::User(user) => &user.ret_type,
      Self::Native(native) => &native.ret_type,
    }
  }
}

struct UserFn<'src> {
  args: Vec<(&'src str, TypeDecl)>,
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
        code: Box::new(move |args| {
          let val =
            args.first().expect("function missing argument");
          println!("print: {val}");
          Value::I64(0)
        }),
      }),
    );
    funcs.insert(
      "dbg".to_string(),
      FnDef::Native(NativeFn {
        args: vec![("arg", TypeDecl::Any)],
        ret_type: TypeDecl::Any,
        code: Box::new(move |args| {
          let val =
            args.first().expect("function missing argument");
          println!("dbg: {val:?}");
          Value::I64(0)
        }),
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

    Self {
      vars: Variables::new(),
      funcs,
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
      Statement::VarDef(name, _td, expr) => {
        let value = eval(expr, frame)?;
        frame.vars.insert(name.to_string(), value);
      }
      Statement::VarAssign(name, expr) => {
        if !frame.vars.contains_key(*name) {
          panic!("Variable is not defined");
        }
        let value = eval(expr, frame)?;
        frame.vars.insert(name.to_string(), value);
      }
      Statement::For {
        loop_var,
        start,
        end,
        stmts,
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
      Statement::FnDef { name, args, stmts } => {
        frame.funcs.insert(
          name.to_string(),
          FnDef::User(UserFn {
            args: args.clone(),
            ret_type: TypeDecl::Any,
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
enum Expression<'src> {
  Ident(&'src str),
  NumLiteral(f64),
  StrLiteral(String),
  FnInvoke(&'src str, Vec<Expression<'src>>),
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
enum Statement<'src> {
  Expression(Expression<'src>),
  VarDef(&'src str, TypeDecl, Expression<'src>),
  VarAssign(&'src str, Expression<'src>),
  For {
    loop_var: &'src str,
    start: Expression<'src>,
    end: Expression<'src>,
    stmts: Statements<'src>,
  },
  FnDef {
    name: &'src str,
    args: Vec<(&'src str, TypeDecl)>,
    stmts: Statements<'src>,
  },
  Return(Expression<'src>),
  Break,
  Continue,
}

type Statements<'a> = Vec<Statement<'a>>;

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
    args: vec![("arg", TypeDecl::F64)],
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
  let res = match expr {
    Expression::Ident("pi") => Value::F64(std::f64::consts::PI),
    Expression::Ident(id) => {
      frame.vars.get(*id).cloned().expect("Variable not found")
    }
    Expression::NumLiteral(n) => Value::F64(*n),
    Expression::StrLiteral(s) => Value::Str(s.clone()),
    Expression::FnInvoke(name, args) => {
      let mut arg_vals = vec![];
      for arg in args.iter() {
        arg_vals.push(eval(arg, frame)?);
      }

      if let Some(func) = frame.get_fn(*name) {
        func.call(&arg_vals, frame)
      } else {
        panic!("Unknown function {name:?}");
      }
    }
    Expression::Add(lhs, rhs) => {
      eval(lhs, frame)? + eval(rhs, frame)?
    }
    Expression::Sub(lhs, rhs) => {
      eval(lhs, frame)? - eval(rhs, frame)?
    }
    Expression::Mul(lhs, rhs) => {
      eval(lhs, frame)? * eval(rhs, frame)?
    }
    Expression::Div(lhs, rhs) => {
      eval(lhs, frame)? / eval(rhs, frame)?
    }
    Expression::Gt(lhs, rhs) => {
      if eval(lhs, frame)? > eval(rhs, frame)? {
        Value::I64(1)
      } else {
        Value::I64(0)
      }
    }
    Expression::Lt(lhs, rhs) => {
      if eval(lhs, frame)? < eval(rhs, frame)? {
        Value::I64(1)
      } else {
        Value::I64(0)
      }
    }
    Expression::If(cond, t_case, f_case) => {
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

fn factor(i: &str) -> IResult<&str, Expression> {
  alt((str_literal, num_literal, func_call, ident, parens))(i)
}

fn func_call(i: &str) -> IResult<&str, Expression> {
  let (r, ident) =
    delimited(multispace0, identifier, multispace0)(i)?;
  // println!("func_invoke ident: {}", ident);
  let (r, args) = delimited(
    multispace0,
    delimited(
      tag("("),
      many0(delimited(
        multispace0,
        expr,
        delimited(multispace0, opt(tag(",")), multispace0),
      )),
      tag(")"),
    ),
    multispace0,
  )(r)?;
  Ok((r, Expression::FnInvoke(ident, args)))
}

fn ident(input: &str) -> IResult<&str, Expression> {
  let (r, res) =
    delimited(multispace0, identifier, multispace0)(input)?;
  Ok((r, Expression::Ident(res)))
}

fn identifier(input: &str) -> IResult<&str, &str> {
  recognize(pair(
    alt((alpha1, tag("_"))),
    many0(alt((alphanumeric1, tag("_")))),
  ))(input)
}

fn str_literal(i: &str) -> IResult<&str, Expression> {
  let (r0, _) = preceded(multispace0, char('\"'))(i)?;
  let (r, val) = many0(none_of("\""))(r0)?;
  let (r, _) = terminated(char('"'), multispace0)(r)?;
  Ok((
    r,
    Expression::StrLiteral(
      val
        .iter()
        .collect::<String>()
        .replace("\\\\", "\\")
        .replace("\\n", "\n"),
    ),
  ))
}

fn num_literal(input: &str) -> IResult<&str, Expression> {
  let (r, v) =
    delimited(multispace0, recognize_float, multispace0)(
      input,
    )?;
  Ok((
    r,
    Expression::NumLiteral(v.parse().map_err(|_| {
      nom::Err::Error(nom::error::Error {
        input,
        code: nom::error::ErrorKind::Digit,
      })
    })?),
  ))
}

fn parens(i: &str) -> IResult<&str, Expression> {
  delimited(
    multispace0,
    delimited(tag("("), expr, tag(")")),
    multispace0,
  )(i)
}

fn term(i: &str) -> IResult<&str, Expression> {
  let (i, init) = factor(i)?;

  fold_many0(
    pair(
      delimited(
        multispace0,
        alt((char('*'), char('/'))),
        multispace0,
      ),
      factor,
    ),
    move || init.clone(),
    |acc, (op, val): (char, Expression)| match op {
      '*' => Expression::Mul(Box::new(acc), Box::new(val)),
      '/' => Expression::Div(Box::new(acc), Box::new(val)),
      _ => {
        panic!("Multiplicative expression should have '*' or '/' operator")
      }
    },
  )(i)
}

fn num_expr(i: &str) -> IResult<&str, Expression> {
  let (i, init) = term(i)?;

  fold_many0(
    pair(
      delimited(
        multispace0,
        alt((char('+'), char('-'))),
        multispace0,
      ),
      term,
    ),
    move || init.clone(),
    |acc, (op, val): (char, Expression)| match op {
      '+' => Expression::Add(Box::new(acc), Box::new(val)),
      '-' => Expression::Sub(Box::new(acc), Box::new(val)),
      _ => {
        panic!(
          "Additive expression should have '+' or '-' operator"
        )
      }
    },
  )(i)
}

fn cond_expr(i: &str) -> IResult<&str, Expression> {
  let (i, first) = num_expr(i)?;
  let (i, cond) = delimited(
    multispace0,
    alt((char('<'), char('>'))),
    multispace0,
  )(i)?;
  let (i, second) = num_expr(i)?;
  Ok((
    i,
    match cond {
      '<' => Expression::Lt(Box::new(first), Box::new(second)),
      '>' => Expression::Gt(Box::new(first), Box::new(second)),
      _ => unreachable!(),
    },
  ))
}

fn open_brace(i: &str) -> IResult<&str, ()> {
  let (i, _) =
    delimited(multispace0, char('{'), multispace0)(i)?;
  Ok((i, ()))
}

fn close_brace(i: &str) -> IResult<&str, ()> {
  let (i, _) =
    delimited(multispace0, char('}'), multispace0)(i)?;
  Ok((i, ()))
}

fn if_expr(i: &str) -> IResult<&str, Expression> {
  let (i, _) =
    delimited(multispace0, tag("if"), multispace0)(i)?;
  let (i, cond) = expr(i)?;
  let (i, t_case) =
    delimited(open_brace, statements, close_brace)(i)?;
  let (i, f_case) = opt(preceded(
    delimited(multispace0, tag("else"), multispace0),
    delimited(open_brace, statements, close_brace),
  ))(i)?;

  Ok((
    i,
    Expression::If(
      Box::new(cond),
      Box::new(t_case),
      f_case.map(Box::new),
    ),
  ))
}

fn expr(i: &str) -> IResult<&str, Expression> {
  alt((if_expr, cond_expr, num_expr))(i)
}

fn var_def(i: &str) -> IResult<&str, Statement> {
  let (i, _) =
    delimited(multispace0, tag("var"), multispace1)(i)?;
  let (i, name) =
    delimited(multispace0, identifier, multispace0)(i)?;
  let (i, _) =
    delimited(multispace0, char('='), multispace0)(i)?;
  let (i, expr) = delimited(multispace0, expr, multispace0)(i)?;
  let (i, _) =
    delimited(multispace0, char(':'), multispace0)(i)?;
  let (i, td) = type_decl(i)?;
  let (i, _) =
    delimited(multispace0, char(';'), multispace0)(i)?;
  Ok((i, Statement::VarDef(name, td, expr)))
}

fn var_assign(i: &str) -> IResult<&str, Statement> {
  let (i, name) =
    delimited(multispace0, identifier, multispace0)(i)?;
  let (i, _) =
    delimited(multispace0, char('='), multispace0)(i)?;
  let (i, expr) = delimited(multispace0, expr, multispace0)(i)?;
  Ok((i, Statement::VarAssign(name, expr)))
}

fn expr_statement(i: &str) -> IResult<&str, Statement> {
  let (i, res) = expr(i)?;
  Ok((i, Statement::Expression(res)))
}

fn for_statement(i: &str) -> IResult<&str, Statement> {
  let (i, _) =
    delimited(multispace0, tag("for"), multispace0)(i)?;
  let (i, loop_var) =
    delimited(multispace0, identifier, multispace0)(i)?;
  let (i, _) =
    delimited(multispace0, tag("in"), multispace0)(i)?;
  let (i, start) =
    delimited(multispace0, expr, multispace0)(i)?;
  let (i, _) =
    delimited(multispace0, tag("to"), multispace0)(i)?;
  let (i, end) = delimited(multispace0, expr, multispace0)(i)?;
  let (i, stmts) =
    delimited(open_brace, statements, close_brace)(i)?;
  Ok((
    i,
    Statement::For {
      loop_var,
      start,
      end,
      stmts,
    },
  ))
}

fn type_decl(i: &str) -> IResult<&str, TypeDecl> {
  let (i, td) =
    delimited(multispace0, identifier, multispace0)(i)?;
  Ok((
    i,
    match td {
      "i64" => TypeDecl::I64,
      "f64" => TypeDecl::F64,
      "str" => TypeDecl::Str,
      _ => {
        panic!("Type annotation has unknown type: {td}")
      }
    },
  ))
}

fn argument(i: &str) -> IResult<&str, (&str, TypeDecl)> {
  let (i, _) = multispace0(i)?;
  let (i, ident) = identifier(i)?;
  let (i, _) = multispace0(i)?;
  let (i, _) = char(':')(i)?;
  let (i, td) = type_decl(i)?;

  Ok((i, (ident, td)))
}

fn fn_def_statement(i: &str) -> IResult<&str, Statement> {
  let (i, _) =
    delimited(multispace0, tag("fn"), multispace0)(i)?;
  let (i, name) =
    delimited(multispace0, identifier, multispace0)(i)?;
  let (i, _) =
    delimited(multispace0, tag("("), multispace0)(i)?;
  let (i, args) = separated_list0(char(','), argument)(i)?;
  let (i, _) =
    delimited(multispace0, tag(")"), multispace0)(i)?;
  let (i, stmts) =
    delimited(open_brace, statements, close_brace)(i)?;
  Ok((i, Statement::FnDef { name, args, stmts }))
}

fn return_statement(i: &str) -> IResult<&str, Statement> {
  let (i, _) =
    delimited(multispace0, tag("return"), multispace0)(i)?;
  let (i, ex) = delimited(multispace0, expr, multispace0)(i)?;
  Ok((i, Statement::Return(ex)))
}

fn break_statement(i: &str) -> IResult<&str, Statement> {
  let (i, _) =
    delimited(multispace0, tag("break"), multispace0)(i)?;
  Ok((i, Statement::Break))
}

fn continue_statement(i: &str) -> IResult<&str, Statement> {
  let (i, _) =
    delimited(multispace0, tag("continue"), multispace0)(i)?;
  Ok((i, Statement::Continue))
}

fn general_statement<'a>(
  last: bool,
) -> impl Fn(&'a str) -> IResult<&'a str, Statement> {
  let terminator = move |i| -> IResult<&str, ()> {
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
  input: &str,
) -> IResult<&str, Statement> {
  general_statement(true)(input)
}

pub(crate) fn statement(
  input: &str,
) -> IResult<&str, Statement> {
  general_statement(false)(input)
}

fn statements(i: &str) -> IResult<&str, Statements> {
  let (i, mut stmts) = many0(statement)(i)?;
  let (i, last) = opt(last_statement)(i)?;
  let (i, _) = opt(multispace0)(i)?;
  if let Some(last) = last {
    stmts.push(last);
  }
  Ok((i, stmts))
}

fn statements_finish(
  i: &str,
) -> Result<Statements, nom::error::Error<&str>> {
  let (_, res) = statements(i).finish()?;
  Ok(res)
}
