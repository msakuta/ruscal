use std::{collections::HashMap, io::Read, ops::ControlFlow};

use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::{
    alpha1, alphanumeric1, char, multispace0, multispace1,
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

enum FnDef<'src> {
  User(UserFn<'src>),
  Native(NativeFn),
}

impl<'src> FnDef<'src> {
  fn call(&self, args: &[f64], frame: &StackFrame) -> f64 {
    match self {
      Self::User(code) => {
        let mut new_frame = StackFrame::push_stack(frame);
        new_frame.vars = args
          .iter()
          .zip(code.args.iter())
          .map(|(arg, name)| (name.to_string(), *arg))
          .collect();
        match eval_stmts(&code.stmts, &mut new_frame) {
          EvalResult::Continue(val)
          | EvalResult::Break(val) => val,
        }
      }
      Self::Native(code) => (code.code)(args),
    }
  }
}

struct UserFn<'src> {
  args: Vec<&'src str>,
  stmts: Statements<'src>,
}

struct NativeFn {
  code: Box<dyn Fn(&[f64]) -> f64>,
}

type Variables = HashMap<String, f64>;
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
      unary_fn(|arg| {
        println!("print: {arg}");
        0.
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
  let mut last_result = EvalResult::Continue(0.);
  for statement in stmts {
    match statement {
      Statement::Expression(expr) => {
        last_result = EvalResult::Continue(eval(expr, frame)?);
      }
      Statement::VarDef(name, expr) => {
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
        let start = eval(start, frame)? as isize;
        let end = eval(end, frame)? as isize;
        for i in start..end {
          frame.vars.insert(loop_var.to_string(), i as f64);
          eval_stmts(stmts, frame)?;
        }
      }
      Statement::FnDef { name, args, stmts } => {
        frame.funcs.insert(
          name.to_string(),
          FnDef::User(UserFn {
            args: args.clone(),
            stmts: stmts.clone(),
          }),
        );
      }
      Statement::Return(expr) => {
        return EvalResult::Break(eval(expr, frame)?);
      }
    }
  }
  last_result
}

#[derive(Debug, PartialEq, Clone)]
enum Expression<'src> {
  Ident(&'src str),
  NumLiteral(f64),
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
  VarDef(&'src str, Expression<'src>),
  VarAssign(&'src str, Expression<'src>),
  For {
    loop_var: &'src str,
    start: Expression<'src>,
    end: Expression<'src>,
    stmts: Statements<'src>,
  },
  FnDef {
    name: &'src str,
    args: Vec<&'src str>,
    stmts: Statements<'src>,
  },
  Return(Expression<'src>),
}

type Statements<'a> = Vec<Statement<'a>>;

fn unary_fn<'a>(f: fn(f64) -> f64) -> FnDef<'a> {
  FnDef::Native(NativeFn {
    code: Box::new(move |args| {
      f(*args
        .into_iter()
        .next()
        .expect("function missing argument"))
    }),
  })
}

fn binary_fn<'a>(f: fn(f64, f64) -> f64) -> FnDef<'a> {
  FnDef::Native(NativeFn {
    code: Box::new(move |args| {
      let mut args = args.into_iter();
      let lhs = args
        .next()
        .expect("function missing the first argument");
      let rhs = args
        .next()
        .expect("function missing the second argument");
      f(*lhs, *rhs)
    }),
  })
}

// #[derive(Debug)]
// enum EvalResult {
//     Yield(f64),
//     Return(f64),
// }

type EvalResult = ControlFlow<f64, f64>;

fn eval<'src>(
  expr: &Expression<'src>,
  frame: &mut StackFrame<'src>,
) -> EvalResult {
  let res = match expr {
    Expression::Ident("pi") => std::f64::consts::PI,
    Expression::Ident(id) => {
      *frame.vars.get(*id).expect("Variable not found")
    }
    Expression::NumLiteral(n) => *n,
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
        1.
      } else {
        0.
      }
    }
    Expression::Lt(lhs, rhs) => {
      if eval(lhs, frame)? < eval(rhs, frame)? {
        1.
      } else {
        0.
      }
    }
    Expression::If(cond, t_case, f_case) => {
      if eval(cond, frame)? != 0. {
        eval_stmts(t_case, frame)?
      } else if let Some(f_case) = f_case {
        eval_stmts(f_case, frame)?
      } else {
        0.
      }
    }
  };
  EvalResult::Continue(res)
}

fn factor(i: &str) -> IResult<&str, Expression> {
  alt((number, func_call, ident, parens))(i)
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

fn number(input: &str) -> IResult<&str, Expression> {
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
    delimited(multispace0, char(';'), multispace0)(i)?;
  Ok((i, Statement::VarDef(name, expr)))
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

fn fn_def_statement(i: &str) -> IResult<&str, Statement> {
  let (i, _) =
    delimited(multispace0, tag("fn"), multispace0)(i)?;
  let (i, name) =
    delimited(multispace0, identifier, multispace0)(i)?;
  let (i, _) =
    delimited(multispace0, tag("("), multispace0)(i)?;
  let (i, args) = separated_list0(
    char(','),
    delimited(multispace0, identifier, multispace0),
  )(i)?;
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
