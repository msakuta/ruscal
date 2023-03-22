use std::io::Read;

use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::{
    alpha1, alphanumeric1, char, multispace0,
  },
  combinator::{opt, recognize},
  error::ParseError,
  multi::{fold_many0, many0, separated_list0},
  number::complete::recognize_float,
  sequence::{delimited, pair},
  Finish, IResult, Parser,
};

fn main() {
  let mut buf = String::new();
  if std::io::stdin().read_to_string(&mut buf).is_ok() {
    let parsed_statements = match statements(&buf) {
      Ok(parsed_statements) => parsed_statements,
      Err(e) => {
        eprintln!("Parse error: {e:?}");
        return;
      }
    };

    for statement in parsed_statements {
      println!("eval: {:?}", eval(statement));
    }
  }
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
}

type Statements<'a> = Vec<Expression<'a>>;

fn unary_fn(
  f: fn(f64) -> f64,
) -> impl Fn(Vec<Expression>) -> f64 {
  move |args| {
    f(eval(
      args
        .into_iter()
        .next()
        .expect("function missing argument"),
    ))
  }
}

fn binary_fn(
  f: fn(f64, f64) -> f64,
) -> impl Fn(Vec<Expression>) -> f64 {
  move |args| {
    let mut args = args.into_iter();
    let lhs = eval(
      args.next().expect("function missing the first argument"),
    );
    let rhs = eval(
      args
        .next()
        .expect("function missing the second argument"),
    );
    f(lhs, rhs)
  }
}

fn eval(expr: Expression) -> f64 {
  use Expression::*;
  match expr {
    Ident("pi") => std::f64::consts::PI,
    Ident(id) => panic!("Unknown name {:?}", id),
    NumLiteral(n) => n,
    FnInvoke("sqrt", args) => unary_fn(f64::sqrt)(args),
    FnInvoke("sin", args) => unary_fn(f64::sin)(args),
    FnInvoke("cos", args) => unary_fn(f64::cos)(args),
    FnInvoke("tan", args) => unary_fn(f64::tan)(args),
    FnInvoke("asin", args) => unary_fn(f64::asin)(args),
    FnInvoke("acos", args) => unary_fn(f64::acos)(args),
    FnInvoke("atan", args) => unary_fn(f64::atan)(args),
    FnInvoke("atan2", args) => binary_fn(f64::atan2)(args),
    FnInvoke("pow", args) => binary_fn(f64::powf)(args),
    FnInvoke("exp", args) => unary_fn(f64::exp)(args),
    FnInvoke("log", args) => binary_fn(f64::log)(args),
    FnInvoke("log10", args) => unary_fn(f64::log10)(args),
    FnInvoke(name, _) => {
      panic!("Unknown function {name:?}")
    }
    Add(lhs, rhs) => eval(*lhs) + eval(*rhs),
    Sub(lhs, rhs) => eval(*lhs) - eval(*rhs),
    Mul(lhs, rhs) => eval(*lhs) * eval(*rhs),
    Div(lhs, rhs) => eval(*lhs) / eval(*rhs),
  }
}

fn space_delimited<'src, O, E>(
  f: impl Parser<&'src str, O, E>,
) -> impl FnMut(&'src str) -> IResult<&'src str, O, E>
where
  E: ParseError<&'src str>,
{
  delimited(multispace0, f, multispace0)
}

fn factor(i: &str) -> IResult<&str, Expression> {
  alt((number, func_call, ident, parens))(i)
}

fn func_call(i: &str) -> IResult<&str, Expression> {
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
  Ok((r, Expression::FnInvoke(ident, args)))
}

fn ident(input: &str) -> IResult<&str, Expression> {
  let (r, res) = space_delimited(identifier)(input)?;
  Ok((r, Expression::Ident(res)))
}

fn identifier(input: &str) -> IResult<&str, &str> {
  recognize(pair(
    alt((alpha1, tag("_"))),
    many0(alt((alphanumeric1, tag("_")))),
  ))(input)
}

fn number(input: &str) -> IResult<&str, Expression> {
  let (r, v) = space_delimited(recognize_float)(input)?;
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
  space_delimited(delimited(tag("("), expr, tag(")")))(i)
}

fn term(i: &str) -> IResult<&str, Expression> {
  let (i, init) = factor(i)?;

  fold_many0(
    pair(space_delimited(alt((char('*'), char('/')))), factor),
    move || init.clone(),
    |acc, (op, val): (char, Expression)| {
      match op {
        '*' => Expression::Mul(Box::new(acc), Box::new(val)),
        '/' => Expression::Div(Box::new(acc), Box::new(val)),
        _ => panic!(
            "Multiplicative expression should have '*' or '/' operator"
        ),
      }
    },
  )(i)
}

fn expr(i: &str) -> IResult<&str, Expression> {
  let (i, init) = term(i)?;

  fold_many0(
    pair(space_delimited(alt((char('+'), char('-')))), term),
    move || init.clone(),
    |acc, (op, val): (char, Expression)| match op {
      '+' => Expression::Add(Box::new(acc), Box::new(val)),
      '-' => Expression::Sub(Box::new(acc), Box::new(val)),
      _ => panic!(
        "Additive expression should have '+' or '-' operator"
      ),
    },
  )(i)
}

fn statements(
  i: &str,
) -> Result<Statements, nom::error::Error<&str>> {
  let (_, res) = separated_list0(tag(";"), expr)(i).finish()?;
  Ok(res)
}
