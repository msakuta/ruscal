use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::{
    alpha1, alphanumeric1, char, multispace0,
  },
  combinator::recognize,
  multi::{fold_many0, many0},
  number::complete::recognize_float,
  sequence::{delimited, pair},
  IResult,
};

fn main() {
  fn ex_eval<'src>(
    input: &'src str,
  ) -> Result<f64, nom::Err<nom::error::Error<&'src str>>> {
    expr(input).map(|(_, e)| eval(e))
  }

  let input = "123";
  println!("source: {:?}, parsed: {:?}", input, ex_eval(input));

  let input = "(123 + 456 ) + pi";
  println!("source: {:?}, parsed: {:?}", input, ex_eval(input));

  let input = "10 + (100 + 1)";
  println!("source: {:?}, parsed: {:?}", input, ex_eval(input));

  let input = "((1 + 2) + (3 + 4)) + 5 + 6";
  println!("source: {:?}, parsed: {:?}", input, ex_eval(input));
}

#[derive(Debug, PartialEq, Clone)]
enum Expression<'src> {
  Ident(&'src str),
  NumLiteral(f64),
  Add(Box<Expression<'src>>, Box<Expression<'src>>),
}

fn eval(expr: Expression) -> f64 {
  match expr {
    Expression::Ident("pi") => std::f64::consts::PI,
    Expression::Ident(id) => panic!("Unknown name {:?}", id),
    Expression::NumLiteral(n) => n,
    Expression::Add(lhs, rhs) => eval(*lhs) + eval(*rhs),
  }
}

fn term(i: &str) -> IResult<&str, Expression> {
  alt((number, ident, parens))(i)
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

fn expr(i: &str) -> IResult<&str, Expression> {
  let (i, init) = term(i)?;

  fold_many0(
    pair(delimited(multispace0, char('+'), multispace0), term),
    move || init.clone(),
    |acc, (_op, val): (char, Expression)| {
      Expression::Add(Box::new(acc), Box::new(val))
    },
  )(i)
}
