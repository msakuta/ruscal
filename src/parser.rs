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

use crate::ast::{
  ExprEnum, Expression, Span, Statement, Statements, TypeDecl,
};

pub trait GetSpan<'a> {
  fn span(&self) -> Span<'a>;
}

impl<'a> GetSpan<'a> for Statements<'a> {
  fn span(&self) -> Span<'a> {
    self.iter().find_map(|stmt| stmt.span()).unwrap()
  }
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
pub(crate) fn calc_offset<'a>(
  i: Span<'a>,
  r: Span<'a>,
) -> Span<'a> {
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

fn await_expr(i: Span) -> IResult<Span, Expression> {
  let i0 = i;
  let (i, _) = space_delimited(tag("await"))(i)?;
  let (i, ex) = cut(space_delimited(expr))(i)?;
  Ok((
    i,
    Expression::new(
      ExprEnum::Await(Box::new(ex)),
      calc_offset(i0, i),
    ),
  ))
}

fn expr(i: Span) -> IResult<Span, Expression> {
  alt((await_expr, if_expr, cond_expr, num_expr))(i)
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
      "cofn" => TypeDecl::Coro,
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
  let (i, fn_kw) =
    space_delimited(alt((tag("cofn"), tag("fn"))))(i)?;
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
      cofn: *fn_kw == "cofn",
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

fn yield_statement(i: Span) -> IResult<Span, Statement> {
  let (i, _) = space_delimited(tag("yield"))(i)?;
  let (i, ex) = cut(space_delimited(expr))(i)?;
  Ok((i, Statement::Yield(ex)))
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
      terminated(yield_statement, terminator),
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

pub fn statements_finish(
  i: Span,
) -> Result<Statements, nom::error::Error<Span>> {
  let (_, res) = statements(i).finish()?;
  Ok(res)
}
