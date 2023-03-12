use std::{collections::HashMap, io::Read};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{
        alpha1, alphanumeric1, char, multispace0, multispace1,
    },
    combinator::{opt, recognize},
    multi::{fold_many0, many0, separated_list0},
    number::complete::recognize_float,
    sequence::{delimited, pair, preceded},
    Finish, IResult,
};

fn main() {
    let mut buf = String::new();
    if !std::io::stdin().read_to_string(&mut buf).is_ok() {
        panic!("Failed to read from stdin");
    }
    let parsed_statements = match statements(&buf) {
        Ok(parsed_statements) => parsed_statements,
        Err(e) => {
            eprintln!("Parse error: {e:?}");
            return;
        }
    };

    let mut variables = HashMap::new();

    for statement in parsed_statements {
        match statement {
            Statement::Expression(expr) => {
                println!("eval: {:?}", eval(expr, &variables))
            }
            Statement::VarDef(name, expr) => {
                let value = eval(expr, &variables);
                variables.insert(name, value);
            }
            Statement::VarAssign(name, expr) => {
                if !variables.contains_key(name) {
                    panic!("Variable is not defined");
                }
                let value = eval(expr, &variables);
                variables.insert(name, value);
            }
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
    If(
        Box<Expression<'src>>,
        Box<Expression<'src>>,
        Option<Box<Expression<'src>>>,
    ),
}

#[derive(Debug, PartialEq, Clone)]
enum Statement<'src> {
    Expression(Expression<'src>),
    VarDef(&'src str, Expression<'src>),
    VarAssign(&'src str, Expression<'src>),
}

type Statements<'a> = Vec<Statement<'a>>;

fn unary_fn(
    f: fn(f64) -> f64,
) -> impl Fn(Vec<Expression>, &HashMap<&str, f64>) -> f64 {
    move |args, variables| {
        f(eval(
            args.into_iter().next().expect("function missing argument"),
            variables,
        ))
    }
}

fn binary_fn(
    f: fn(f64, f64) -> f64,
) -> impl Fn(Vec<Expression>, &HashMap<&str, f64>) -> f64 {
    move |args, variables| {
        let mut args = args.into_iter();
        let lhs = eval(
            args.next().expect("function missing the first argument"),
            variables,
        );
        let rhs = eval(
            args.next().expect("function missing the second argument"),
            variables,
        );
        f(lhs, rhs)
    }
}

fn eval(expr: Expression, vars: &HashMap<&str, f64>) -> f64 {
    match expr {
        Expression::Ident("pi") => std::f64::consts::PI,
        Expression::Ident(id) => *vars.get(id).expect("Variable not found"),
        Expression::NumLiteral(n) => n,
        Expression::FnInvoke("sqrt", args) => unary_fn(f64::sqrt)(args, vars),
        Expression::FnInvoke("sin", args) => unary_fn(f64::sin)(args, vars),
        Expression::FnInvoke("cos", args) => unary_fn(f64::cos)(args, vars),
        Expression::FnInvoke("tan", args) => unary_fn(f64::tan)(args, vars),
        Expression::FnInvoke("asin", args) => unary_fn(f64::asin)(args, vars),
        Expression::FnInvoke("acos", args) => unary_fn(f64::acos)(args, vars),
        Expression::FnInvoke("atan", args) => unary_fn(f64::atan)(args, vars),
        Expression::FnInvoke("atan2", args) => {
            binary_fn(f64::atan2)(args, vars)
        }
        Expression::FnInvoke("pow", args) => binary_fn(f64::powf)(args, vars),
        Expression::FnInvoke("exp", args) => unary_fn(f64::exp)(args, vars),
        Expression::FnInvoke("log", args) => binary_fn(f64::log)(args, vars),
        Expression::FnInvoke("log10", args) => {
            unary_fn(f64::log10)(args, vars)
        }
        Expression::FnInvoke(name, _) => panic!("Unknown function {name:?}"),
        Expression::Add(lhs, rhs) => eval(*lhs, vars) + eval(*rhs, vars),
        Expression::Sub(lhs, rhs) => eval(*lhs, vars) - eval(*rhs, vars),
        Expression::Mul(lhs, rhs) => eval(*lhs, vars) * eval(*rhs, vars),
        Expression::Div(lhs, rhs) => eval(*lhs, vars) / eval(*rhs, vars),
        Expression::If(cond, t_case, f_case) => {
            if eval(*cond, vars) != 0. {
                eval(*t_case, vars)
            } else if let Some(f_case) = f_case {
                eval(*f_case, vars)
            } else {
                0.
            }
        }
    }
}

fn factor(i: &str) -> IResult<&str, Expression> {
    alt((number, func_call, ident, parens))(i)
}

fn func_call(i: &str) -> IResult<&str, Expression> {
    let (r, ident) = delimited(multispace0, identifier, multispace0)(i)?;
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
    let (r, res) = delimited(multispace0, identifier, multispace0)(input)?;
    Ok((r, Expression::Ident(res)))
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn number(input: &str) -> IResult<&str, Expression> {
    let (r, v) = delimited(multispace0, recognize_float, multispace0)(input)?;
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
            delimited(multispace0, alt((char('*'), char('/'))), multispace0),
            factor,
        ),
        move || init.clone(),
        |acc, (op, val): (char, Expression)| match op {
            '*' => Expression::Mul(Box::new(acc), Box::new(val)),
            '/' => Expression::Div(Box::new(acc), Box::new(val)),
            _ => panic!(
                "Multiplicative expression should have '*' or '/' operator"
            ),
        },
    )(i)
}

fn num_expr(i: &str) -> IResult<&str, Expression> {
    let (i, init) = term(i)?;

    fold_many0(
        pair(
            delimited(multispace0, alt((char('+'), char('-'))), multispace0),
            term,
        ),
        move || init.clone(),
        |acc, (op, val): (char, Expression)| match op {
            '+' => Expression::Add(Box::new(acc), Box::new(val)),
            '-' => Expression::Sub(Box::new(acc), Box::new(val)),
            _ => {
                panic!("Additive expression should have '+' or '-' operator")
            }
        },
    )(i)
}

fn open_brace(i: &str) -> IResult<&str, ()> {
    let (i, _) = delimited(multispace0, char('{'), multispace0)(i)?;
    Ok((i, ()))
}

fn close_brace(i: &str) -> IResult<&str, ()> {
    let (i, _) = delimited(multispace0, char('}'), multispace0)(i)?;
    Ok((i, ()))
}

fn if_expr(i: &str) -> IResult<&str, Expression> {
    let (i, _) = delimited(multispace0, tag("if"), multispace0)(i)?;
    let (i, cond) = expr(i)?;
    let (i, t_case) = delimited(open_brace, expr, close_brace)(i)?;
    let (i, f_case) = opt(preceded(
        delimited(multispace0, tag("else"), multispace0),
        delimited(open_brace, expr, close_brace),
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
    alt((if_expr, num_expr))(i)
}

fn var_def(i: &str) -> IResult<&str, Statement> {
    let (i, _) = delimited(multispace0, tag("var"), multispace1)(i)?;
    let (i, name) = delimited(multispace0, identifier, multispace0)(i)?;
    let (i, _) = delimited(multispace0, char('='), multispace0)(i)?;
    let (i, expr) = delimited(multispace0, expr, multispace0)(i)?;
    Ok((i, Statement::VarDef(name, expr)))
}

fn var_assign(i: &str) -> IResult<&str, Statement> {
    let (i, name) = delimited(multispace0, identifier, multispace0)(i)?;
    let (i, _) = delimited(multispace0, char('='), multispace0)(i)?;
    let (i, expr) = delimited(multispace0, expr, multispace0)(i)?;
    Ok((i, Statement::VarAssign(name, expr)))
}

fn expr_statement(i: &str) -> IResult<&str, Statement> {
    let (i, res) = expr(i)?;
    Ok((i, Statement::Expression(res)))
}

fn statement(i: &str) -> IResult<&str, Statement> {
    alt((var_def, var_assign, expr_statement))(i)
}

fn statements(i: &str) -> Result<Statements, nom::error::Error<&str>> {
    let (_, res) = separated_list0(tag(";"), statement)(i).finish()?;
    Ok(res)
}
