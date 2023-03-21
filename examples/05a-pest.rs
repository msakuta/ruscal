#[macro_use]
extern crate pest_derive;

use ::pest::{
  iterators::{Pair, Pairs},
  Parser,
};

#[derive(Parser)]
#[grammar = "../examples/expr.pest"]
struct ExprParser;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
  NumLiteral(f64),
  Ident(String),
  FnInvoke(String, Vec<Expression>),
  Add(Box<Expression>, Box<Expression>),
  Sub(Box<Expression>, Box<Expression>),
  Mult(Box<Expression>, Box<Expression>),
  Div(Box<Expression>, Box<Expression>),
}

fn parse_value(value: Pair<Rule>) -> Expression {
  match value.as_rule() {
    Rule::number => {
      Expression::NumLiteral(value.as_str().parse().unwrap())
    }
    Rule::ident => Expression::Ident(value.as_str().to_owned()),
    _ => unreachable!(),
  }
}

fn parse_product(terms: Pairs<Rule>) -> Expression {
  let mut product = None;
  let mut last_op = None;
  for value in terms {
    match value.as_rule() {
      Rule::sum => {
        product = Some(parse_sum(value));
      }
      Rule::value => {
        let value =
          parse_value(value.into_inner().next().unwrap());
        if let Some((last_product, last_op)) =
          product.zip(last_op)
        {
          if let Rule::times = last_op {
            product = Some(Expression::Mult(
              Box::new(last_product),
              Box::new(value),
            ));
          } else {
            product = Some(Expression::Div(
              Box::new(last_product),
              Box::new(value),
            ));
          }
        } else {
          product = Some(value);
        }
      }
      Rule::times | Rule::divides => {
        last_op = Some(value.as_rule())
      }
      _ => (),
    }
  }
  product.unwrap()
}

fn parse_sum(sum: Pair<Rule>) -> Expression {
  let mut ret = None;
  let mut last_op = Rule::add;
  for sub_expr in sum.into_inner() {
    match sub_expr.as_rule() {
      Rule::product => {
        let value = parse_product(sub_expr.into_inner());
        if let Some(last_sum) = ret {
          if let Rule::add = last_op {
            ret = Some(Expression::Add(
              Box::new(last_sum),
              Box::new(value),
            ));
          } else {
            ret = Some(Expression::Sub(
              Box::new(last_sum),
              Box::new(value),
            ));
          }
        } else {
          ret = Some(value);
        }
      }
      Rule::add | Rule::subtract => {
        last_op = sub_expr.as_rule()
      }
      _ => unreachable!(),
    }
  }
  ret.unwrap()
}

fn parse_string(input: &str) -> Option<Expression> {
  let parsed = ExprParser::parse(Rule::file, &input)
    .expect("unsuccessful parse") // unwrap the parse result
    .next()
    .unwrap(); // get and unwrap the `file` rule; never fails

  let inner = parsed.into_inner().next().unwrap();
  assert!(matches!(inner.as_rule(), Rule::sum));
  Some(parse_sum(inner))
}

fn main() {
  let parsed = ExprParser::parse(Rule::AB, &"abc")
    .expect("unsuccessful parse") // unwrap the parse result
    .next()
    .unwrap(); // get and unwrap the `file` rule; never fails

  assert!(matches!(parsed.as_rule(), Rule::AB));

  let input = "23 * 34";
  println!(
    "source: {input:?}, parsed: {:#?}",
    parse_string(input)
  );

  let input = "Hello + world";
  println!(
    "source: {input:?}, parsed: {:#?}",
    parse_string(input)
  );

  let input = "(123 + 456 ) + world";
  println!(
    "source: {input:?}, parsed: {:#?}",
    parse_string(input)
  );

  let input = "car + cdr + cdr";
  println!(
    "source: {input:?}, parsed: {:#?}",
    parse_string(input)
  );

  let input = "((1 + 2) + (3 + 4)) + 5 + 6";
  println!(
    "source: {input:?}, parsed: {:#?}",
    parse_string(input)
  );
}
