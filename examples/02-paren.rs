fn main() {
  let s = "(123  456  world)";
  println!("source: {:?}, parsed:\n {:?}", s, source(s));

  let s = "((car cdr) cdr)";
  println!("source: {:?}, parsed:\n {:?}", s, source(s));

  let s = "()())))((()))";
  println!("source: {:?}, parsed:\n {:?}", s, source(s));
}

fn advance_char(input: &str) -> &str {
  let mut chars = input.chars();
  chars.next();
  chars.as_str()
}

fn peek_char(input: &str) -> Option<char> {
  input.chars().next()
}

fn source(mut input: &str) -> Vec<Token> {
  let mut tokens = vec![];
  while !input.is_empty() {
    input = if let (next_input, Some(token)) = token(input) {
      tokens.push(token);
      next_input
    } else {
      break;
    }
  }
  tokens
}

#[derive(Debug, PartialEq, Eq)]
enum Token {
  Ident,
  Number,
  LParen,
  RParen,
}

fn token(i: &str) -> (&str, Option<Token>) {
  if let (i, Some(ident_res)) = ident(whitespace(i)) {
    return (i, Some(ident_res));
  }

  if let (i, Some(number_res)) = number(whitespace(i)) {
    return (i, Some(number_res));
  }

  if let (i, Some(lparen_res)) = lparen(whitespace(i)) {
    return (i, Some(lparen_res));
  }

  if let (i, Some(rparen_res)) = rparen(whitespace(i)) {
    return (i, Some(rparen_res));
  }

  (whitespace(i), None)
}

fn whitespace(mut input: &str) -> &str {
  while matches!(peek_char(input), Some(' ')) {
    input = advance_char(input);
  }
  input
}

fn ident(mut input: &str) -> (&str, Option<Token>) {
  if matches!(
    peek_char(input),
    Some(_x @ ('a'..='z' | 'A'..='Z'))
  ) {
    input = advance_char(input);
    while matches!(
      peek_char(input),
      Some(_x @ ('a'..='z' | 'A'..='Z' | '0'..='9'))
    ) {
      input = advance_char(input);
    }
    (input, Some(Token::Ident))
  } else {
    (input, None)
  }
}

fn number(mut input: &str) -> (&str, Option<Token>) {
  if matches!(
    peek_char(input),
    Some(_x @ ('-' | '+' | '.' | '0'..='9'))
  ) {
    input = advance_char(input);
    while matches!(
      peek_char(input),
      Some(_x @ ('.' | '0'..='9'))
    ) {
      input = advance_char(input);
    }
    (input, Some(Token::Number))
  } else {
    (input, None)
  }
}

fn lparen(mut input: &str) -> (&str, Option<Token>) {
  if matches!(peek_char(input), Some('(')) {
    input = advance_char(input);
    (input, Some(Token::LParen))
  } else {
    (input, None)
  }
}

fn rparen(mut input: &str) -> (&str, Option<Token>) {
  if matches!(peek_char(input), Some(')')) {
    input = advance_char(input);
    (input, Some(Token::RParen))
  } else {
    (input, None)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_whitespace() {
    assert_eq!(whitespace("    "), "");
  }

  #[test]
  fn test_ident() {
    assert_eq!(ident("Adam"), ("", Some(Token::Ident)));
  }

  #[test]
  fn test_number() {
    assert_eq!(number("123.45 "), (" ", Some(Token::Number)));
  }
}
