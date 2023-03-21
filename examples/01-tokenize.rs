fn main() {
  let input = "123 world";
  println!("source: {:?}, parsed: {:?}", input, source(input));

  let input = "Hello world";
  println!("source: {:?}, parsed: {:?}", input, source(input));

  let input = "      world";
  println!("source: {:?}, parsed: {:?}", input, source(input));
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
}

fn token(i: &str) -> (&str, Option<Token>) {
  if let (i, Some(ident_res)) = ident(whitespace(i)) {
    return (i, Some(ident_res));
  }

  if let (i, Some(number_res)) = number(whitespace(i)) {
    return (i, Some(number_res));
  }

  (i, None)
}

fn whitespace(mut input: &str) -> &str {
  while matches!(input.chars().next(), Some(' ')) {
    let mut chars = input.chars();
    chars.next();
    input = chars.as_str();
  }
  input
}

fn ident(mut input: &str) -> (&str, Option<Token>) {
  if matches!(
    input.chars().next(),
    Some(_x @ ('a'..='z' | 'A'..='Z'))
  ) {
    while matches!(
      input.chars().next(),
      Some(_x @ ('a'..='z' | 'A'..='Z' | '0'..='9'))
    ) {
      let mut chars = input.chars();
      chars.next();
      input = chars.as_str();
    }
    (input, Some(Token::Ident))
  } else {
    (input, None)
  }
}

fn number(mut input: &str) -> (&str, Option<Token>) {
  if matches!(
    input.chars().next(),
    Some(_x @ ('-' | '+' | '.' | '0'..='9'))
  ) {
    while matches!(
      input.chars().next(),
      Some(_x @ ('.' | '0'..='9'))
    ) {
      let mut chars = input.chars();
      chars.next();
      input = chars.as_str();
    }
    (input, Some(Token::Number))
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
