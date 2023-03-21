fn main() {
  let s = "Hello world";
  println!("source: {:?}, parsed:\n {:?}", s, source(s));

  let s = "(123  456 ) world";
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

fn source(mut input: &str) -> (&str, TokenTree) {
  let mut tokens = vec![];
  while !input.is_empty() {
    input = if let Some((next_input, token)) = token(input) {
      match token {
        Token::LParen => {
          let (next_input, tt) = source(next_input);
          tokens.push(tt);
          next_input
        }
        Token::RParen => {
          return (next_input, TokenTree::Tree(tokens))
        }
        _ => {
          tokens.push(TokenTree::Token(token));
          next_input
        }
      }
    } else {
      break;
    }
  }
  (input, TokenTree::Tree(tokens))
}

#[derive(Debug, PartialEq)]
enum Token<'src> {
  Ident(&'src str),
  Number(f64),
  LParen,
  RParen,
}

#[derive(Debug, PartialEq)]
enum TokenTree<'src> {
  Token(Token<'src>),
  Tree(Vec<TokenTree<'src>>),
}

fn token(input: &str) -> Option<(&str, Token)> {
  if let Some(res) = ident(whitespace(input)) {
    return Some(res);
  }

  if let Some(res) = number(whitespace(input)) {
    return Some(res);
  }

  if let Some(res) = lparen(whitespace(input)) {
    return Some(res);
  }

  if let Some(res) = rparen(whitespace(input)) {
    return Some(res);
  }

  None
}

fn whitespace(mut input: &str) -> &str {
  while matches!(peek_char(input), Some(' ')) {
    let mut chars = input.chars();
    chars.next();
    input = chars.as_str();
  }
  input
}

fn ident(mut input: &str) -> Option<(&str, Token)> {
  let start = input;
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
    Some((
      input,
      Token::Ident(&start[..(start.len() - input.len())]),
    ))
  } else {
    None
  }
}

fn number(mut input: &str) -> Option<(&str, Token)> {
  let start = input;
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
    if let Ok(num) =
      start[..(start.len() - input.len())].parse::<f64>()
    {
      Some((input, Token::Number(num)))
    } else {
      None
    }
  } else {
    None
  }
}

fn lparen(mut input: &str) -> Option<(&str, Token)> {
  if matches!(peek_char(input), Some('(')) {
    input = advance_char(input);
    Some((input, Token::LParen))
  } else {
    None
  }
}

fn rparen(mut input: &str) -> Option<(&str, Token)> {
  if matches!(peek_char(input), Some(')')) {
    input = advance_char(input);
    Some((input, Token::RParen))
  } else {
    None
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
    assert_eq!(ident("Adam"), Some(("", Token::Ident("Adam"))));
  }

  #[test]
  fn test_number() {
    assert_eq!(
      number("123.45 "),
      Some((" ", Token::Number(123.45)))
    );
  }
}
