fn main() {
  let source = "123 world";
  println!(
    "source: {}, parsed: {:?}",
    source,
    ident(whitespace(number(source)))
  );
}

fn whitespace(mut input: &str) -> &str {
  while matches!(input.chars().next(), Some(' ')) {
    let mut chars = input.chars();
    chars.next();
    input = chars.as_str();
  }
  input
}

fn ident(mut input: &str) -> &str {
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
  }
  input
}

fn number(mut input: &str) -> &str {
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
  }
  input
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
    assert_eq!(ident("Adam"), "");
  }

  #[test]
  fn test_number() {
    assert_eq!(number("123.45 "), " ");
  }
}
