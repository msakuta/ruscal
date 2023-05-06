enum Instruction {
  LoadLiteral(i64),
  Add,
}

fn main() {
  let instructions = [
    Instruction::LoadLiteral(42),
    Instruction::LoadLiteral(36),
    Instruction::Add,
  ];

  let result = interpret(&instructions);

  println!("result: {result:?}");
}

fn interpret(instructions: &[Instruction]) -> Option<i64> {
  let mut stack = vec![];

  for instruction in instructions {
    match instruction {
      Instruction::LoadLiteral(value) => stack.push(*value),
      Instruction::Add => {
        let rhs = stack.pop().expect("Stack underflow");
        let lhs = stack.pop().expect("Stack underflow");
        stack.push(lhs + rhs);
      }
    }
  }

  stack.pop()
}
