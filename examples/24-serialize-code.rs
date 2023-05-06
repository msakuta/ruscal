use std::io::{BufReader, BufWriter, Read, Write};

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
  LoadLiteral,
  Add,
}

impl From<u8> for OpCode {
  #[allow(non_upper_case_globals)]
  fn from(o: u8) -> Self {
    const LoadLiteral: u8 = OpCode::LoadLiteral as u8;
    const Add: u8 = OpCode::Add as u8;

    match o {
      LoadLiteral => OpCode::LoadLiteral,
      Add => OpCode::Add,
      _ => panic!("Opcode \"{:02X}\" unrecognized!", o),
    }
  }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
struct Instruction {
  op: OpCode,
  arg0: u8,
}

impl Instruction {
  fn new(op: OpCode, arg0: u8) -> Self {
    Self { op, arg0 }
  }

  fn serialize(
    &self,
    writer: &mut impl Write,
  ) -> Result<(), std::io::Error> {
    writer.write_all(&[self.op as u8, self.arg0])?;
    Ok(())
  }

  fn deserialize(
    reader: &mut impl Read,
  ) -> Result<Self, std::io::Error> {
    let mut buf = [0u8; 2];
    reader.read_exact(&mut buf)?;
    Ok(Self::new(buf[0].into(), buf[1]))
  }
}

fn write_program(file: &str) {
  let instructions = [
    Instruction::new(OpCode::LoadLiteral, 42),
    Instruction::new(OpCode::LoadLiteral, 36),
    Instruction::new(OpCode::Add, 0),
  ];

  if let Ok(writer) = std::fs::File::create(file) {
    let mut writer = BufWriter::new(writer);
    for instruction in &instructions {
      instruction.serialize(&mut writer).unwrap();
    }
    println!("Written {} instructions", instructions.len());
  }
}

fn read_program(file: &str) -> Option<Vec<Instruction>> {
  if let Ok(reader) = std::fs::File::open(file) {
    let mut reader = BufReader::new(reader);
    let mut instructions = vec![];
    while let Ok(inst) = Instruction::deserialize(&mut reader) {
      instructions.push(inst);
    }
    Some(instructions)
  } else {
    None
  }
}

fn main() {
  let mut args = std::env::args();
  args.next();
  match args.next().as_ref().map(|s| s as &str) {
    Some("w") => write_program("bytecode.bin"),
    Some("r") => {
      if let Some(instructions) = read_program("bytecode.bin") {
        let result = interpret(&instructions);
        println!("result: {result:?}");
      }
    }
    _ => println!("Please specify w or r as an argument"),
  }
}

fn interpret(instructions: &[Instruction]) -> Option<i64> {
  let mut stack = vec![];

  for instruction in instructions {
    match instruction.op {
      OpCode::LoadLiteral => {
        stack.push(instruction.arg0 as i64);
      }
      OpCode::Add => {
        let rhs = stack.pop().expect("Stack underflow");
        let lhs = stack.pop().expect("Stack underflow");
        stack.push(lhs + rhs);
      }
    }
  }

  stack.pop()
}
