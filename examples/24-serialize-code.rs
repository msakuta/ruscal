use std::io::{BufReader, BufWriter, Read, Write};

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
  NumLiteral,
  Add,
}

impl From<u8> for OpCode {
  #[allow(non_upper_case_globals)]
  fn from(o: u8) -> Self {
    const NumLiteral: u8 = OpCode::NumLiteral as u8;
    const Add: u8 = OpCode::Add as u8;

    match o {
      NumLiteral => OpCode::NumLiteral,
      Add => OpCode::Add,
      _ => panic!("Opcode \"{:02X}\" unrecognized!", o),
    }
  }
}

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
    writer.write(&[self.op as u8])?;
    writer.write(&self.arg0.to_le_bytes())?;
    Ok(())
  }

  fn deserialize(
    reader: &mut impl Read,
  ) -> Result<Option<Self>, std::io::Error> {
    let mut op = [0u8; 1];
    if reader.read(&mut op)? == 0 {
      return Ok(None);
    }
    let op = op[0].into();
    let mut arg0 = [0u8; 1];
    if reader.read(&mut arg0)? == 0 {
      return Ok(None);
    }
    let arg0 = u8::from_le_bytes(arg0);
    Ok(Some(Self::new(op, arg0)))
  }
}

fn main() {
  let mut args = std::env::args();
  args.next(); // (1)
  match args.next().as_ref().map(|s| s as &str) {
    Some("w") => {
      let instructions = [
        Instruction::new(OpCode::NumLiteral, 42),
        Instruction::new(OpCode::NumLiteral, 36),
        Instruction::new(OpCode::Add, 0),
      ];

      if let Ok(writer) = std::fs::File::create("bytecode.bin")
      {
        let mut writer = BufWriter::new(writer);
        for instruction in &instructions {
          instruction.serialize(&mut writer).unwrap();
        }
        println!("Written {} instructions", instructions.len());
      }
    }
    Some("r") => {
      if let Ok(reader) = std::fs::File::open("bytecode.bin") {
        let mut reader = BufReader::new(reader);
        let mut instructions = vec![];
        while let Some(inst) =
          Instruction::deserialize(&mut reader).unwrap()
        {
          instructions.push(inst);
        }
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
      OpCode::NumLiteral => {
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
