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

struct Compiler {
  consts: Vec<i64>,
  instructions: Vec<Instruction>,
}

impl Compiler {
  fn new() -> Self {
    Self {
      consts: vec![],
      instructions: vec![],
    }
  }

  fn add_const(&mut self, value: i64) -> u8 {
    let ret = self.consts.len();
    self.consts.push(value);
    ret as u8
  }

  fn add_inst(&mut self, op: OpCode, arg0: u8) {
    self.instructions.push(Instruction { op, arg0 });
  }

  fn write_const_table(
    &self,
    writer: &mut impl Write,
  ) -> std::io::Result<()> {
    writer.write_all(&self.consts.len().to_le_bytes())?;
    for value in &self.consts {
      writer.write_all(&value.to_le_bytes())?;
    }
    Ok(())
  }

  fn write_insts(
    &self,
    writer: &mut impl Write,
  ) -> std::io::Result<()> {
    writer.write_all(&self.instructions.len().to_le_bytes())?;
    for instruction in &self.instructions {
      instruction.serialize(writer).unwrap();
    }
    Ok(())
  }
}

fn write_program(file: &str) -> std::io::Result<()> {
  let mut compiler = Compiler::new();
  let arg = compiler.add_const(512);
  compiler.add_inst(OpCode::LoadLiteral, arg);
  let arg = compiler.add_const(1024);
  compiler.add_inst(OpCode::LoadLiteral, arg);
  compiler.add_inst(OpCode::Add, 0);

  let writer = std::fs::File::create(file)?;
  let mut writer = BufWriter::new(writer);
  compiler.write_const_table(&mut writer).unwrap();
  compiler.write_insts(&mut writer).unwrap();
  println!(
    "Written {} instructions",
    compiler.instructions.len()
  );
  Ok(())
}

struct ByteCode {
  consts: Vec<i64>,
  instructions: Vec<Instruction>,
}

impl ByteCode {
  fn new() -> Self {
    Self {
      consts: vec![],
      instructions: vec![],
    }
  }

  fn read_const_table(
    &mut self,
    reader: &mut impl Read,
  ) -> std::io::Result<()> {
    let mut buf = [0; std::mem::size_of::<usize>()];
    reader.read_exact(&mut buf)?;
    let num_consts = usize::from_le_bytes(buf);
    for _ in 0..num_consts {
      let mut buf = [0u8; std::mem::size_of::<i64>()];
      reader.read_exact(&mut buf)?;
      self.consts.push(i64::from_le_bytes(buf));
    }
    Ok(())
  }

  fn read_instructions(
    &mut self,
    reader: &mut impl Read,
  ) -> std::io::Result<()> {
    let mut buf = [0; std::mem::size_of::<usize>()];
    reader.read_exact(&mut buf)?;
    let num_instructions = usize::from_le_bytes(buf);
    for _ in 0..num_instructions {
      let inst = Instruction::deserialize(reader)?;
      self.instructions.push(inst);
    }
    Ok(())
  }

  fn interpret(&self) -> Option<i64> {
    let mut stack = vec![];

    for instruction in &self.instructions {
      match instruction.op {
        OpCode::LoadLiteral => {
          stack.push(self.consts[instruction.arg0 as usize]);
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
}

fn read_program(file: &str) -> std::io::Result<ByteCode> {
  let reader = std::fs::File::open(file)?;
  let mut reader = BufReader::new(reader);
  let mut bytecode = ByteCode::new();
  bytecode.read_const_table(&mut reader)?;
  bytecode.read_instructions(&mut reader)?;
  Ok(bytecode)
}

fn main() {
  let mut args = std::env::args();
  args.next();
  match args.next().as_ref().map(|s| s as &str) {
    Some("w") => write_program("bytecode.bin").unwrap(),
    Some("r") => {
      if let Ok(bytecode) = read_program("bytecode.bin") {
        let result = bytecode.interpret();
        println!("result: {result:?}");
      }
    }
    _ => println!("Please specify w or r as an argument"),
  }
}
