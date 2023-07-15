use std::io::{Read, Write};

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
  LoadLiteral,
  Store,
  Copy,
  /// Duplicate the value on the top of the stack arg0 times
  Dup,
  Add,
  Sub,
  Mul,
  Div,
  Call,
  Jmp,
  /// Jump if false
  Jf,
  /// Pop a value from the stack, compare it with a value at arg0, push true if it's less
  Lt,
  /// Pop n values from the stack where n is given by arg0
  Pop,
  /// Return current function
  Ret,
  /// Suspend current function execution where it can resume later.
  Yield,
  /// Await a coroutine in progress until the next yield
  Await,
}

macro_rules! impl_op_from {
    ($($op:ident),*) => {
      impl From<u8> for OpCode {
        #[allow(non_upper_case_globals)]
        fn from(o: u8) -> Self {
          $(const $op: u8 = OpCode::$op as u8;)*

          match o {
            $($op => Self::$op,)*
            _ => panic!("Opcode \"{:02X}\" unrecognized!", o),
          }
        }
      }
    }
  }

impl_op_from!(
  LoadLiteral,
  Store,
  Copy,
  Dup,
  Add,
  Sub,
  Mul,
  Div,
  Call,
  Jmp,
  Jf,
  Lt,
  Pop,
  Ret,
  Yield,
  Await
);

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Instruction {
  pub(crate) op: OpCode,
  pub(crate) arg0: u8,
}

impl Instruction {
  fn new(op: OpCode, arg0: u8) -> Self {
    Self { op, arg0 }
  }

  pub(crate) fn serialize(
    &self,
    writer: &mut impl Write,
  ) -> Result<(), std::io::Error> {
    writer.write_all(&[self.op as u8, self.arg0])?;
    Ok(())
  }

  pub(crate) fn deserialize(
    reader: &mut impl Read,
  ) -> Result<Self, std::io::Error> {
    let mut buf = [0u8; 2];
    reader.read_exact(&mut buf)?;
    Ok(Self::new(buf[0].into(), buf[1]))
  }
}
