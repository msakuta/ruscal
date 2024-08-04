use std::{
  any::Any,
  collections::HashMap,
  io::{Read, Write},
  rc::Rc,
};

use crate::{
  ast::{Span, TypeDecl},
  instructions::{Instruction, OpCode},
  value::{
    deserialize_size, deserialize_str, serialize_size,
    serialize_str, Value,
  },
};

pub struct FnByteCode {
  args: Vec<String>,
  pub(crate) literals: Vec<Value>,
  pub(crate) instructions: Vec<Instruction>,
  pub(crate) cofn: bool,
}

impl FnByteCode {
  pub(crate) fn new(
    args: Vec<String>,
    literals: Vec<Value>,
    instructions: Vec<Instruction>,
    cofn: bool,
  ) -> Self {
    Self {
      args,
      literals,
      instructions,
      cofn,
    }
  }

  fn write_args(
    args: &[String],
    writer: &mut impl Write,
  ) -> std::io::Result<()> {
    serialize_size(args.len(), writer)?;
    for arg in args {
      serialize_str(arg, writer)?;
    }
    Ok(())
  }

  fn write_literals(
    literals: &[Value],
    writer: &mut impl Write,
  ) -> std::io::Result<()> {
    serialize_size(literals.len(), writer)?;
    for value in literals {
      value.serialize(writer)?;
    }
    Ok(())
  }

  fn write_insts(
    instructions: &[Instruction],
    writer: &mut impl Write,
  ) -> std::io::Result<()> {
    serialize_size(instructions.len(), writer)?;
    for instruction in instructions {
      instruction.serialize(writer).unwrap();
    }
    Ok(())
  }

  pub(crate) fn serialize(
    &self,
    writer: &mut impl Write,
  ) -> std::io::Result<()> {
    Self::write_args(&self.args, writer)?;
    Self::write_literals(&self.literals, writer)?;
    Self::write_insts(&self.instructions, writer)?;
    writer.write_all(&[self.cofn as u8])?;
    Ok(())
  }

  fn read_args(
    reader: &mut impl Read,
  ) -> std::io::Result<Vec<String>> {
    let num_args = deserialize_size(reader)?;
    let mut args = Vec::with_capacity(num_args);
    for _ in 0..num_args {
      args.push(deserialize_str(reader)?);
    }
    Ok(args)
  }

  fn read_literals(
    reader: &mut impl Read,
  ) -> std::io::Result<Vec<Value>> {
    let num_literals = deserialize_size(reader)?;
    let mut literals = Vec::with_capacity(num_literals);
    for _ in 0..num_literals {
      literals.push(Value::deserialize(reader)?);
    }
    Ok(literals)
  }

  fn read_instructions(
    reader: &mut impl Read,
  ) -> std::io::Result<Vec<Instruction>> {
    let num_instructions = deserialize_size(reader)?;
    let mut instructions = Vec::with_capacity(num_instructions);
    for _ in 0..num_instructions {
      let inst = Instruction::deserialize(reader)?;
      instructions.push(inst);
    }
    Ok(instructions)
  }

  fn deserialize(
    reader: &mut impl Read,
  ) -> std::io::Result<Self> {
    let args = Self::read_args(reader)?;
    let literals = Self::read_literals(reader)?;
    let instructions = Self::read_instructions(reader)?;
    let mut cofn = [0u8];
    reader.read_exact(&mut cofn)?;
    Ok(Self {
      args,
      literals,
      instructions,
      cofn: cofn[0] != 0,
    })
  }

  pub(crate) fn disasm(
    &self,
    writer: &mut impl Write,
  ) -> std::io::Result<()> {
    disasm_common(&self.literals, &self.instructions, writer)
  }
}

fn disasm_common(
  literals: &[Value],
  instructions: &[Instruction],
  writer: &mut impl Write,
) -> std::io::Result<()> {
  use OpCode::*;
  writeln!(writer, "  Literals [{}]", literals.len())?;
  for (i, con) in literals.iter().enumerate() {
    writeln!(writer, "    [{i}] {}", *con)?;
  }

  writeln!(writer, "  Instructions [{}]", instructions.len())?;
  for (i, inst) in instructions.iter().enumerate() {
    match inst.op {
      LoadLiteral => writeln!(
        writer,
        "    [{i}] {:?} {} ({:?})",
        inst.op, inst.arg0, literals[inst.arg0 as usize]
      )?,
      Copy | Dup | Call | Jmp | Jf | Pop | Store => writeln!(
        writer,
        "    [{i}] {:?} {}",
        inst.op, inst.arg0
      )?,
      _ => writeln!(writer, "    [{i}] {:?}", inst.op)?,
    }
  }
  Ok(())
}
pub(crate) enum FnDecl<'src> {
  User(UserFn<'src>),
  Native(NativeFn<'src>),
}

impl<'src> FnDecl<'src> {
  pub fn args(&self) -> Vec<(&'src str, TypeDecl)> {
    match self {
      Self::User(user) => user
        .args
        .iter()
        .map(|(name, ty)| (*name.fragment(), *ty))
        .collect(),
      Self::Native(code) => code.args.clone(),
    }
  }

  pub fn ret_type(&self) -> TypeDecl {
    match self {
      Self::User(user) => {
        if user.cofn {
          TypeDecl::Coro
        } else {
          user.ret_type
        }
      }
      Self::Native(native) => native.ret_type,
    }
  }
}

pub struct UserFn<'src> {
  args: Vec<(Span<'src>, TypeDecl)>,
  ret_type: TypeDecl,
  cofn: bool,
}

impl<'src> UserFn<'src> {
  pub fn new(
    args: Vec<(Span<'src>, TypeDecl)>,
    ret_type: TypeDecl,
    cofn: bool,
  ) -> Self {
    Self {
      args,
      ret_type,
      cofn,
    }
  }
}

pub(crate) type NativeFnClosure =
  Box<dyn Fn(&dyn Any, &[Value]) -> Value>;

pub struct NativeFn<'src> {
  args: Vec<(&'src str, TypeDecl)>,
  ret_type: TypeDecl,
  pub(crate) code: NativeFnClosure,
}

impl<'src> NativeFn<'src> {
  pub fn new(
    args: Vec<(&'src str, TypeDecl)>,
    ret_type: TypeDecl,
    code: NativeFnClosure,
  ) -> Self {
    Self {
      args,
      ret_type,
      code,
    }
  }
}

pub enum FnDef {
  User(Rc<FnByteCode>),
  Native(NativeFn<'static>),
}

type Functions<'src> = HashMap<String, FnDecl<'src>>;

pub(crate) fn standard_functions<'src>() -> Functions<'src> {
  let mut funcs = Functions::new();
  funcs.insert("sqrt".to_string(), unary_fn(f64::sqrt));
  funcs.insert("sin".to_string(), unary_fn(f64::sin));
  funcs.insert("cos".to_string(), unary_fn(f64::cos));
  funcs.insert("tan".to_string(), unary_fn(f64::tan));
  funcs.insert("asin".to_string(), unary_fn(f64::asin));
  funcs.insert("acos".to_string(), unary_fn(f64::acos));
  funcs.insert("atan".to_string(), unary_fn(f64::atan));
  funcs.insert("atan2".to_string(), binary_fn(f64::atan2));
  funcs.insert("pow".to_string(), binary_fn(f64::powf));
  funcs.insert("exp".to_string(), unary_fn(f64::exp));
  funcs.insert("log".to_string(), binary_fn(f64::log));
  funcs.insert("log10".to_string(), unary_fn(f64::log10));
  funcs.insert(
    "print".to_string(),
    FnDecl::Native(NativeFn {
      args: vec![("arg", TypeDecl::Any)],
      ret_type: TypeDecl::Any,
      code: Box::new(print_fn),
    }),
  );
  funcs.insert(
    "dbg".to_string(),
    FnDecl::Native(NativeFn {
      args: vec![("arg", TypeDecl::Any)],
      ret_type: TypeDecl::Any,
      code: Box::new(dbg_fn),
    }),
  );
  funcs.insert(
    "puts".to_string(),
    FnDecl::Native(NativeFn {
      args: vec![("arg", TypeDecl::Any)],
      ret_type: TypeDecl::Any,
      code: Box::new(puts_fn),
    }),
  );
  funcs.insert(
    "type".to_string(),
    FnDecl::Native(NativeFn {
      args: vec![("arg", TypeDecl::Any)],
      ret_type: TypeDecl::Str,
      code: Box::new(type_fn),
    }),
  );
  funcs.insert(
    "i64".to_string(),
    FnDecl::Native(NativeFn {
      args: vec![("arg", TypeDecl::Any)],
      ret_type: TypeDecl::I64,
      code: Box::new(move |_, args| {
        Value::I64(
          args
            .first()
            .expect("function missing argument")
            .coerce_i64()
            .unwrap_or(0),
        )
      }),
    }),
  );
  funcs.insert(
    "f64".to_string(),
    FnDecl::Native(NativeFn {
      args: vec![("arg", TypeDecl::Any)],
      ret_type: TypeDecl::F64,
      code: Box::new(move |_, args| {
        Value::F64(
          args
            .first()
            .expect("function missing argument")
            .coerce_f64()
            .unwrap_or(0.),
        )
      }),
    }),
  );
  funcs.insert(
    "str".to_string(),
    FnDecl::Native(NativeFn {
      args: vec![("arg", TypeDecl::Any)],
      ret_type: TypeDecl::Str,
      code: Box::new(move |_, args| {
        Value::Str(
          args
            .first()
            .expect("function missing argument")
            .coerce_str()
            .unwrap_or("".to_string()),
        )
      }),
    }),
  );
  funcs
}

fn unary_fn<'a>(f: fn(f64) -> f64) -> FnDecl<'a> {
  FnDecl::Native(NativeFn {
    args: vec![("lhs", TypeDecl::F64), ("rhs", TypeDecl::F64)],
    ret_type: TypeDecl::F64,
    code: Box::new(move |_, args| {
      Value::F64(f(args
        .iter()
        .next()
        .expect("function missing argument")
        .coerce_f64()
        .unwrap()))
    }),
  })
}

fn binary_fn<'a>(f: fn(f64, f64) -> f64) -> FnDecl<'a> {
  FnDecl::Native(NativeFn {
    args: vec![("lhs", TypeDecl::F64), ("rhs", TypeDecl::F64)],
    ret_type: TypeDecl::F64,
    code: Box::new(move |_, args| {
      let mut args = args.iter();
      let lhs = args
        .next()
        .expect("function missing the first argument")
        .coerce_f64()
        .unwrap();
      let rhs = args
        .next()
        .expect("function missing the second argument")
        .coerce_f64()
        .unwrap();
      Value::F64(f(lhs, rhs))
    }),
  })
}

fn print_fn(_: &dyn Any, args: &[Value]) -> Value {
  for arg in args {
    print!("{} ", arg);
  }
  println!();
  Value::F64(0.)
}

fn dbg_fn(_: &dyn Any, values: &[Value]) -> Value {
  println!("dbg: {:?}", values[0]);
  Value::I64(0)
}

fn puts_fn(_: &dyn Any, args: &[Value]) -> Value {
  for arg in args {
    print!("{}", arg);
  }
  Value::F64(0.)
}

fn type_fn(_: &dyn Any, args: &[Value]) -> Value {
  Value::Str(match args.first() {
    Some(value) => match value {
      Value::I64(_) => "I64".to_string(),
      Value::F64(_) => "F64".to_string(),
      Value::Str(_) => "Str".to_string(),
      Value::Coro(_) => "Coro".to_string(),
    },
    _ => "".to_string(),
  })
}

pub struct ByteCode {
  pub(crate) funcs: HashMap<String, FnDef>,
}

impl Default for ByteCode {
  fn default() -> Self {
    Self::new()
  }
}

impl ByteCode {
  pub fn new() -> Self {
    Self {
      funcs: HashMap::new(),
    }
  }

  pub fn add_fn(
    &mut self,
    name: String,
    native_fn: NativeFn<'static>,
  ) {
    self.funcs.insert(name, FnDef::Native(native_fn));
  }

  pub fn read_funcs(
    &mut self,
    reader: &mut impl Read,
  ) -> std::io::Result<()> {
    let num_funcs = deserialize_size(reader)?;
    let mut funcs: HashMap<_, _> = standard_functions()
      .into_iter()
      .filter_map(|(name, f)| {
        if let FnDecl::Native(f) = f {
          Some((name, FnDef::Native(f)))
        } else {
          None
        }
      })
      .collect();
    for _ in 0..num_funcs {
      let name = deserialize_str(reader)?;
      funcs.insert(
        name,
        FnDef::User(Rc::new(FnByteCode::deserialize(reader)?)),
      );
    }
    self.funcs = funcs;
    Ok(())
  }

  pub fn disasm(
    &self,
    writer: &mut impl Write,
  ) -> std::io::Result<()> {
    for (fname, fn_def) in &self.funcs {
      match fn_def {
        FnDef::User(f) => {
          writeln!(writer, "Function {}:", fname)?;
          f.disasm(writer)?;
        }
        FnDef::Native(_) => {
          writeln!(writer, "Function {}: <Native>", fname)?;
        }
      }
    }
    Ok(())
  }
}
