use std::{
  collections::HashMap,
  fmt::Display,
  io::{BufReader, BufWriter, Read, Write},
};

use ::rusty_programmer::{dprintln, parse_args, RunMode};
use rusty_programmer::Args;

use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::{
    alpha1, alphanumeric1, char, multispace0, multispace1,
  },
  combinator::{opt, recognize},
  error::ParseError,
  multi::{fold_many0, many0, separated_list0},
  number::complete::recognize_float,
  sequence::{delimited, pair, preceded, terminated},
  Finish, IResult, Parser,
};

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
  LoadLiteral,
  Store,
  Copy,
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
  Add,
  Sub,
  Mul,
  Div,
  Call,
  Jmp,
  Jf,
  Lt,
  Pop,
  Ret
);

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

fn serialize_size(
  sz: usize,
  writer: &mut impl Write,
) -> std::io::Result<()> {
  writer.write_all(&(sz as u32).to_le_bytes())
}

fn deserialize_size(
  reader: &mut impl Read,
) -> std::io::Result<usize> {
  let mut buf = [0u8; std::mem::size_of::<u32>()];
  reader.read_exact(&mut buf)?;
  Ok(u32::from_le_bytes(buf) as usize)
}

fn serialize_str(
  s: &str,
  writer: &mut impl Write,
) -> std::io::Result<()> {
  serialize_size(s.len(), writer)?;
  writer.write_all(s.as_bytes())?;
  Ok(())
}

fn deserialize_str(
  reader: &mut impl Read,
) -> std::io::Result<String> {
  let mut buf = vec![0u8; deserialize_size(reader)?];
  reader.read_exact(&mut buf)?;
  let s = String::from_utf8(buf).unwrap();
  Ok(s)
}

#[repr(u8)]
enum ValueKind {
  F64,
  Str,
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
  F64(f64),
  Str(String),
}

impl Default for Value {
  fn default() -> Self {
    Self::F64(0.)
  }
}

impl Display for Value {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    match self {
      Self::F64(value) => write!(f, "{value}"),
      Self::Str(value) => write!(f, "{value:?}"),
    }
  }
}

impl Value {
  fn kind(&self) -> ValueKind {
    match self {
      Self::F64(_) => ValueKind::F64,
      Self::Str(_) => ValueKind::Str,
    }
  }

  fn serialize(
    &self,
    writer: &mut impl Write,
  ) -> std::io::Result<()> {
    let kind = self.kind() as u8;
    writer.write_all(&[kind])?;
    match self {
      Self::F64(value) => {
        writer.write_all(&value.to_le_bytes())?;
      }
      Self::Str(value) => {
        serialize_str(value, writer)?;
      }
    }
    Ok(())
  }

  #[allow(non_upper_case_globals)]
  fn deserialize(
    reader: &mut impl Read,
  ) -> std::io::Result<Self> {
    const F64: u8 = ValueKind::F64 as u8;
    const Str: u8 = ValueKind::Str as u8;

    let mut kind_buf = [0u8; 1];
    reader.read_exact(&mut kind_buf)?;
    match kind_buf[0] {
      F64 => {
        let mut buf = [0u8; std::mem::size_of::<f64>()];
        reader.read_exact(&mut buf)?;
        Ok(Value::F64(f64::from_le_bytes(buf)))
      }
      Str => Ok(Value::Str(deserialize_str(reader)?)),
      _ => Err(std::io::Error::new(
        std::io::ErrorKind::Other,
        format!(
          "ValueKind {} does not match to any known value",
          kind_buf[0]
        ),
      )),
    }
  }

  fn coerce_f64(&self) -> f64 {
    match self {
      Self::F64(value) => *value,
      _ => panic!(
        "Coercion failed: {:?} cannot be coerced to f64",
        self
      ),
    }
  }
}

#[derive(Debug, Clone, Default)]
enum Target {
  #[default]
  Temp,
  Literal(usize),
  Local(String),
}

struct FnDef {
  args: Vec<String>,
  literals: Vec<Value>,
  instructions: Vec<Instruction>,
}

impl FnDef {
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

  fn serialize(
    &self,
    writer: &mut impl Write,
  ) -> std::io::Result<()> {
    Self::write_args(&self.args, writer)?;
    Self::write_literals(&self.literals, writer)?;
    Self::write_insts(&self.instructions, writer)?;
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
    Ok(Self {
      args,
      literals,
      instructions,
    })
  }

  fn disasm(
    &self,
    writer: &mut impl Write,
  ) -> std::io::Result<()> {
    use OpCode::*;
    writeln!(writer, "  Literals [{}]", self.literals.len())?;
    for (i, con) in self.literals.iter().enumerate() {
      writeln!(writer, "    [{i}] {}", *con)?;
    }

    writeln!(
      writer,
      "  Instructions [{}]",
      self.instructions.len()
    )?;
    for (i, inst) in self.instructions.iter().enumerate() {
      match inst.op {
        LoadLiteral => writeln!(
          writer,
          "    [{i}] {:?} {} ({:?})",
          inst.op, inst.arg0, self.literals[inst.arg0 as usize]
        )?,
        Copy | Call | Jmp | Jf | Lt | Pop | Store => writeln!(
          writer,
          "    [{i}] {:?} {}",
          inst.op, inst.arg0
        )?,
        _ => writeln!(writer, "    [{i}] {:?}", inst.op)?,
      }
    }
    Ok(())
  }
}

struct Compiler {
  literals: Vec<Value>,
  instructions: Vec<Instruction>,
  target_stack: Vec<Target>,
  funcs: HashMap<String, FnDef>,
}

impl Compiler {
  fn new() -> Self {
    Self {
      literals: vec![],
      instructions: vec![],
      target_stack: vec![],
      funcs: HashMap::new(),
    }
  }

  fn add_literal(&mut self, value: Value) -> u8 {
    let existing = self
      .literals
      .iter()
      .enumerate()
      .find(|(_, val)| **val == value);
    if let Some((i, _)) = existing {
      i as u8
    } else {
      let ret = self.literals.len();
      self.literals.push(value);
      ret as u8
    }
  }

  /// Returns absolute position of inserted value
  fn add_inst(&mut self, op: OpCode, arg0: u8) -> usize {
    let inst = self.instructions.len();
    self.instructions.push(Instruction { op, arg0 });
    inst
  }

  fn add_copy_inst(&mut self, stack_idx: usize) -> usize {
    let inst = self.add_inst(
      OpCode::Copy,
      (self.target_stack.len() - stack_idx - 1) as u8,
    );
    self.target_stack.push(Target::Temp);
    inst
  }

  fn add_binop_inst(&mut self, op: OpCode) -> usize {
    self.target_stack.pop();
    self.add_inst(op, 0)
  }

  fn add_store_inst(&mut self, stack_idx: usize) -> usize {
    let inst = self.add_inst(
      OpCode::Store,
      (self.target_stack.len() - stack_idx - 1) as u8,
    );
    self.target_stack.pop();
    inst
  }

  fn add_jf_inst(&mut self, inst_jmp: usize) -> usize {
    let inst = self.add_inst(OpCode::Jf, inst_jmp as u8);
    self.target_stack.pop();
    inst
  }

  /// Pop until given stack index
  fn add_pop_until_inst(
    &mut self,
    stack_idx: usize,
  ) -> Option<usize> {
    if self.target_stack.len() <= stack_idx {
      return None;
    }
    let inst = self.add_inst(
      OpCode::Pop,
      (self.target_stack.len() - stack_idx - 1) as u8,
    );
    self.target_stack.resize(stack_idx + 1, Target::Temp);
    Some(inst)
  }

  fn add_fn(&mut self, name: String, args: &[&str]) {
    self.funcs.insert(
      name,
      FnDef {
        args: args.iter().map(|arg| arg.to_string()).collect(),
        literals: std::mem::take(&mut self.literals),
        instructions: std::mem::take(&mut self.instructions),
      },
    );
  }

  fn write_funcs(
    &self,
    writer: &mut impl Write,
  ) -> std::io::Result<()> {
    serialize_size(self.funcs.len(), writer)?;
    for (name, func) in &self.funcs {
      serialize_str(name, writer)?;
      func.serialize(writer)?;
    }
    Ok(())
  }

  fn compile_expr(&mut self, ex: &Expression) -> usize {
    match ex {
      Expression::NumLiteral(num) => {
        let id = self.add_literal(Value::F64(*num));
        self.add_inst(OpCode::LoadLiteral, id);
        self.target_stack.push(Target::Literal(id as usize));
        self.target_stack.len() - 1
      }
      Expression::Ident(ident) => {
        let var = self.target_stack.iter().enumerate().find(
          |(_i, tgt)| {
            if let Target::Local(id) = tgt {
              id == ident
            } else {
              false
            }
          },
        );
        if let Some(var) = var {
          return var.0;
        } else {
          panic!(
            "Compile error! Variable not found: {ident:?}"
          );
        }
      }
      Expression::Add(lhs, rhs) => {
        self.bin_op(OpCode::Add, lhs, rhs)
      }
      Expression::Sub(lhs, rhs) => {
        self.bin_op(OpCode::Sub, lhs, rhs)
      }
      Expression::Mul(lhs, rhs) => {
        self.bin_op(OpCode::Mul, lhs, rhs)
      }
      Expression::Div(lhs, rhs) => {
        self.bin_op(OpCode::Div, lhs, rhs)
      }
      Expression::Gt(lhs, rhs) => {
        self.bin_op(OpCode::Lt, rhs, lhs)
      }
      Expression::Lt(lhs, rhs) => {
        self.bin_op(OpCode::Lt, lhs, rhs)
      }
      Expression::FnInvoke(name, args) => {
        let name =
          self.add_literal(Value::Str(name.to_string()));
        let args = args
          .iter()
          .map(|arg| self.compile_expr(arg))
          .collect::<Vec<_>>();

        let stack_before_call = self.target_stack.len();
        self.add_inst(OpCode::LoadLiteral, name);
        self.target_stack.push(Target::Literal(name as usize));
        for arg in &args {
          self.add_copy_inst(*arg);
        }

        self.add_inst(OpCode::Call, args.len() as u8);
        self
          .target_stack
          .resize(stack_before_call + 1, Target::Temp);
        self.target_stack.len() - 1
      }
      Expression::If(cond, true_branch, false_branch) => {
        use OpCode::*;
        let cond = self.compile_expr(cond);
        self.add_inst(
          Copy,
          (self.target_stack.len() - cond - 1) as u8,
        );
        let jf_inst = self.add_jf_inst(0);
        let _true_branch = self.compile_expr(true_branch);
        if let Some(false_branch) = false_branch.as_ref() {
          let jmp_inst = self.add_inst(Jmp, 0);
          self.instructions[jf_inst].arg0 =
            self.instructions.len() as u8;
          let fb = self.compile_expr(&false_branch);
          self.add_inst(
            Copy,
            (self.target_stack.len() - fb - 1) as u8,
          );
          self.instructions[jmp_inst].arg0 =
            self.instructions.len() as u8;
        } else {
          self.instructions[jf_inst].arg0 =
            self.instructions.len() as u8;
        }
        self.target_stack.len() - 1
      }
    }
  }

  fn bin_op(
    &mut self,
    op: OpCode,
    lhs: &Expression,
    rhs: &Expression,
  ) -> usize {
    let lhs = self.compile_expr(lhs);
    let rhs = self.compile_expr(rhs);
    self.add_copy_inst(lhs);
    self.add_copy_inst(rhs);
    self.add_inst(op, 0);
    self.target_stack.pop();
    self.target_stack.pop();
    self.target_stack.push(Target::Temp);
    self.target_stack.len() - 1
  }

  fn compile_stmts(
    &mut self,
    stmts: &Statements,
  ) -> Result<(), Box<dyn std::error::Error>> {
    for stmt in stmts {
      match stmt {
        Statement::Expression(ex) => {
          self.compile_expr(ex);
        }
        Statement::VarDef(vname, ex) => {
          let ex = self.compile_expr(ex);
          self.target_stack[ex] =
            Target::Local(vname.to_string());
        }
        Statement::VarAssign(vname, ex) => {
          let stk_ex = self.compile_expr(ex);
          let (stk_local, _) = self
            .target_stack
            .iter_mut()
            .enumerate()
            .find(|(_, tgt)| {
              if let Target::Local(tgt) = tgt {
                tgt == vname
              } else {
                false
              }
            })
            .ok_or_else(|| {
              "Variable name not found".to_string()
            })?;
          self.add_copy_inst(stk_ex);
          self.add_store_inst(stk_local);
        }
        Statement::For {
          loop_var,
          start,
          end,
          stmts,
        } => {
          let stk_start = self.compile_expr(start);
          let stk_end = self.compile_expr(end);
          dprintln!("start: {stk_start} end: {stk_end}");
          self.add_copy_inst(stk_start);
          let stk_loop_var = self.target_stack.len() - 1;
          self.target_stack[stk_loop_var] =
            Target::Local(loop_var.to_string());
          dprintln!("after start: {:?}", self.target_stack);
          let stk_check_exit = self.target_stack.len();
          self.add_copy_inst(stk_loop_var);
          self.add_copy_inst(stk_end);
          dprintln!("before cmp: {:?}", self.target_stack);
          self.add_binop_inst(OpCode::Lt);
          let jf_inst = self.add_jf_inst(0);
          dprintln!("start in loop: {:?}", self.target_stack);
          self.compile_stmts(stmts)?;
          let one = self.add_literal(Value::F64(1.));
          dprintln!("end in loop: {:?}", self.target_stack);
          self.add_copy_inst(stk_loop_var);
          self.add_inst(OpCode::LoadLiteral, one);
          self.target_stack.push(Target::Literal(one as usize));
          self.add_inst(OpCode::Add, 0);
          self.target_stack.pop();
          self.add_store_inst(stk_loop_var);
          self.add_pop_until_inst(stk_loop_var);
          self.add_inst(OpCode::Jmp, stk_check_exit as u8);
          self.instructions[jf_inst].arg0 =
            self.instructions.len() as u8;
        }
        Statement::FnDef { name, args, stmts } => {
          let literals = std::mem::take(&mut self.literals);
          let instructions =
            std::mem::take(&mut self.instructions);
          let target_stack =
            std::mem::take(&mut self.target_stack);
          self.target_stack = args
            .iter()
            .map(|arg| Target::Local(arg.to_string()))
            .collect();
          self.compile_stmts(stmts)?;
          self.add_fn(name.to_string(), args);
          self.literals = literals;
          self.instructions = instructions;
          self.target_stack = target_stack;
        }
      }
    }
    Ok(())
  }

  fn compile(
    &mut self,
    stmts: &Statements,
  ) -> Result<(), Box<dyn std::error::Error>> {
    let name = "main";
    self.compile_stmts(stmts)?;
    self.add_fn(name.to_string(), &[]);
    Ok(())
  }

  fn disasm(
    &self,
    writer: &mut impl Write,
  ) -> std::io::Result<()> {
    for (name, fn_def) in &self.funcs {
      writeln!(writer, "Function {name:?}:")?;
      fn_def.disasm(writer)?;
    }
    Ok(())
  }
}

fn write_program(
  source: &str,
  writer: &mut impl Write,
  out_file: &str,
  disasm: bool,
) -> Result<(), Box<dyn std::error::Error>> {
  let mut compiler = Compiler::new();
  let stmts = statements_finish(source).map_err(|e| {
    std::io::Error::new(
      std::io::ErrorKind::Other,
      e.to_string(),
    )
  })?;

  dprintln!("AST: {stmts:?}");

  compiler.compile(&stmts)?;

  if disasm {
    compiler.disasm(&mut std::io::stdout())?;
  }

  compiler.write_funcs(writer)?;
  dprintln!(
    "Written {} literals and {} instructions to {out_file:?}",
    compiler.literals.len(),
    compiler.instructions.len()
  );
  Ok(())
}

fn print_fn(args: &[Value]) -> Value {
  for arg in args {
    print!("{:?} ", arg);
  }
  println!("");
  Value::F64(0.)
}

struct ByteCode {
  funcs: HashMap<String, FnDef>,
}

impl ByteCode {
  fn new() -> Self {
    Self {
      funcs: HashMap::new(),
    }
  }

  fn read_funcs(
    &mut self,
    reader: &mut impl Read,
  ) -> std::io::Result<()> {
    let num_funcs = deserialize_size(reader)?;
    let mut funcs = HashMap::new();
    for _ in 0..num_funcs {
      let name = deserialize_str(reader)?;
      funcs.insert(name, FnDef::deserialize(reader)?);
    }
    self.funcs = funcs;
    Ok(())
  }

  fn interpret(
    &self,
    fn_name: &str,
    args: &[Value],
  ) -> Result<Value, Box<dyn std::error::Error>> {
    let fn_def = self.funcs.get(fn_name).ok_or_else(|| {
      format!("Function {fn_name:?} was not found")
    })?;
    let mut stack = args.to_vec();
    let mut ip = 0;

    while ip < fn_def.instructions.len() {
      let instruction = &fn_def.instructions[ip];
      dprintln!(
        "interpret[{ip}]: {instruction:?} stack: {stack:?}"
      );
      match instruction.op {
        OpCode::LoadLiteral => {
          stack.push(
            fn_def.literals[instruction.arg0 as usize].clone(),
          );
        }
        OpCode::Store => {
          let idx = stack.len() - instruction.arg0 as usize - 1;
          let value =
            stack.pop().expect("Store needs an argument");
          stack[idx] = value;
        }
        OpCode::Copy => {
          stack.push(
            stack[stack.len() - instruction.arg0 as usize - 1]
              .clone(),
          );
        }
        OpCode::Add => self
          .interpret_bin_op(&mut stack, |lhs, rhs| lhs + rhs),
        OpCode::Sub => self
          .interpret_bin_op(&mut stack, |lhs, rhs| lhs - rhs),
        OpCode::Mul => self
          .interpret_bin_op(&mut stack, |lhs, rhs| lhs * rhs),
        OpCode::Div => self
          .interpret_bin_op(&mut stack, |lhs, rhs| lhs / rhs),
        OpCode::Call => {
          let args =
            &stack[stack.len() - instruction.arg0 as usize..];
          let fname =
            &stack[stack.len() - instruction.arg0 as usize - 1];
          let Value::Str(fname) = fname else {
            panic!("Function name shall be a string: {fname:?}");
          };
          let res = match fname as &str {
            "sqrt" => unary_fn(f64::sqrt)(args),
            "sin" => unary_fn(f64::sin)(args),
            "cos" => unary_fn(f64::cos)(args),
            "tan" => unary_fn(f64::tan)(args),
            "asin" => unary_fn(f64::asin)(args),
            "acos" => unary_fn(f64::acos)(args),
            "atan" => unary_fn(f64::atan)(args),
            "atan2" => binary_fn(f64::atan2)(args),
            "pow" => binary_fn(f64::powf)(args),
            "exp" => unary_fn(f64::exp)(args),
            "log" => binary_fn(f64::log)(args),
            "log10" => unary_fn(f64::log10)(args),
            "print" => print_fn(args),
            _ => self.interpret(fname, args)?,
          };
          stack.resize(
            stack.len() - instruction.arg0 as usize - 1,
            Value::F64(0.),
          );
          stack.push(res);
        }
        OpCode::Jmp => {
          ip = instruction.arg0 as usize;
          continue;
        }
        OpCode::Jf => {
          let cond = stack.pop().expect("Jf needs an argument");
          if cond.coerce_f64() == 0. {
            ip = instruction.arg0 as usize;
            continue;
          }
        }
        OpCode::Lt => self
          .interpret_bin_op(&mut stack, |lhs, rhs| {
            (lhs < rhs) as i32 as f64
          }),
        OpCode::Pop => {
          stack.resize(
            stack.len() - instruction.arg0 as usize,
            Value::default(),
          );
        }
        OpCode::Ret => {
          return Ok(
            stack
              .pop()
              .ok_or_else(|| "Stack underflow".to_string())?,
          );
        }
      }
      ip += 1;
    }

    Ok(
      stack
        .pop()
        .ok_or_else(|| "Stack underflow".to_string())?,
    )
  }

  fn interpret_bin_op(
    &self,
    stack: &mut Vec<Value>,
    op: impl FnOnce(f64, f64) -> f64,
  ) {
    let rhs =
      stack.pop().expect("Stack underflow").coerce_f64();
    let lhs =
      stack.pop().expect("Stack underflow").coerce_f64();
    stack.push(Value::F64(op(lhs, rhs)));
  }
}

fn unary_fn(f: fn(f64) -> f64) -> impl Fn(&[Value]) -> Value {
  move |args| {
    let arg = args.first().expect("function missing argument");
    let ret = f(arg.coerce_f64());
    Value::F64(ret)
  }
}

fn binary_fn(
  f: fn(f64, f64) -> f64,
) -> impl Fn(&[Value]) -> Value {
  move |args| {
    let mut args = args.into_iter();
    let lhs = args
      .next()
      .expect("function missing the first argument")
      .coerce_f64();
    let rhs = args
      .next()
      .expect("function missing the second argument")
      .coerce_f64();
    Value::F64(f(lhs, rhs))
  }
}

fn compile(
  writer: &mut impl Write,
  args: &Args,
  out_file: &str,
) -> Result<(), Box<dyn std::error::Error>> {
  let src = args.source.as_ref().ok_or_else(|| {
    Box::new(std::io::Error::new(
      std::io::ErrorKind::Other,
      "Please specify source file to compile after -c"
        .to_string(),
    ))
  })?;
  let source = std::fs::read_to_string(src)?;
  write_program(&source, writer, out_file, args.disasm)
}

fn read_program(
  reader: &mut impl Read,
) -> std::io::Result<ByteCode> {
  let mut bytecode = ByteCode::new();
  bytecode.read_funcs(reader)?;
  Ok(bytecode)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let Some(args) = parse_args() else { return Ok(()) };

  match args.run_mode {
    RunMode::Compile => {
      let writer = std::fs::File::create(&args.output)?;
      let mut writer = BufWriter::new(writer);
      compile(&mut writer, &args, &args.output)?;
    }
    RunMode::Run(code_file) => {
      let reader = std::fs::File::open(&code_file)?;
      let mut reader = BufReader::new(reader);
      let bytecode = read_program(&mut reader)?;
      if let Err(e) = bytecode.interpret("main", &[]) {
        eprintln!("Runtime error: {e:?}");
      }
    }
    RunMode::CompileAndRun => {
      let mut buf = vec![];
      compile(
        &mut std::io::Cursor::new(&mut buf),
        &args,
        "<Memory>",
      )?;
      let bytecode =
        read_program(&mut std::io::Cursor::new(&mut buf))?;
      if let Err(e) = bytecode.interpret("main", &[]) {
        eprintln!("Runtime error: {e:?}");
      }
    }
    _ => println!("Please specify -c, -r or -R as an argument"),
  }
  Ok(())
}

#[derive(Debug, PartialEq, Clone)]
enum Expression<'src> {
  Ident(&'src str),
  NumLiteral(f64),
  FnInvoke(&'src str, Vec<Expression<'src>>),
  Add(Box<Expression<'src>>, Box<Expression<'src>>),
  Sub(Box<Expression<'src>>, Box<Expression<'src>>),
  Mul(Box<Expression<'src>>, Box<Expression<'src>>),
  Div(Box<Expression<'src>>, Box<Expression<'src>>),
  Gt(Box<Expression<'src>>, Box<Expression<'src>>),
  Lt(Box<Expression<'src>>, Box<Expression<'src>>),
  If(
    Box<Expression<'src>>,
    Box<Expression<'src>>,
    Option<Box<Expression<'src>>>,
  ),
}

#[derive(Debug, PartialEq, Clone)]
enum Statement<'src> {
  Expression(Expression<'src>),
  VarDef(&'src str, Expression<'src>),
  VarAssign(&'src str, Expression<'src>),
  For {
    loop_var: &'src str,
    start: Expression<'src>,
    end: Expression<'src>,
    stmts: Statements<'src>,
  },
  FnDef {
    name: &'src str,
    args: Vec<&'src str>,
    stmts: Statements<'src>,
  },
}

type Statements<'a> = Vec<Statement<'a>>;

fn space_delimited<'src, O, E>(
  f: impl Parser<&'src str, O, E>,
) -> impl FnMut(&'src str) -> IResult<&'src str, O, E>
where
  E: ParseError<&'src str>,
{
  delimited(multispace0, f, multispace0)
}

fn factor(i: &str) -> IResult<&str, Expression> {
  alt((number, func_call, ident, parens))(i)
}

fn func_call(i: &str) -> IResult<&str, Expression> {
  let (r, ident) = space_delimited(identifier)(i)?;
  let (r, args) = space_delimited(delimited(
    tag("("),
    many0(delimited(
      multispace0,
      expr,
      space_delimited(opt(tag(","))),
    )),
    tag(")"),
  ))(r)?;
  Ok((r, Expression::FnInvoke(ident, args)))
}

fn ident(input: &str) -> IResult<&str, Expression> {
  let (r, res) = space_delimited(identifier)(input)?;
  Ok((r, Expression::Ident(res)))
}

fn identifier(input: &str) -> IResult<&str, &str> {
  recognize(pair(
    alt((alpha1, tag("_"))),
    many0(alt((alphanumeric1, tag("_")))),
  ))(input)
}

fn number(input: &str) -> IResult<&str, Expression> {
  let (r, v) = space_delimited(recognize_float)(input)?;
  Ok((
    r,
    Expression::NumLiteral(v.parse().map_err(|_| {
      nom::Err::Error(nom::error::Error {
        input,
        code: nom::error::ErrorKind::Digit,
      })
    })?),
  ))
}

fn parens(i: &str) -> IResult<&str, Expression> {
  space_delimited(delimited(tag("("), expr, tag(")")))(i)
}

fn term(i: &str) -> IResult<&str, Expression> {
  let (i, init) = factor(i)?;

  fold_many0(
    pair(space_delimited(alt((char('*'), char('/')))), factor),
    move || init.clone(),
    |acc, (op, val): (char, Expression)| {
      match op {
      '*' => Expression::Mul(Box::new(acc), Box::new(val)),
      '/' => Expression::Div(Box::new(acc), Box::new(val)),
        _ => panic!(
            "Multiplicative expression should have '*' or '/' operator"
        ),
      }
    },
  )(i)
}

fn num_expr(i: &str) -> IResult<&str, Expression> {
  let (i, init) = term(i)?;

  fold_many0(
    pair(space_delimited(alt((char('+'), char('-')))), term),
    move || init.clone(),
    |acc, (op, val): (char, Expression)| match op {
      '+' => Expression::Add(Box::new(acc), Box::new(val)),
      '-' => Expression::Sub(Box::new(acc), Box::new(val)),
      _ => {
        panic!(
          "Additive expression should have '+' or '-' operator"
        )
      }
    },
  )(i)
}

fn cond_expr(i: &str) -> IResult<&str, Expression> {
  let (i, first) = num_expr(i)?;
  let (i, cond) =
    space_delimited(alt((char('<'), char('>'))))(i)?;
  let (i, second) = num_expr(i)?;
  Ok((
    i,
    match cond {
      '<' => Expression::Lt(Box::new(first), Box::new(second)),
      '>' => Expression::Gt(Box::new(first), Box::new(second)),
      _ => unreachable!(),
    },
  ))
}

fn open_brace(i: &str) -> IResult<&str, ()> {
  let (i, _) = space_delimited(char('{'))(i)?;
  Ok((i, ()))
}

fn close_brace(i: &str) -> IResult<&str, ()> {
  let (i, _) = space_delimited(char('}'))(i)?;
  Ok((i, ()))
}

fn if_expr(i: &str) -> IResult<&str, Expression> {
  let (i, _) = space_delimited(tag("if"))(i)?;
  let (i, cond) = expr(i)?;
  let (i, t_case) =
    delimited(open_brace, expr, close_brace)(i)?;
  let (i, f_case) = opt(preceded(
    space_delimited(tag("else")),
    delimited(open_brace, expr, close_brace),
  ))(i)?;

  Ok((
    i,
    Expression::If(
      Box::new(cond),
      Box::new(t_case),
      f_case.map(Box::new),
    ),
  ))
}

fn expr(i: &str) -> IResult<&str, Expression> {
  alt((if_expr, cond_expr, num_expr))(i)
}

fn var_def(i: &str) -> IResult<&str, Statement> {
  let (i, _) =
    delimited(multispace0, tag("var"), multispace1)(i)?;
  let (i, name) = space_delimited(identifier)(i)?;
  let (i, _) = space_delimited(char('='))(i)?;
  let (i, expr) = space_delimited(expr)(i)?;
  Ok((i, Statement::VarDef(name, expr)))
}

fn var_assign(i: &str) -> IResult<&str, Statement> {
  let (i, name) = space_delimited(identifier)(i)?;
  let (i, _) = space_delimited(char('='))(i)?;
  let (i, expr) = space_delimited(expr)(i)?;
  Ok((i, Statement::VarAssign(name, expr)))
}

fn expr_statement(i: &str) -> IResult<&str, Statement> {
  let (i, res) = expr(i)?;
  Ok((i, Statement::Expression(res)))
}

fn for_statement(i: &str) -> IResult<&str, Statement> {
  let (i, _) = space_delimited(tag("for"))(i)?;
  let (i, loop_var) = space_delimited(identifier)(i)?;
  let (i, _) = space_delimited(tag("in"))(i)?;
  let (i, start) = space_delimited(expr)(i)?;
  let (i, _) = space_delimited(tag("to"))(i)?;
  let (i, end) = space_delimited(expr)(i)?;
  let (i, stmts) =
    delimited(open_brace, statements, close_brace)(i)?;
  Ok((
    i,
    Statement::For {
      loop_var,
      start,
      end,
      stmts,
    },
  ))
}

fn fn_def_statement(i: &str) -> IResult<&str, Statement> {
  let (i, _) = space_delimited(tag("fn"))(i)?;
  let (i, name) = space_delimited(identifier)(i)?;
  let (i, _) = space_delimited(tag("("))(i)?;
  let (i, args) =
    separated_list0(char(','), space_delimited(identifier))(i)?;
  let (i, _) = space_delimited(tag(")"))(i)?;
  let (i, stmts) =
    delimited(open_brace, statements, close_brace)(i)?;
  Ok((i, Statement::FnDef { name, args, stmts }))
}

fn statement(i: &str) -> IResult<&str, Statement> {
  alt((
    for_statement,
    fn_def_statement,
    terminated(
      alt((var_def, var_assign, expr_statement)),
      char(';'),
    ),
  ))(i)
}

fn statements(i: &str) -> IResult<&str, Statements> {
  let (i, stmts) = many0(statement)(i)?;
  let (i, _) = opt(char(';'))(i)?;
  Ok((i, stmts))
}

fn statements_finish(
  i: &str,
) -> Result<Statements, nom::error::Error<&str>> {
  let (_, res) = statements(i).finish()?;
  Ok(res)
}