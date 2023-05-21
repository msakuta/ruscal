use std::{
  collections::HashMap,
  error::Error,
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
    none_of,
  },
  combinator::{map_res, opt, recognize},
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
  I64,
  Str,
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
  F64(f64),
  I64(i64),
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
      Self::I64(value) => write!(f, "{value}"),
      Self::Str(value) => write!(f, "{value}"),
    }
  }
}

impl Value {
  fn kind(&self) -> ValueKind {
    match self {
      Self::F64(_) => ValueKind::F64,
      Self::I64(_) => ValueKind::I64,
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
      Self::I64(value) => {
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
    const I64: u8 = ValueKind::I64 as u8;
    const Str: u8 = ValueKind::Str as u8;

    let mut kind_buf = [0u8; 1];
    reader.read_exact(&mut kind_buf)?;
    match kind_buf[0] {
      F64 => {
        let mut buf = [0u8; std::mem::size_of::<f64>()];
        reader.read_exact(&mut buf)?;
        Ok(Value::F64(f64::from_le_bytes(buf)))
      }
      I64 => {
        let mut buf = [0u8; std::mem::size_of::<i64>()];
        reader.read_exact(&mut buf)?;
        Ok(Value::I64(i64::from_le_bytes(buf)))
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
      Self::I64(value) => *value as f64,
      _ => panic!(
        "Coercion failed: {:?} cannot be coerced to f64",
        self
      ),
    }
  }

  fn coerce_i64(&self) -> i64 {
    match self {
      Self::F64(value) => *value as i64,
      Self::I64(value) => *value,
      _ => panic!(
        "Coercion failed: {:?} cannot be coerced to i64",
        self
      ),
    }
  }

  fn coerce_str(&self) -> String {
    match self {
      Self::F64(value) => format!("{value}"),
      Self::I64(value) => format!("{value}"),
      Self::Str(value) => value.clone(),
    }
  }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
/// Absolute Stack Index
struct StkIdx(usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
/// Instruction Pointer
struct InstPtr(usize);

#[derive(Debug, Clone, Default)]
enum Target {
  #[default]
  Temp,
  Literal(usize),
  Local(String),
}

struct LoopFrame {
  start: StkIdx,
  break_ips: Vec<InstPtr>,
}

impl LoopFrame {
  fn new(start: StkIdx) -> Self {
    Self {
      start,
      break_ips: vec![],
    }
  }
}

struct FnByteCode {
  args: Vec<String>,
  literals: Vec<Value>,
  instructions: Vec<Instruction>,
}

impl FnByteCode {
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
      Copy | Call | Jmp | Jf | Pop | Store => writeln!(
        writer,
        "    [{i}] {:?} {}",
        inst.op, inst.arg0
      )?,
      _ => writeln!(writer, "    [{i}] {:?}", inst.op)?,
    }
  }
  Ok(())
}

#[derive(Debug)]
struct LoopStackUnderflowError;

impl Display for LoopStackUnderflowError {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    write!(f, "A break statement outside loop")
  }
}

impl Error for LoopStackUnderflowError {}

struct Compiler {
  literals: Vec<Value>,
  instructions: Vec<Instruction>,
  target_stack: Vec<Target>,
  funcs: HashMap<String, FnByteCode>,
  loop_stack: Vec<LoopFrame>,
}

impl Compiler {
  fn new() -> Self {
    Self {
      literals: vec![],
      instructions: vec![],
      target_stack: vec![],
      funcs: HashMap::new(),
      loop_stack: vec![],
    }
  }

  fn stack_top(&self) -> StkIdx {
    StkIdx(self.target_stack.len() - 1)
  }

  fn fixup_breaks(&mut self) -> Result<(), Box<dyn Error>> {
    let loop_frame =
      self.loop_stack.pop().ok_or(LoopStackUnderflowError)?;
    let break_jmp_addr = self.instructions.len();
    for ip in loop_frame.break_ips {
      self.instructions[ip.0].arg0 = break_jmp_addr as u8;
    }
    Ok(())
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
  fn add_inst(&mut self, op: OpCode, arg0: u8) -> InstPtr {
    let inst = self.instructions.len();
    self.instructions.push(Instruction { op, arg0 });
    InstPtr(inst)
  }

  fn add_copy_inst(&mut self, stack_idx: StkIdx) -> InstPtr {
    let inst = self.add_inst(
      OpCode::Copy,
      (self.target_stack.len() - stack_idx.0 - 1) as u8,
    );
    self.target_stack.push(Target::Temp);
    inst
  }

  fn add_binop_inst(&mut self, op: OpCode) -> InstPtr {
    self.target_stack.pop();
    self.add_inst(op, 0)
  }

  fn add_store_inst(&mut self, stack_idx: StkIdx) -> InstPtr {
    if self.target_stack.len() < stack_idx.0 + 1 {
      eprintln!("Compiled bytecode so far:");
      disasm_common(
        &self.literals,
        &self.instructions,
        &mut std::io::stderr(),
      )
      .unwrap();
      panic!("Target stack undeflow during compilation!");
    }
    let inst = self.add_inst(
      OpCode::Store,
      (self.target_stack.len() - stack_idx.0 - 1) as u8,
    );
    self.target_stack.pop();
    inst
  }

  fn add_jf_inst(&mut self) -> InstPtr {
    // Push with jump address 0, because it will be set later
    let inst = self.add_inst(OpCode::Jf, 0);
    self.target_stack.pop();
    inst
  }

  fn fixup_jmp(&mut self, ip: InstPtr) {
    self.instructions[ip.0].arg0 =
      self.instructions.len() as u8;
  }

  /// Pop until given stack index
  fn add_pop_until_inst(
    &mut self,
    stack_idx: StkIdx,
  ) -> Option<InstPtr> {
    if self.target_stack.len() <= stack_idx.0 {
      return None;
    }
    let inst = self.add_inst(
      OpCode::Pop,
      (self.target_stack.len() - stack_idx.0 - 1) as u8,
    );
    self.target_stack.resize(stack_idx.0 + 1, Target::Temp);
    Some(inst)
  }

  fn add_fn(
    &mut self,
    name: String,
    args: &[(&str, TypeDecl)],
  ) {
    self.funcs.insert(
      name,
      FnByteCode {
        args: args
          .iter()
          .map(|(arg, _)| arg.to_string())
          .collect(),
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

  fn compile_expr(
    &mut self,
    ex: &Expression,
  ) -> Result<StkIdx, Box<dyn Error>> {
    Ok(match ex {
      Expression::NumLiteral(num) => {
        let id = self.add_literal(Value::F64(*num));
        self.add_inst(OpCode::LoadLiteral, id);
        self.target_stack.push(Target::Literal(id as usize));
        self.stack_top()
      }
      Expression::StrLiteral(str) => {
        let id = self.add_literal(Value::Str(str.clone()));
        self.add_inst(OpCode::LoadLiteral, id);
        self.target_stack.push(Target::Literal(id as usize));
        self.stack_top()
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
          return Ok(StkIdx(var.0));
        } else {
          return Err(
            format!("Variable not found: {ident:?}").into(),
          );
        }
      }
      Expression::Add(lhs, rhs) => {
        self.bin_op(OpCode::Add, lhs, rhs)?
      }
      Expression::Sub(lhs, rhs) => {
        self.bin_op(OpCode::Sub, lhs, rhs)?
      }
      Expression::Mul(lhs, rhs) => {
        self.bin_op(OpCode::Mul, lhs, rhs)?
      }
      Expression::Div(lhs, rhs) => {
        self.bin_op(OpCode::Div, lhs, rhs)?
      }
      Expression::Gt(lhs, rhs) => {
        self.bin_op(OpCode::Lt, rhs, lhs)?
      }
      Expression::Lt(lhs, rhs) => {
        self.bin_op(OpCode::Lt, lhs, rhs)?
      }
      Expression::FnInvoke(name, args) => {
        let stack_before_args = self.target_stack.len();
        let name =
          self.add_literal(Value::Str(name.to_string()));
        let args = args
          .iter()
          .map(|arg| self.compile_expr(arg))
          .collect::<Result<Vec<_>, _>>()?;

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
        self.coerce_stack(StkIdx(stack_before_args));
        self.stack_top()
      }
      Expression::If(cond, true_branch, false_branch) => {
        use OpCode::*;
        let cond = self.compile_expr(cond)?;
        self.add_copy_inst(cond);
        let jf_inst = self.add_jf_inst();
        let stack_size_before = self.target_stack.len();
        self.compile_stmts_or_zero(true_branch)?;
        self.coerce_stack(StkIdx(stack_size_before + 1));
        let jmp_inst = self.add_inst(Jmp, 0);
        self.fixup_jmp(jf_inst);
        self
          .target_stack
          .resize(stack_size_before, Target::Temp);
        if let Some(false_branch) = false_branch.as_ref() {
          self.compile_stmts_or_zero(&false_branch)?;
        }
        self.coerce_stack(StkIdx(stack_size_before + 1));
        self.fixup_jmp(jmp_inst);
        self.stack_top()
      }
    })
  }

  fn bin_op(
    &mut self,
    op: OpCode,
    lhs: &Expression,
    rhs: &Expression,
  ) -> Result<StkIdx, Box<dyn Error>> {
    let lhs = self.compile_expr(lhs)?;
    let rhs = self.compile_expr(rhs)?;
    self.add_copy_inst(lhs);
    self.add_copy_inst(rhs);
    self.add_inst(op, 0);
    self.target_stack.pop();
    self.target_stack.pop();
    self.target_stack.push(Target::Temp);
    Ok(self.stack_top())
  }

  /// Coerce the stack size to be target + 1, and move the old top
  /// to the new top.
  fn coerce_stack(&mut self, target: StkIdx) {
    if target.0 < self.target_stack.len() - 1 {
      self.add_store_inst(target);
      self.add_pop_until_inst(target);
    } else if self.target_stack.len() - 1 < target.0 {
      for _ in self.target_stack.len() - 1..target.0 {
        self.add_copy_inst(self.stack_top());
      }
    }
  }

  fn compile_stmts(
    &mut self,
    stmts: &Statements,
  ) -> Result<Option<StkIdx>, Box<dyn Error>> {
    let mut last_result = None;
    for stmt in stmts {
      match stmt {
        Statement::Expression(ex) => {
          last_result = Some(self.compile_expr(ex)?);
        }
        Statement::VarDef(vname, _, ex) => {
          let mut ex = self.compile_expr(ex)?;
          if matches!(self.target_stack[ex.0], Target::Local(_))
          {
            self.add_copy_inst(ex);
            ex = self.stack_top();
          }
          self.target_stack[ex.0] =
            Target::Local(vname.to_string());
        }
        Statement::VarAssign(vname, ex) => {
          let stk_ex = self.compile_expr(ex)?;
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
              format!("Variable name not found: {vname}")
            })?;
          self.add_copy_inst(stk_ex);
          self.add_store_inst(StkIdx(stk_local));
        }
        Statement::For {
          loop_var,
          start,
          end,
          stmts,
        } => {
          let stk_start = self.compile_expr(start)?;
          let stk_end = self.compile_expr(end)?;
          dprintln!("start: {stk_start:?} end: {stk_end:?}");
          self.add_copy_inst(stk_start);
          let stk_loop_var = self.stack_top();
          self.target_stack[stk_loop_var.0] =
            Target::Local(loop_var.to_string());
          dprintln!("after start: {:?}", self.target_stack);
          let inst_check_exit = self.instructions.len();
          self.add_copy_inst(stk_loop_var);
          self.add_copy_inst(stk_end);
          dprintln!("before cmp: {:?}", self.target_stack);
          self.add_binop_inst(OpCode::Lt);
          let jf_inst = self.add_jf_inst();
          dprintln!("start in loop: {:?}", self.target_stack);
          self.loop_stack.push(LoopFrame::new(stk_loop_var));
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
          self.add_inst(OpCode::Jmp, inst_check_exit as u8);
          self.fixup_jmp(jf_inst);
          self.fixup_breaks()?;
        }
        Statement::Break => {
          let start = self
            .loop_stack
            .last()
            .map(|loop_frame| loop_frame.start)
            .ok_or(LoopStackUnderflowError)?;
          self.add_pop_until_inst(start);

          let loop_frame = self
            .loop_stack
            .last_mut()
            .ok_or(LoopStackUnderflowError)?;
          let break_ip = self.instructions.len();
          loop_frame.break_ips.push(InstPtr(break_ip));
          self.add_inst(OpCode::Jmp, 0);
        }
        Statement::FnDef {
          name, args, stmts, ..
        } => {
          let literals = std::mem::take(&mut self.literals);
          let instructions =
            std::mem::take(&mut self.instructions);
          let target_stack =
            std::mem::take(&mut self.target_stack);
          self.target_stack = args
            .iter()
            .map(|arg| Target::Local(arg.0.to_string()))
            .collect();
          self.compile_stmts(stmts)?;
          self.add_fn(name.to_string(), args);
          self.literals = literals;
          self.instructions = instructions;
          self.target_stack = target_stack;
        }
        Statement::Return(ex) => {
          return Ok(Some(self.compile_expr(ex)?));
        }
      }
    }
    Ok(last_result)
  }

  fn compile_stmts_or_zero(
    &mut self,
    stmts: &Statements,
  ) -> Result<StkIdx, Box<dyn Error>> {
    Ok(self.compile_stmts(stmts)?.unwrap_or_else(|| {
      let id = self.add_literal(Value::F64(0.));
      self.add_inst(OpCode::LoadLiteral, id as u8);
      self.stack_top()
    }))
  }

  fn compile(
    &mut self,
    stmts: &Statements,
  ) -> Result<(), Box<dyn std::error::Error>> {
    let name = "main";
    self.compile_stmts_or_zero(stmts)?;
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
  show_ast: bool,
) -> Result<(), Box<dyn std::error::Error>> {
  let mut compiler = Compiler::new();
  let stmts =
    statements_finish(source).map_err(|e| e.to_string())?;

  if show_ast {
    println!("AST: {stmts:#?}");
  }

  match type_check(&stmts, &mut TypeCheckContext::new()) {
    Ok(_) => println!("Typecheck Ok"),
    Err(e) => return Err(e.into()),
  }

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

fn dbg_fn(values: &[Value]) -> Value {
  println!("dbg: {:?}", values[0]);
  Value::I64(0)
}

fn puts_fn(args: &[Value]) -> Value {
  for arg in args {
    print!("{}", arg);
  }
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
        FnDef::User(FnByteCode::deserialize(reader)?),
      );
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
    let fn_def = match fn_def {
      FnDef::User(user) => user,
      FnDef::Native(n) => return Ok((*n.code)(args)),
    };
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
        OpCode::Add => self.interpret_bin_op_str(
          &mut stack,
          |lhs, rhs| lhs + rhs,
          |lhs, rhs| lhs + rhs,
          |lhs, rhs| Some(format!("{lhs}{rhs}")),
        ),
        OpCode::Sub => self.interpret_bin_op(
          &mut stack,
          |lhs, rhs| lhs - rhs,
          |lhs, rhs| lhs - rhs,
        ),
        OpCode::Mul => self.interpret_bin_op(
          &mut stack,
          |lhs, rhs| lhs * rhs,
          |lhs, rhs| lhs * rhs,
        ),
        OpCode::Div => self.interpret_bin_op(
          &mut stack,
          |lhs, rhs| lhs / rhs,
          |lhs, rhs| lhs / rhs,
        ),
        OpCode::Call => {
          let args =
            &stack[stack.len() - instruction.arg0 as usize..];
          let fname =
            &stack[stack.len() - instruction.arg0 as usize - 1];
          let Value::Str(fname) = fname else {
              panic!("Function name shall be a string: {fname:?}");
            };
          let res = self.interpret(fname, args)?;
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
        OpCode::Lt => self.interpret_bin_op(
          &mut stack,
          |lhs, rhs| (lhs < rhs) as i32 as f64,
          |lhs, rhs| (lhs < rhs) as i64,
        ),
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

  fn interpret_bin_op_str(
    &self,
    stack: &mut Vec<Value>,
    op_f64: impl FnOnce(f64, f64) -> f64,
    op_i64: impl FnOnce(i64, i64) -> i64,
    op_str: impl FnOnce(&str, &str) -> Option<String>,
  ) {
    use Value::*;
    let rhs = stack.pop().expect("Stack underflow");
    let lhs = stack.pop().expect("Stack underflow");
    let res = match (lhs, rhs) {
      (F64(lhs), F64(rhs)) => F64(op_f64(lhs, rhs)),
      (I64(lhs), I64(rhs)) => I64(op_i64(lhs, rhs)),
      (F64(lhs), I64(rhs)) => F64(op_f64(lhs, rhs as f64)),
      (I64(lhs), F64(rhs)) => F64(op_f64(lhs as f64, rhs)),
      (Str(lhs), Str(rhs)) => {
        if let Some(res) = op_str(&lhs, &rhs) {
          Str(res)
        } else {
          panic!("Incompatible types in binary op: {lhs:?} and {rhs:?}");
        }
      }
      (lhs, rhs) => panic!(
        "Incompatible types in binary op: {lhs:?} and {rhs:?}"
      ),
    };
    stack.push(res);
  }

  fn interpret_bin_op(
    &self,
    stack: &mut Vec<Value>,
    op_f64: impl FnOnce(f64, f64) -> f64,
    op_i64: impl FnOnce(i64, i64) -> i64,
  ) {
    self
      .interpret_bin_op_str(stack, op_f64, op_i64, |_, _| None)
  }
}

fn unary_fn<'a>(f: fn(f64) -> f64) -> FnDecl<'a> {
  FnDecl::Native(NativeFn {
    args: vec![("lhs", TypeDecl::F64), ("rhs", TypeDecl::F64)],
    ret_type: TypeDecl::F64,
    code: Box::new(move |args| {
      Value::F64(f(args
        .into_iter()
        .next()
        .expect("function missing argument")
        .coerce_f64()))
    }),
  })
}

fn binary_fn<'a>(f: fn(f64, f64) -> f64) -> FnDecl<'a> {
  FnDecl::Native(NativeFn {
    args: vec![("lhs", TypeDecl::F64), ("rhs", TypeDecl::F64)],
    ret_type: TypeDecl::F64,
    code: Box::new(move |args| {
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
    }),
  })
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
  write_program(
    &source,
    writer,
    out_file,
    args.disasm,
    args.show_ast,
  )
}

fn read_program(
  reader: &mut impl Read,
) -> std::io::Result<ByteCode> {
  let mut bytecode = ByteCode::new();
  bytecode.read_funcs(reader)?;
  Ok(bytecode)
}

enum FnDef {
  User(FnByteCode),
  Native(NativeFn<'static>),
}

type Functions<'src> = HashMap<String, FnDecl<'src>>;

fn standard_functions<'src>() -> Functions<'src> {
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
    "i64".to_string(),
    FnDecl::Native(NativeFn {
      args: vec![("arg", TypeDecl::Any)],
      ret_type: TypeDecl::I64,
      code: Box::new(move |args| {
        Value::I64(
          args
            .first()
            .expect("function missing argument")
            .coerce_i64(),
        )
      }),
    }),
  );
  funcs.insert(
    "f64".to_string(),
    FnDecl::Native(NativeFn {
      args: vec![("arg", TypeDecl::Any)],
      ret_type: TypeDecl::F64,
      code: Box::new(move |args| {
        Value::F64(
          args
            .first()
            .expect("function missing argument")
            .coerce_f64(),
        )
      }),
    }),
  );
  funcs.insert(
    "str".to_string(),
    FnDecl::Native(NativeFn {
      args: vec![("arg", TypeDecl::Any)],
      ret_type: TypeDecl::Str,
      code: Box::new(move |args| {
        Value::Str(
          args
            .first()
            .expect("function missing argument")
            .coerce_str(),
        )
      }),
    }),
  );
  funcs
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TypeDecl {
  Any,
  F64,
  I64,
  Str,
}

fn tc_coerce_type<'src>(
  value: &TypeDecl,
  target: &TypeDecl,
) -> Result<TypeDecl, TypeCheckError> {
  use TypeDecl::*;
  Ok(match (value, target) {
    (_, Any) => value.clone(),
    (Any, _) => target.clone(),
    (F64 | I64, F64) => F64,
    (F64, I64) => F64,
    (I64, I64) => I64,
    (Str, Str) => Str,
    _ => {
      return Err(TypeCheckError::new(format!(
        "{:?} cannot be assigned to {:?}",
        value, target
      )))
    }
  })
}

pub struct TypeCheckContext<'src> {
  /// Variables table for type checking.
  vars: HashMap<&'src str, TypeDecl>,
  /// Function names are owned strings because it can be either from source or native.
  funcs: HashMap<String, FnDecl<'src>>,
  super_context: Option<&'src TypeCheckContext<'src>>,
}

impl<'src> TypeCheckContext<'src> {
  pub fn new() -> Self {
    Self {
      vars: HashMap::new(),
      funcs: standard_functions(),
      super_context: None,
    }
  }

  fn get_var(&self, name: &str) -> Option<TypeDecl> {
    if let Some(val) = self.vars.get(name) {
      Some(val.clone())
    } else {
      None
    }
  }

  fn get_fn(&self, name: &str) -> Option<&FnDecl<'src>> {
    if let Some(val) = self.funcs.get(name) {
      Some(val)
    } else if let Some(super_ctx) = self.super_context {
      super_ctx.get_fn(name)
    } else {
      None
    }
  }

  fn push_stack(super_ctx: &'src Self) -> Self {
    Self {
      vars: HashMap::new(),
      funcs: HashMap::new(),
      super_context: Some(super_ctx),
    }
  }
}

#[derive(Debug)]
pub struct TypeCheckError {
  msg: String,
}

impl<'src> std::fmt::Display for TypeCheckError {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    write!(f, "{}", self.msg,)
  }
}

impl Error for TypeCheckError {}

impl TypeCheckError {
  fn new(msg: String) -> Self {
    Self { msg }
  }
}

fn tc_binary_op<'src>(
  lhs: &Expression<'src>,
  rhs: &Expression<'src>,
  ctx: &mut TypeCheckContext<'src>,
  op: &str,
) -> Result<TypeDecl, TypeCheckError> {
  let lhst = tc_expr(lhs, ctx)?;
  let rhst = tc_expr(rhs, ctx)?;
  binary_op_type(&lhst, &rhst).map_err(|_| {
    TypeCheckError::new(format!(
      "Operation {op} between incompatible type: {:?} and {:?}",
      lhst, rhst,
    ))
  })
}

fn binary_op_type(
  lhs: &TypeDecl,
  rhs: &TypeDecl,
) -> Result<TypeDecl, ()> {
  use TypeDecl::*;
  Ok(match (lhs, rhs) {
    (Any, _) => Any,
    (_, Any) => Any,
    (I64, I64) => I64,
    (F64 | I64, F64 | I64) => F64,
    (Str, Str) => Str,
    _ => return Err(()),
  })
}

fn tc_binary_cmp<'src>(
  lhs: &Expression<'src>,
  rhs: &Expression<'src>,
  ctx: &mut TypeCheckContext<'src>,
  op: &str,
) -> Result<TypeDecl, TypeCheckError> {
  use TypeDecl::*;
  let lhst = tc_expr(lhs, ctx)?;
  let rhst = tc_expr(rhs, ctx)?;
  Ok(match (&lhst, &rhst) {
    (Any, _) => I64,
    (_, Any) => I64,
    (F64, F64) => I64,
    (I64, I64) => I64,
    (Str, Str) => I64,
    _ => {
      return Err(TypeCheckError::new(format!(
      "Operation {op} between incompatible type: {:?} and {:?}",
      lhst, rhst,
    )))
    }
  })
}

fn tc_expr<'src>(
  e: &Expression<'src>,
  ctx: &mut TypeCheckContext<'src>,
) -> Result<TypeDecl, TypeCheckError> {
  use Expression::*;
  Ok(match &e {
    NumLiteral(_val) => TypeDecl::F64,
    StrLiteral(_val) => TypeDecl::Str,
    Ident(str) => ctx.get_var(str).ok_or_else(|| {
      TypeCheckError::new(format!(
        "Variable {:?} not found in scope",
        str
      ))
    })?,
    FnInvoke(str, args) => {
      let args_ty = args
        .iter()
        .map(|v| tc_expr(v, ctx))
        .collect::<Result<Vec<_>, _>>()?;
      let func = ctx.get_fn(*str).ok_or_else(|| {
        TypeCheckError::new(format!(
          "function {} is not defined",
          str
        ))
      })?;
      let args_decl = func.args();
      for (arg_ty, decl) in args_ty.iter().zip(args_decl.iter())
      {
        tc_coerce_type(&arg_ty, &decl.1)?;
      }
      func.ret_type()
    }
    Add(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Add")?,
    Sub(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Sub")?,
    Mul(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Mult")?,
    Div(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Div")?,
    Lt(lhs, rhs) => tc_binary_cmp(&lhs, &rhs, ctx, "LT")?,
    Gt(lhs, rhs) => tc_binary_cmp(&lhs, &rhs, ctx, "GT")?,
    If(cond, true_branch, false_branch) => {
      tc_coerce_type(&tc_expr(cond, ctx)?, &TypeDecl::I64)?;
      let true_type = type_check(true_branch, ctx)?;
      if let Some(false_type) = false_branch {
        let false_type = type_check(false_type, ctx)?;
        binary_op_type(&true_type, &false_type).map_err(
          |_| {
            TypeCheckError::new(format!(
              "Conditional expression doesn't have the \
            compatible types in true and false branch: \
            {:?} and {:?}",
              true_type, false_type
            ))
          },
        )?
      } else {
        true_type
      }
    }
  })
}

fn type_check<'src>(
  stmts: &Vec<Statement<'src>>,
  ctx: &mut TypeCheckContext<'src>,
) -> Result<TypeDecl, TypeCheckError> {
  let mut res = TypeDecl::Any;
  for stmt in stmts {
    match stmt {
      Statement::VarDef(var, type_, init_expr) => {
        let init_type = tc_expr(init_expr, ctx)?;
        let init_type = tc_coerce_type(&init_type, type_)?;
        ctx.vars.insert(*var, init_type);
      }
      Statement::VarAssign(var, expr) => {
        let init_type = tc_expr(expr, ctx)?;
        let var =
          ctx.vars.get(*var).expect("Variable not found");
        tc_coerce_type(&init_type, var)?;
      }
      Statement::FnDef {
        name,
        args,
        ret_type,
        stmts,
      } => {
        // Function declaration needs to be added first to allow recursive calls
        ctx.funcs.insert(
          name.to_string(),
          FnDecl::User(UserFn {
            args: args.clone(),
            ret_type: *ret_type,
          }),
        );
        let mut subctx = TypeCheckContext::push_stack(ctx);
        for (arg, ty) in args.iter() {
          subctx.vars.insert(arg, *ty);
        }
        let last_stmt = type_check(stmts, &mut subctx)?;
        tc_coerce_type(&last_stmt, &ret_type)?;
      }
      Statement::Expression(e) => {
        res = tc_expr(&e, ctx)?;
      }
      Statement::For {
        loop_var,
        start,
        end,
        stmts,
      } => {
        tc_coerce_type(&tc_expr(start, ctx)?, &TypeDecl::I64)?;
        tc_coerce_type(&tc_expr(end, ctx)?, &TypeDecl::I64)?;
        ctx.vars.insert(loop_var, TypeDecl::I64);
        res = type_check(stmts, ctx)?;
      }
      Statement::Return(e) => {
        return tc_expr(e, ctx);
      }
      Statement::Break => {
        // TODO: check types in break out site. For now we disallow break with values like Rust.
      } // Statement::Continue => (),
    }
  }
  Ok(res)
}

enum FnDecl<'src> {
  User(UserFn<'src>),
  Native(NativeFn<'src>),
}

impl<'src> FnDecl<'src> {
  fn args(&self) -> Vec<(&'src str, TypeDecl)> {
    match self {
      Self::User(user) => user.args.clone(),
      Self::Native(code) => code.args.clone(),
    }
  }

  fn ret_type(&self) -> TypeDecl {
    match self {
      Self::User(user) => user.ret_type,
      Self::Native(native) => native.ret_type,
    }
  }
}

struct UserFn<'src> {
  args: Vec<(&'src str, TypeDecl)>,
  ret_type: TypeDecl,
}

struct NativeFn<'src> {
  args: Vec<(&'src str, TypeDecl)>,
  ret_type: TypeDecl,
  code: Box<dyn Fn(&[Value]) -> Value>,
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
  StrLiteral(String),
  FnInvoke(&'src str, Vec<Expression<'src>>),
  Add(Box<Expression<'src>>, Box<Expression<'src>>),
  Sub(Box<Expression<'src>>, Box<Expression<'src>>),
  Mul(Box<Expression<'src>>, Box<Expression<'src>>),
  Div(Box<Expression<'src>>, Box<Expression<'src>>),
  Gt(Box<Expression<'src>>, Box<Expression<'src>>),
  Lt(Box<Expression<'src>>, Box<Expression<'src>>),
  If(
    Box<Expression<'src>>,
    Box<Statements<'src>>,
    Option<Box<Statements<'src>>>,
  ),
}

#[derive(Debug, PartialEq, Clone)]
enum Statement<'src> {
  Expression(Expression<'src>),
  VarDef(&'src str, TypeDecl, Expression<'src>),
  VarAssign(&'src str, Expression<'src>),
  For {
    loop_var: &'src str,
    start: Expression<'src>,
    end: Expression<'src>,
    stmts: Statements<'src>,
  },
  Break,
  FnDef {
    name: &'src str,
    args: Vec<(&'src str, TypeDecl)>,
    ret_type: TypeDecl,
    stmts: Statements<'src>,
  },
  Return(Expression<'src>),
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
  alt((str_literal, number, func_call, ident, parens))(i)
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

fn str_literal(i: &str) -> IResult<&str, Expression> {
  let (r0, _) = preceded(multispace0, char('\"'))(i)?;
  let (r, val) = many0(none_of("\""))(r0)?;
  let (r, _) = terminated(char('"'), multispace0)(r)?;
  Ok((
    r,
    Expression::StrLiteral(
      val
        .iter()
        .collect::<String>()
        .replace("\\\\", "\\")
        .replace("\\n", "\n"),
    ),
  ))
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
    delimited(open_brace, statements, close_brace)(i)?;
  let (i, f_case) = opt(preceded(
    space_delimited(tag("else")),
    alt((
      delimited(open_brace, statements, close_brace),
      map_res(
        if_expr,
        |v| -> Result<Vec<Statement>, nom::error::Error<&str>> {
          Ok(vec![Statement::Expression(v)])
        },
      ),
    )),
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
  let (i, _) = space_delimited(char(':'))(i)?;
  let (i, td) = type_decl(i)?;
  let (i, _) = space_delimited(char('='))(i)?;
  let (i, expr) = space_delimited(expr)(i)?;
  Ok((i, Statement::VarDef(name, td, expr)))
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

fn type_decl(i: &str) -> IResult<&str, TypeDecl> {
  let (i, td) = space_delimited(identifier)(i)?;
  Ok((
    i,
    match td {
      "i64" => TypeDecl::I64,
      "f64" => TypeDecl::F64,
      "str" => TypeDecl::Str,
      _ => {
        panic!("Type annotation has unknown type: {td}")
      }
    },
  ))
}

fn argument(i: &str) -> IResult<&str, (&str, TypeDecl)> {
  let (i, ident) = space_delimited(identifier)(i)?;
  let (i, _) = char(':')(i)?;
  let (i, td) = type_decl(i)?;

  Ok((i, (ident, td)))
}

fn fn_def_statement(i: &str) -> IResult<&str, Statement> {
  let (i, _) = space_delimited(tag("fn"))(i)?;
  let (i, name) = space_delimited(identifier)(i)?;
  let (i, _) = space_delimited(tag("("))(i)?;
  let (i, args) =
    separated_list0(char(','), space_delimited(argument))(i)?;
  let (i, _) = space_delimited(tag(")"))(i)?;
  let (i, _) = space_delimited(tag("->"))(i)?;
  let (i, ret_type) = type_decl(i)?;
  let (i, stmts) =
    delimited(open_brace, statements, close_brace)(i)?;
  Ok((
    i,
    Statement::FnDef {
      name,
      args,
      ret_type,
      stmts,
    },
  ))
}

fn return_statement(i: &str) -> IResult<&str, Statement> {
  let (i, _) = space_delimited(tag("return"))(i)?;
  let (i, ex) = space_delimited(expr)(i)?;
  Ok((i, Statement::Return(ex)))
}

fn break_stmt(input: &str) -> IResult<&str, Statement> {
  let (r, _) = space_delimited(tag("break"))(input)?;
  Ok((r, Statement::Break))
}

fn general_statement<'a>(
  last: bool,
) -> impl Fn(&'a str) -> IResult<&'a str, Statement> {
  let terminator = move |i| -> IResult<&str, ()> {
    let mut semicolon = pair(tag(";"), multispace0);
    if last {
      Ok((opt(semicolon)(i)?.0, ()))
    } else {
      Ok((semicolon(i)?.0, ()))
    }
  };
  move |input: &str| {
    alt((
      terminated(var_def, terminator),
      terminated(var_assign, terminator),
      fn_def_statement,
      for_statement,
      terminated(return_statement, terminator),
      terminated(break_stmt, terminator),
      terminated(expr_statement, terminator),
    ))(input)
  }
}

pub(crate) fn last_statement(
  input: &str,
) -> IResult<&str, Statement> {
  general_statement(true)(input)
}

pub(crate) fn statement(
  input: &str,
) -> IResult<&str, Statement> {
  general_statement(false)(input)
}

fn statements(i: &str) -> IResult<&str, Statements> {
  let (r, mut v) = many0(statement)(i)?;
  let (r, last) = opt(last_statement)(r)?;
  let (r, _) = opt(multispace0)(r)?;
  if let Some(last) = last {
    v.push(last);
  }
  Ok((r, v))
}

#[test]
fn t_stmts() {
  let s = "1; 2; 3";
  assert_eq!(
    statements(s),
    Ok((
      "",
      vec![
        Statement::Expression(Expression::NumLiteral(1.)),
        Statement::Expression(Expression::NumLiteral(2.)),
        Statement::Expression(Expression::NumLiteral(3.))
      ]
    ))
  );
}

#[test]
fn t_stmts_semicolon_terminated() {
  let s = "1; 2; 3;";
  assert_eq!(
    statements(s),
    Ok((
      "",
      vec![
        Statement::Expression(Expression::NumLiteral(1.)),
        Statement::Expression(Expression::NumLiteral(2.)),
        Statement::Expression(Expression::NumLiteral(3.))
      ]
    ))
  );
}

fn statements_finish(
  i: &str,
) -> Result<Statements, nom::error::Error<&str>> {
  let (_, res) = statements(i).finish()?;
  Ok(res)
}
