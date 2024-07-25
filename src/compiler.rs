use std::{
  collections::HashMap, error::Error, fmt::Display, io::Write,
  rc::Rc,
};

use crate::{
  ast::{
    ExprEnum, Expression, Span, Statement, Statements, TypeDecl,
  },
  bytecode::{
    standard_functions, ByteCode, FnByteCode, FnDecl, FnDef,
  },
  instructions::{Instruction, OpCode},
  value::{serialize_size, serialize_str, Value},
};

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
  #[allow(dead_code)]
  Literal(usize),
  Local(String),
}

struct LoopFrame {
  start: StkIdx,
  break_ips: Vec<InstPtr>,
  continue_ips: Vec<(InstPtr, usize)>,
}

impl LoopFrame {
  fn new(stack_start: StkIdx) -> Self {
    Self {
      start: stack_start,
      break_ips: vec![],
      continue_ips: vec![],
    }
  }
}

#[derive(Debug)]
struct LoopStackUnderflowError;

impl Display for LoopStackUnderflowError {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    write!(f, "A break or continue statement outside loop")
  }
}

impl Error for LoopStackUnderflowError {}

pub struct Compiler {
  literals: Vec<Value>,
  instructions: Vec<Instruction>,
  target_stack: Vec<Target>,
  funcs: HashMap<String, FnByteCode>,
  loop_stack: Vec<LoopFrame>,
}

impl Compiler {
  pub fn new() -> Self {
    Self {
      literals: vec![],
      instructions: vec![],
      target_stack: vec![],
      funcs: HashMap::new(),
      loop_stack: vec![],
    }
  }

  pub fn into_bytecode(self) -> ByteCode {
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
    for (key, value) in self.funcs {
      funcs.insert(key, FnDef::User(Rc::new(value)));
    }

    ByteCode { funcs }
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

  fn fixup_continues(&mut self) -> Result<(), Box<dyn Error>> {
    let loop_frame =
      self.loop_stack.last().ok_or(LoopStackUnderflowError)?;
    let continue_jmp_addr = self.instructions.len();
    for (ip, stk) in &loop_frame.continue_ips {
      self.instructions[ip.0].arg0 =
        (self.target_stack.len() - stk) as u8;
      self.instructions[ip.0 + 1].arg0 =
        continue_jmp_addr as u8;
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

  fn add_load_literal_inst(&mut self, lit: u8) -> InstPtr {
    let inst = self.add_inst(OpCode::LoadLiteral, lit);
    self.target_stack.push(Target::Literal(lit as usize));
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
    args: &[(Span, TypeDecl)],
    cofn: bool,
  ) {
    self.funcs.insert(
      name,
      FnByteCode::new(
        args.iter().map(|(arg, _)| arg.to_string()).collect(),
        std::mem::take(&mut self.literals),
        std::mem::take(&mut self.instructions),
        cofn,
      ),
    );
  }

  pub(crate) fn write_funcs(
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
    Ok(match &ex.expr {
      ExprEnum::NumLiteral(num) => {
        let id = self.add_literal(Value::F64(*num));
        self.add_load_literal_inst(id);
        self.stack_top()
      }
      ExprEnum::StrLiteral(str) => {
        let id = self.add_literal(Value::Str(str.clone()));
        self.add_load_literal_inst(id);
        self.stack_top()
      }
      ExprEnum::Ident(ident) => {
        let var = self.target_stack.iter().enumerate().find(
          |(_i, tgt)| {
            if let Target::Local(id) = tgt {
              id == ident.fragment()
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
      ExprEnum::Add(lhs, rhs) => {
        self.bin_op(OpCode::Add, lhs, rhs)?
      }
      ExprEnum::Sub(lhs, rhs) => {
        self.bin_op(OpCode::Sub, lhs, rhs)?
      }
      ExprEnum::Mul(lhs, rhs) => {
        self.bin_op(OpCode::Mul, lhs, rhs)?
      }
      ExprEnum::Div(lhs, rhs) => {
        self.bin_op(OpCode::Div, lhs, rhs)?
      }
      ExprEnum::Gt(lhs, rhs) => {
        self.bin_op(OpCode::Lt, rhs, lhs)?
      }
      ExprEnum::Lt(lhs, rhs) => {
        self.bin_op(OpCode::Lt, lhs, rhs)?
      }
      ExprEnum::FnInvoke(name, args) => {
        let stack_before_args = self.target_stack.len();
        let name =
          self.add_literal(Value::Str(name.to_string()));
        let args = args
          .iter()
          .map(|arg| self.compile_expr(arg))
          .collect::<Result<Vec<_>, _>>()?;

        let stack_before_call = self.target_stack.len();
        self.add_load_literal_inst(name);
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
      ExprEnum::If(cond, true_branch, false_branch) => {
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
      ExprEnum::Await(ex) => {
        let res = self.compile_expr(ex)?;
        self.add_copy_inst(res);
        self.add_inst(OpCode::Await, 0);
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
        Statement::VarDef { name, ex, .. } => {
          let mut ex = self.compile_expr(ex)?;
          if !matches!(self.target_stack[ex.0], Target::Temp) {
            self.add_copy_inst(ex);
            ex = self.stack_top();
          }
          self.target_stack[ex.0] =
            Target::Local(name.to_string());
        }
        Statement::VarAssign { name, ex, .. } => {
          let stk_ex = self.compile_expr(ex)?;
          let (stk_local, _) = self
            .target_stack
            .iter_mut()
            .enumerate()
            .find(|(_, tgt)| {
              if let Target::Local(tgt) = tgt {
                tgt == name.fragment()
              } else {
                false
              }
            })
            .ok_or_else(|| {
              format!("Variable name not found: {name}")
            })?;
          self.add_copy_inst(stk_ex);
          self.add_store_inst(StkIdx(stk_local));
        }
        Statement::For {
          loop_var,
          start,
          end,
          stmts,
          ..
        } => {
          let stk_start = self.compile_expr(start)?;
          let stk_end = self.compile_expr(end)?;
          // dprintln!("start: {stk_start:?} end: {stk_end:?}");
          self.add_copy_inst(stk_start);
          let stk_loop_var = self.stack_top();
          self.target_stack[stk_loop_var.0] =
            Target::Local(loop_var.to_string());
          // dprintln!("after start: {:?}", self.target_stack);
          let inst_check_exit = self.instructions.len();
          self.add_copy_inst(stk_loop_var);
          self.add_copy_inst(stk_end);
          // dprintln!("before cmp: {:?}", self.target_stack);
          self.add_binop_inst(OpCode::Lt);
          let jf_inst = self.add_jf_inst();
          // dprintln!("start in loop: {:?}", self.target_stack);
          self.loop_stack.push(LoopFrame::new(stk_loop_var));
          self.compile_stmts(stmts)?;
          self.fixup_continues()?;
          let one = self.add_literal(Value::F64(1.));
          // dprintln!("end in loop: {:?}", self.target_stack);
          self.add_copy_inst(stk_loop_var);
          self.add_load_literal_inst(one);
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
        Statement::Continue => {
          let start = self
            .loop_stack
            .last()
            .map(|frame| frame.start)
            .ok_or(LoopStackUnderflowError)?;
          self.add_pop_until_inst(start);

          let loop_frame = self
            .loop_stack
            .last_mut()
            .ok_or(LoopStackUnderflowError)?;
          let continue_ip = self.instructions.len();
          loop_frame.continue_ips.push((
            InstPtr(continue_ip),
            self.target_stack.len(),
          ));
          self.add_inst(OpCode::Dup, 0);
          self.add_inst(OpCode::Jmp, 0);
        }
        Statement::FnDef {
          name,
          args,
          stmts,
          cofn,
          ..
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
          self.add_fn(name.to_string(), args, *cofn);
          self.literals = literals;
          self.instructions = instructions;
          self.target_stack = target_stack;
        }
        Statement::Return(ex) => {
          let res = self.compile_expr(ex)?;
          self.add_inst(
            OpCode::Ret,
            (self.target_stack.len() - res.0 - 1) as u8,
          );
        }
        Statement::Yield(ex) => {
          let res = self.compile_expr(ex)?;
          self.add_inst(
            OpCode::Yield,
            (self.target_stack.len() - res.0 - 1) as u8,
          );
          self.target_stack.pop();
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
      self.add_load_literal_inst(id);
      self.stack_top()
    }))
  }

  pub fn compile(
    &mut self,
    stmts: &Statements,
  ) -> Result<(), Box<dyn std::error::Error>> {
    let name = "main";
    self.compile_stmts_or_zero(stmts)?;
    self.add_fn(name.to_string(), &[], false);
    Ok(())
  }

  pub fn disasm(
    &self,
    writer: &mut impl Write,
  ) -> std::io::Result<()> {
    for (name, fn_def) in &self.funcs {
      if fn_def.cofn {
        writeln!(writer, "Coroutine {name:?}:")?;
      } else {
        writeln!(writer, "Function {name:?}:")?;
      }
      fn_def.disasm(writer)?;
    }
    Ok(())
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
      Copy | Call | Jmp | Jf | Pop | Store | Ret => writeln!(
        writer,
        "    [{i}] {:?} {}",
        inst.op, inst.arg0
      )?,
      _ => writeln!(writer, "    [{i}] {:?}", inst.op)?,
    }
  }
  Ok(())
}
