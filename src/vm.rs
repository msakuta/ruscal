use std::{cell::RefCell, error::Error, rc::Rc};

use crate::{
  bytecode::{ByteCode, FnByteCode, FnDef},
  // dprintln,
  instructions::{Instruction, OpCode},
  value::Value,
};

pub enum YieldResult {
  Finished(Value),
  Suspend(Value),
}

pub struct StackFrame {
  fn_def: Rc<FnByteCode>,
  args: usize,
  stack: Vec<Value>,
  ip: usize,
}

impl StackFrame {
  fn new(fn_def: Rc<FnByteCode>, args: Vec<Value>) -> Self {
    Self {
      fn_def,
      args: args.len(),
      stack: args,
      ip: 0,
    }
  }

  fn inst(&self) -> Option<Instruction> {
    let ret = self.fn_def.instructions.get(self.ip)?;
    // dprintln!(
    //   "interpret[{}]: {:?} stack: {:?}",
    //   self.ip,
    //   ret,
    //   self.stack
    // );
    Some(*ret)
  }
}

pub struct Vm {
  bytecode: Rc<ByteCode>,
  stack_frames: Vec<StackFrame>,
  user_data: Box<dyn std::any::Any>,
  debug_output: bool,
}

impl std::fmt::Debug for Vm {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    write!(f, "<Vm>")
  }
}

impl Vm {
  pub fn new(
    bytecode: Rc<ByteCode>,
    user_data: Box<dyn std::any::Any>,
    debug_output: bool,
  ) -> Self {
    Self {
      bytecode,
      stack_frames: vec![],
      user_data,
      debug_output,
    }
  }

  pub fn top(&self) -> Result<&StackFrame, String> {
    self
      .stack_frames
      .last()
      .ok_or_else(|| "Stack frame underflow".to_string())
  }

  fn top_mut(&mut self) -> Result<&mut StackFrame, String> {
    self
      .stack_frames
      .last_mut()
      .ok_or_else(|| "Stack frame underflow".to_string())
  }

  /// A convenience function to run a function without the
  /// ability to suspend execution.
  /// An yield instruction would be an error.
  #[allow(dead_code)]
  fn run_fn(
    &mut self,
    fn_name: &str,
    args: &[Value],
  ) -> Result<Value, Box<dyn Error>> {
    let fn_def =
      self.bytecode.funcs.get(fn_name).ok_or_else(|| {
        format!("Function {fn_name:?} was not found")
      })?;
    let fn_def = match fn_def {
      FnDef::User(user) => user.clone(),
      FnDef::Native(n) => {
        return Ok((*n.code)(self.user_data.as_ref(), args))
      }
    };

    self
      .stack_frames
      .push(StackFrame::new(fn_def, args.to_vec()));

    match self.interpret()? {
      YieldResult::Finished(val) => Ok(val),
      YieldResult::Suspend(_) => {
        Err("Yielded at toplevel".into())
      }
    }
  }

  pub fn init_fn(
    &mut self,
    fn_name: &str,
    args: &[Value],
  ) -> Result<(), Box<dyn Error>> {
    let fn_def =
      self.bytecode.funcs.get(fn_name).ok_or_else(|| {
        format!("Function {fn_name:?} was not found")
      })?;
    let fn_def = match fn_def {
        FnDef::User(user) => user.clone(),
        FnDef::Native(_) => return Err("Native function cannot be called as a coroutine. Use `run_fn` instead.".into()),
      };

    self
      .stack_frames
      .push(StackFrame::new(fn_def, args.to_vec()));

    Ok(())
  }

  fn return_fn(
    &mut self,
  ) -> Result<Option<YieldResult>, Box<dyn Error>> {
    let top_frame = self.top_mut()?;
    let res = top_frame.stack.pop().ok_or_else(|| {
      format!(
        "Stack underflow at Ret ({}) {:?}",
        top_frame.ip, top_frame.stack
      )
    })?;
    let args = top_frame.args;

    if self.stack_frames.pop().is_none()
      || self.stack_frames.is_empty()
    {
      return Ok(Some(YieldResult::Finished(res)));
    }

    if self.debug_output {
      println!("Returning {}", res);
    }

    let stack = &mut self.top_mut()?.stack;
    stack.resize(stack.len() - args - 1, Value::F64(0.));
    stack.push(res);
    self.top_mut()?.ip += 1;
    Ok(None)
  }

  pub fn interpret(
    &mut self,
  ) -> Result<YieldResult, Box<dyn Error>> {
    loop {
      let (instruction, ip) =
        if let Some(instruction) = self.top()?.inst() {
          (instruction, self.top()?.ip)
        } else {
          if let Some(res) = self.return_fn()? {
            return Ok(res);
          }
          continue;
        };

      if self.debug_output {
        println!(
          "interpret[{ip}]: {instruction:?} stack: {stack:?}",
          stack = self.top()?.stack
        );
      }

      match instruction.op {
        OpCode::LoadLiteral => {
          let stack_frame = self.top_mut()?;
          stack_frame.stack.push(
            stack_frame.fn_def.literals
              [instruction.arg0 as usize]
              .clone(),
          );
        }
        OpCode::Store => {
          let stack = &mut self.top_mut()?.stack;
          let idx = stack.len() - instruction.arg0 as usize - 1;
          let value =
            stack.pop().expect("Store needs an argument");
          stack[idx] = value;
        }
        OpCode::Copy => {
          let stack = &mut self.top_mut()?.stack;
          stack.push(
            stack[stack.len() - instruction.arg0 as usize - 1]
              .clone(),
          );
        }
        OpCode::Add => Self::interpret_bin_op_str(
          &mut self.top_mut()?.stack,
          |lhs, rhs| lhs + rhs,
          |lhs, rhs| lhs + rhs,
          |lhs, rhs| Some(format!("{lhs}{rhs}")),
        ),
        OpCode::Sub => Self::interpret_bin_op(
          &mut self.top_mut()?.stack,
          |lhs, rhs| lhs - rhs,
          |lhs, rhs| lhs - rhs,
        ),
        OpCode::Mul => Self::interpret_bin_op(
          &mut self.top_mut()?.stack,
          |lhs, rhs| lhs * rhs,
          |lhs, rhs| lhs * rhs,
        ),
        OpCode::Div => Self::interpret_bin_op(
          &mut self.top_mut()?.stack,
          |lhs, rhs| lhs / rhs,
          |lhs, rhs| lhs / rhs,
        ),
        OpCode::Call => {
          let stack = &self.top()?.stack;
          let args =
            &stack[stack.len() - instruction.arg0 as usize..];
          let fname =
            &stack[stack.len() - instruction.arg0 as usize - 1];
          let Value::Str(fname) = fname else {
                panic!("Function name shall be a string: {fname:?} in fn {:?}", self.top()?.stack);
              };
          let fn_def =
            self.bytecode.funcs.get(fname).ok_or_else(
              || {
                format!(
                  "Function name shall be a string: {fname:?}"
                )
              },
            )?;
          match fn_def {
            FnDef::User(user_fn) => {
              if user_fn.cofn {
                let mut vm = Vm::new(
                  self.bytecode.clone(),
                  Box::new(()),
                  self.debug_output,
                );
                vm.stack_frames.push(StackFrame::new(
                  user_fn.clone(),
                  args.to_vec(),
                ));
                let stack = &mut self.top_mut()?.stack;
                stack.resize(
                  stack.len() - instruction.arg0 as usize - 1,
                  Value::F64(0.),
                );
                stack
                  .push(Value::Coro(Rc::new(RefCell::new(vm))));
              } else {
                self.stack_frames.push(StackFrame::new(
                  user_fn.clone(),
                  args.to_vec(),
                ));
                continue;
              }
            }
            FnDef::Native(native) => {
              let res =
                (native.code)(self.user_data.as_ref(), args);
              let stack = &mut self.top_mut()?.stack;
              stack.resize(
                stack.len() - instruction.arg0 as usize - 1,
                Value::F64(0.),
              );
              stack.push(res);
            }
          }
        }
        OpCode::Jmp => {
          self.top_mut()?.ip = instruction.arg0 as usize;
          continue;
        }
        OpCode::Jf => {
          let stack = &mut self.top_mut()?.stack;
          let cond = stack.pop().expect("Jf needs an argument");
          if cond.coerce_f64() == Ok(0.) {
            self.top_mut()?.ip = instruction.arg0 as usize;
            continue;
          }
        }
        OpCode::Lt => Self::interpret_bin_op(
          &mut self.top_mut()?.stack,
          |lhs, rhs| (lhs < rhs) as i32 as f64,
          |lhs, rhs| (lhs < rhs) as i64,
        ),
        OpCode::Pop => {
          let stack = &mut self.top_mut()?.stack;
          stack.resize(
            stack.len() - instruction.arg0 as usize,
            Value::default(),
          );
        }
        OpCode::Ret => {
          if let Some(res) = self.return_fn()? {
            return Ok(res);
          }
          continue;
        }
        OpCode::Yield => {
          let top_frame = self.top_mut()?;
          let res = top_frame
            .stack
            .pop()
            .ok_or_else(|| "Stack underflow".to_string())?;
          // Increment the ip for the next call
          top_frame.ip += 1;
          return Ok(YieldResult::Suspend(res));
        }
        OpCode::Await => {
          let vms = self
            .top_mut()?
            .stack
            .pop()
            .ok_or_else(|| "Stack underflow".to_string())?;
          let Value::Coro(vm) = vms else {
              return Err("Await keyword applied to a non-coroutine".into());
            };
          match vm.borrow_mut().interpret() {
            Ok(YieldResult::Finished(_)) => (),
            Ok(YieldResult::Suspend(value)) => {
              self.top_mut()?.stack.push(value);
            }
            Err(e) => {
              eprintln!("Runtime error: {e:?}");
            }
          };
        }
      }
      self.top_mut()?.ip += 1;
    }
  }

  fn interpret_bin_op_str(
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
    stack: &mut Vec<Value>,
    op_f64: impl FnOnce(f64, f64) -> f64,
    op_i64: impl FnOnce(i64, i64) -> i64,
  ) {
    Self::interpret_bin_op_str(stack, op_f64, op_i64, |_, _| {
      None
    })
  }

  fn back_trace(&self) {
    for (i, frame) in self.stack_frames.iter().rev().enumerate()
    {
      println!("[{}]: {:?}", i, frame.stack);
    }
  }
}

pub fn debugger(vm: &Vm) -> bool {
  println!("[c]ontinue/[p]rint/[e]xit/[bt]race?");
  loop {
    let mut buffer = String::new();
    if std::io::stdin().read_line(&mut buffer).is_ok() {
      match buffer.trim() {
        "c" => return false,
        "p" => {
          println!("Stack: {:?}", vm.top().unwrap().stack);
        }
        "e" => return true,
        "bt" => vm.back_trace(),
        _ => println!(
          "Please say [c]ontinue/[p]rint/[b]reak/[bt]race"
        ),
      }
    }
  }
}
