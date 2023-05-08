use std::{
  fmt::Display,
  io::{BufReader, BufWriter, Read, Write},
};

use ::rusty_programmer::{dprintln, parse_args, RunMode};

use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::{
    alpha1, alphanumeric1, char, multispace0, multispace1,
  },
  combinator::{opt, recognize},
  error::ParseError,
  multi::{fold_many0, many0},
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
  Pop
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
        writer.write_all(&value.len().to_le_bytes())?;
        writer.write_all(value.as_bytes())?;
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
      Str => {
        let mut len_buf = [0u8; std::mem::size_of::<usize>()];
        reader.read_exact(&mut len_buf)?;
        let len = usize::from_le_bytes(len_buf);
        let mut str_buf = vec![0u8; len];
        reader.read_exact(&mut str_buf)?;
        let str = String::from_utf8(str_buf).map_err(|e| {
          std::io::Error::new(std::io::ErrorKind::Other, e)
        })?;
        Ok(Value::Str(str))
      }
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

struct Compiler {
  literals: Vec<Value>,
  instructions: Vec<Instruction>,
  target_stack: Vec<Target>,
}

impl Compiler {
  fn new() -> Self {
    Self {
      literals: vec![],
      instructions: vec![],
      target_stack: vec![],
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

  fn write_literals(
    &self,
    writer: &mut impl Write,
  ) -> std::io::Result<()> {
    writer.write_all(&self.literals.len().to_le_bytes())?;
    for value in &self.literals {
      value.serialize(writer)?;
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
      Expression::FnInvoke(name, args) => {
        let stack_before_args = self.target_stack.len();
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
        let pop = stack_before_call - stack_before_args;
        if 0 < pop {
          self.add_inst(OpCode::Pop, pop as u8);
          self
            .target_stack
            .resize(stack_before_args + 1, Target::Temp);
        }
        self.target_stack.len() - 1
      }
      Expression::If(cond, true_branch, false_branch) => {
        use OpCode::*;
        let cond = self.compile_expr(cond);
        self.add_inst(
          Copy,
          (self.target_stack.len() - cond - 1) as u8,
        );
        let jf_inst = self.add_inst(Jf, 0);
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
  ) -> Result<(), String> {
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
          self.add_inst(
            OpCode::Store,
            (self.target_stack.len() - stk_local - 1) as u8,
          );
          self.target_stack.pop();
        }
        Statement::For {
          loop_var,
          start,
          end,
          stmts,
        } => {
          let start = self.compile_expr(start);
          let end = self.compile_expr(end);
          dprintln!("start: {start} end: {end}");
          self.add_copy_inst(start);
          let stk_loop_var = self.target_stack.len() - 1;
          self.target_stack[stk_loop_var] =
            Target::Local(loop_var.to_string());
          dprintln!("after start: {:?}", self.target_stack);
          let stk_check_exit = self.target_stack.len();
          self.add_copy_inst(stk_loop_var);
          self.add_inst(
            OpCode::Lt,
            (self.target_stack.len() - end - 1) as u8,
          );
          let jf_inst = self.add_inst(OpCode::Jf, 0);
          self.target_stack.pop();
          dprintln!("start in loop: {:?}", self.target_stack);
          self.compile_stmts(stmts)?;
          let one = self.add_literal(Value::F64(1.));
          dprintln!("end in loop: {:?}", self.target_stack);
          self.add_copy_inst(stk_loop_var);
          self.add_inst(OpCode::LoadLiteral, one);
          self.target_stack.push(Target::Literal(one as usize));
          self.add_inst(OpCode::Add, 0);
          self.target_stack.pop();
          self.add_inst(
            OpCode::Store,
            (self.target_stack.len() - stk_loop_var - 1) as u8,
          );
          self.add_inst(
            OpCode::Pop,
            (self.target_stack.len() - stk_loop_var - 2) as u8,
          );
          self.add_inst(OpCode::Jmp, stk_check_exit as u8);
          self.instructions[jf_inst].arg0 =
            self.instructions.len() as u8;
        }
      }
    }
    Ok(())
  }

  fn disasm(
    &self,
    writer: &mut impl Write,
  ) -> std::io::Result<()> {
    use OpCode::*;
    writeln!(writer, "Literals [{}]", self.literals.len())?;
    for (i, con) in self.literals.iter().enumerate() {
      writeln!(writer, "  [{i}] {}", *con)?;
    }

    writeln!(
      writer,
      "Instructions [{}]",
      self.instructions.len()
    )?;
    for (i, inst) in self.instructions.iter().enumerate() {
      match inst.op {
        LoadLiteral | Copy | Call | Jmp | Jf | Lt | Pop
        | Store => writeln!(
          writer,
          "  [{i}] {:?} {}",
          inst.op, inst.arg0
        )?,
        _ => writeln!(writer, "  [{i}] {:?}", inst.op)?,
      }
    }
    Ok(())
  }
}

fn write_program(
  source: &str,
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

  compiler.compile_stmts(&stmts)?;

  if disasm {
    compiler.disasm(&mut std::io::stdout())?;
  }

  let writer = std::fs::File::create(out_file)?;
  let mut writer = BufWriter::new(writer);
  compiler.write_literals(&mut writer).unwrap();
  compiler.write_insts(&mut writer).unwrap();
  println!(
    "Written {} literals and {} instructions to {out_file:?}",
    compiler.literals.len(),
    compiler.instructions.len()
  );
  Ok(())
}

struct ByteCode {
  literals: Vec<Value>,
  instructions: Vec<Instruction>,
}

impl ByteCode {
  fn new() -> Self {
    Self {
      literals: vec![],
      instructions: vec![],
    }
  }

  fn read_literals(
    &mut self,
    reader: &mut impl Read,
  ) -> std::io::Result<()> {
    let mut buf = [0; std::mem::size_of::<usize>()];
    reader.read_exact(&mut buf)?;
    let num_literals = usize::from_le_bytes(buf);
    for _ in 0..num_literals {
      self.literals.push(Value::deserialize(reader)?);
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

  fn interpret(&self) -> Option<Value> {
    let mut stack = vec![];
    let mut ip = 0;

    while ip < self.instructions.len() {
      let instruction = &self.instructions[ip];
      dprintln!(
        "interpret[{ip}]: {instruction:?} stack: {stack:?}"
      );
      match instruction.op {
        OpCode::LoadLiteral => {
          stack.push(
            self.literals[instruction.arg0 as usize].clone(),
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
            _ => panic!("Unknown function name {fname:?}"),
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
        OpCode::Lt => {
          let lhs = stack.pop().expect("Lt needs an argument");
          let rhs =
            &stack[stack.len() - instruction.arg0 as usize];
          let res = lhs.coerce_f64() < rhs.coerce_f64();
          stack.push(Value::F64(res as i32 as f64));
        }
        OpCode::Pop => {
          stack.resize(
            stack.len() - instruction.arg0 as usize,
            Value::default(),
          );
        }
      }
      ip += 1;
    }

    stack.pop()
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

fn print_fn(args: &[Value]) -> Value {
  for arg in args {
    print!("{:?} ", arg);
  }
  println!("");
  Value::F64(0.)
}

fn read_program(file: &str) -> std::io::Result<ByteCode> {
  let reader = std::fs::File::open(file)?;
  let mut reader = BufReader::new(reader);
  let mut bytecode = ByteCode::new();
  bytecode.read_literals(&mut reader)?;
  bytecode.read_instructions(&mut reader)?;
  Ok(bytecode)
}

fn main() {
  let Some(args) = parse_args() else { return };

  match args.run_mode {
    RunMode::Compile => {
      if let Some(source) = args
        .source
        .and_then(|src| std::fs::read_to_string(src).ok())
      {
        write_program(&source, &args.output, args.disasm)
          .unwrap()
      } else {
        println!(
          "Please specify source file to compile after -c"
        );
      }
    }
    RunMode::Run(code_file) => match read_program(&code_file) {
      Ok(bytecode) => {
        let result = bytecode.interpret();
        println!("result: {result:?}");
      }
      Err(e) => eprintln!("Read program error: {e:?}"),
    },
    _ => println!("Please specify -c or -r as an argument"),
  }
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
  alt((if_expr, num_expr))(i)
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

fn statement(i: &str) -> IResult<&str, Statement> {
  alt((
    for_statement,
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
