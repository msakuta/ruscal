use std::{
  fmt::Display,
  io::{BufReader, BufWriter, Read, Write},
};

use ::rusty_programmer::{dprintln, parse_args, RunMode};

use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::{
    alpha1, alphanumeric1, char, multispace0,
  },
  combinator::{opt, recognize},
  error::ParseError,
  multi::{fold_many0, many0},
  number::complete::recognize_float,
  sequence::{delimited, pair, preceded},
  IResult, Parser,
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
  /// Less-than operator
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
      Self::Str(value) => serialize_str(value, writer)?,
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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
/// Absolute Stack Index
struct StkIdx(usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
/// Instruction Pointer
struct InstPtr(usize);

struct Compiler {
  literals: Vec<Value>,
  instructions: Vec<Instruction>,
  target_stack: Vec<usize>,
}

impl Compiler {
  fn new() -> Self {
    Self {
      literals: vec![],
      instructions: vec![],
      target_stack: vec![],
    }
  }

  fn stack_top(&self) -> StkIdx {
    StkIdx(self.target_stack.len() - 1)
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
    self.target_stack.push(0);
    inst
  }

  fn add_store_inst(&mut self, stack_idx: StkIdx) -> InstPtr {
    let inst = self.add_inst(
      OpCode::Store,
      (self.target_stack.len() - stack_idx.0 - 1) as u8,
    );
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
    self.target_stack.resize(stack_idx.0 + 1, 0);
    Some(inst)
  }

  fn write_literals(
    &self,
    writer: &mut impl Write,
  ) -> std::io::Result<()> {
    serialize_size(self.literals.len(), writer)?;
    for value in &self.literals {
      value.serialize(writer)?;
    }
    Ok(())
  }

  fn write_insts(
    &self,
    writer: &mut impl Write,
  ) -> std::io::Result<()> {
    serialize_size(self.instructions.len(), writer)?;
    for instruction in &self.instructions {
      instruction.serialize(writer).unwrap();
    }
    Ok(())
  }

  fn compile_expr(&mut self, ex: &Expression) -> StkIdx {
    match ex {
      Expression::NumLiteral(num) => {
        let id = self.add_literal(Value::F64(*num));
        self.add_inst(OpCode::LoadLiteral, id);
        self.target_stack.push(id as usize);
        self.stack_top()
      }
      Expression::Ident("pi") => {
        let id =
          self.add_literal(Value::F64(std::f64::consts::PI));
        self.add_inst(OpCode::LoadLiteral, id);
        self.target_stack.push(id as usize);
        self.stack_top()
      }
      Expression::Ident(id) => {
        panic!("Unknown identifier {id:?}");
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
        let stack_before_call = self.target_stack.len();
        let name =
          self.add_literal(Value::Str(name.to_string()));
        let args = args
          .iter()
          .map(|arg| self.compile_expr(arg))
          .collect::<Vec<_>>();

        self.add_inst(OpCode::LoadLiteral, name);
        self.target_stack.push(0);
        for arg in &args {
          self.add_copy_inst(*arg);
          self.target_stack.push(0);
        }

        self.add_inst(OpCode::Call, args.len() as u8);
        self.target_stack.resize(stack_before_call + 1, 0);
        self.stack_top()
      }
      Expression::If(cond, true_branch, false_branch) => {
        use OpCode::*;
        let cond = self.compile_expr(cond);
        self.add_copy_inst(cond);
        let jf_inst = self.add_inst(Jf, 0);
        let stack_size_before = self.target_stack.len();
        self.compile_expr(true_branch);
        self.collapse_stack(StkIdx(stack_size_before + 1));
        if let Some(false_branch) = false_branch.as_ref() {
          let jmp_inst = self.add_inst(Jmp, 0);
          self.fixup_jmp(jf_inst);
          self.target_stack.resize(stack_size_before, 0);
          self.compile_expr(&false_branch);
          self.collapse_stack(StkIdx(stack_size_before + 1));
          self.fixup_jmp(jmp_inst);
        } else {
          self.fixup_jmp(jf_inst);
        }
        self.stack_top()
      }
    }
  }

  fn bin_op(
    &mut self,
    op: OpCode,
    lhs: &Expression,
    rhs: &Expression,
  ) -> StkIdx {
    let lhs = self.compile_expr(lhs);
    let rhs = self.compile_expr(rhs);
    self.add_copy_inst(lhs);
    self.add_copy_inst(rhs);
    self.add_inst(op, 0);
    self.target_stack.push(usize::MAX);
    self.stack_top()
  }

  /// Pop the stack, store it at the target, removing all items higher than the target.
  /// If the target is equal to or higher than the top, does nothing.
  fn collapse_stack(&mut self, target: StkIdx) {
    if target.0 < self.target_stack.len() - 1 {
      self.add_store_inst(StkIdx(target.0));
      self.add_pop_until_inst(StkIdx(target.0));
    }
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
        LoadLiteral => writeln!(
          writer,
          "  [{i}] {:?} {} ({:?})",
          inst.op, inst.arg0, self.literals[inst.arg0 as usize]
        )?,
        Copy | Call | Jmp | Jf | Pop | Store => writeln!(
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
  writer: &mut impl Write,
  out_file: &str,
  disasm: bool,
) -> std::io::Result<()> {
  let mut compiler = Compiler::new();
  let (_, ex) = expr(source).map_err(|e| {
    std::io::Error::new(std::io::ErrorKind::Other, e.to_owned())
  })?;

  compiler.compile_expr(&ex);

  if disasm {
    compiler.disasm(&mut std::io::stdout())?;
  }

  compiler.write_literals(writer).unwrap();
  compiler.write_insts(writer).unwrap();
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
    let num_literals = deserialize_size(reader)?;
    for _ in 0..num_literals {
      self.literals.push(Value::deserialize(reader)?);
    }
    Ok(())
  }

  fn read_instructions(
    &mut self,
    reader: &mut impl Read,
  ) -> std::io::Result<()> {
    let num_instructions = deserialize_size(reader)?;
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

fn read_program(
  reader: &mut impl Read,
) -> std::io::Result<ByteCode> {
  let mut bytecode = ByteCode::new();
  bytecode.read_literals(reader)?;
  bytecode.read_instructions(reader)?;
  Ok(bytecode)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let Some(args) = parse_args() else { return Ok(()) };

  match args.run_mode {
    RunMode::Compile => {
      if let Some(expr) = args.source {
        let writer = std::fs::File::create(&args.output)?;
        let mut writer = BufWriter::new(writer);
        write_program(
          &expr,
          &mut writer,
          &args.output,
          args.disasm,
        )?;
      }
    }
    RunMode::Run(code_file) => {
      let reader = std::fs::File::open(&code_file)?;
      let mut reader = BufReader::new(reader);
      match read_program(&mut reader) {
        Ok(bytecode) => {
          let result = bytecode.interpret();
          println!("result: {result:?}");
        }
        Err(e) => eprintln!("Read program error: {e:?}"),
      }
    }
    RunMode::CompileAndRun => {
      if let Some(expr) = args.source {
        let mut buf = vec![];
        write_program(
          &expr,
          &mut std::io::Cursor::new(&mut buf),
          "<Memory>",
          args.disasm,
        )?;
        let bytecode =
          read_program(&mut std::io::Cursor::new(&mut buf))?;
        let result = bytecode.interpret();
        println!("result: {result:?}");
      }
    }
    _ => println!("Please specify -c or -r as an argument"),
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
    alt((delimited(open_brace, expr, close_brace), if_expr)),
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
