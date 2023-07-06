use std::{
  error::Error,
  io::{Read, Write},
};

use crate::{
  ast::{Span, Statements},
  bytecode::ByteCode,
  compiler::Compiler,
  parser::statements_finish,
  type_checker::{type_check, TypeCheckContext},
  Args, RunMode,
};

pub fn parse_program<'src>(
  source_file: &str,
  source: &'src str,
) -> Result<Statements<'src>, Box<dyn Error>> {
  statements_finish(Span::new(source)).map_err(|e| {
    format!(
      "{}:{}:{}: {}",
      source_file,
      e.input.location_line(),
      e.input.get_utf8_column(),
      e
    )
    .into()
  })
}

pub fn write_program(
  source_file: &str,
  source: &str,
  writer: &mut impl Write,
  _out_file: &str,
  args: &Args,
) -> Result<(), Box<dyn std::error::Error>> {
  let mut compiler = Compiler::new();
  let stmts = parse_program(source_file, source)?;

  if args.show_ast {
    println!("AST: {stmts:#?}");
  }

  match type_check(&stmts, &mut TypeCheckContext::new()) {
    Ok(_) => println!("Typecheck Ok"),
    Err(e) => {
      return Err(
        format!(
          "{}:{}:{}: {}",
          source_file,
          e.span.location_line(),
          e.span.get_utf8_column(),
          e
        )
        .into(),
      )
    }
  }

  if matches!(args.run_mode, RunMode::TypeCheck) {
    return Ok(());
  }

  compiler.compile(&stmts)?;

  if args.disasm {
    compiler.disasm(&mut std::io::stdout())?;
  }

  compiler.write_funcs(writer)?;
  // dprintln!(
  //   "Written {} literals and {} instructions to {out_file:?}",
  //   compiler.literals.len(),
  //   compiler.instructions.len()
  // );
  Ok(())
}

pub fn compile(
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
  write_program(src, &source, writer, out_file, args)
}

pub fn read_program(
  reader: &mut impl Read,
) -> std::io::Result<ByteCode> {
  let mut bytecode = ByteCode::new();
  bytecode.read_funcs(reader)?;
  Ok(bytecode)
}
