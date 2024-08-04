use std::{
  io::{BufReader, BufWriter},
  rc::Rc,
};

use ruscal::{
  file_io::{compile, read_program},
  parse_args,
  value::Value,
  vm::{debugger, Vm, YieldResult},
  RunMode,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let Some(args) = parse_args(true) else {
    return Ok(());
  };

  let run_coro = |mut vm: Vm| {
    if let Err(e) = vm.init_fn("main", &[]) {
      eprintln!("init_fn error: {e:?}");
    }
    loop {
      match vm.interpret() {
        Ok(YieldResult::Finished(_)) => break,
        Ok(YieldResult::Suspend(value)) => {
          println!(
            "Execution suspended with a yielded value {value}"
          );
          if value == Value::Str("break".to_string())
            && debugger(&vm)
          {
            break;
          }
        }
        Err(e) => {
          eprintln!("Runtime error: {e:?}");
          break;
        }
      }
    }
  };

  match args.run_mode {
    RunMode::TypeCheck => {
      if let Err(e) =
        compile(&mut std::io::sink(), &args, &args.output)
      {
        eprintln!("TypeCheck error: {e}");
      }
    }
    RunMode::Compile => {
      let writer = std::fs::File::create(&args.output)?;
      let mut writer = BufWriter::new(writer);
      if let Err(e) = compile(&mut writer, &args, &args.output)
      {
        eprintln!("Compile Error: {e}");
      }
    }
    RunMode::Run(code_file) => {
      let reader = std::fs::File::open(&code_file)?;
      let mut reader = BufReader::new(reader);
      let bytecode = Rc::new(read_program(&mut reader)?);
      run_coro(Vm::new(
        bytecode,
        Box::new(()),
        args.debug_output,
      ));
    }
    RunMode::CompileAndRun => {
      let mut buf = vec![];
      if let Err(e) = compile(
        &mut std::io::Cursor::new(&mut buf),
        &args,
        "<Memory>",
      ) {
        eprintln!("Compile Error: {e}");
        return Ok(());
      }
      let bytecode = Rc::new(read_program(
        &mut std::io::Cursor::new(&mut buf),
      )?);
      run_coro(Vm::new(
        bytecode,
        Box::new(()),
        args.debug_output,
      ));
    }
    _ => {
      println!("Please specify -c, -r, -t or -R as an argument")
    }
  }
  Ok(())
}
