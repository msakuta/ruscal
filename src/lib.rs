use std::sync::atomic::AtomicBool;

pub enum RunMode {
  None,
  Compile,
  Run(String),
}

pub struct Args {
  pub run_mode: RunMode,
  pub source: Option<String>,
  pub output: String,
  pub disasm: bool,
}

pub static DEBUG: AtomicBool = AtomicBool::new(false);

pub fn parse_args() -> Option<Args> {
  let mut run_mode = RunMode::None;
  let mut source = None;
  let mut output = None;
  let mut disasm = false;
  let mut show_help = false;
  let mut args_is_empty = true;

  let mut args = std::env::args();
  let exe = args.next();
  let mut next_arg = args.next();
  while let Some(arg) = next_arg {
    match &arg as &str {
      "-h" => show_help = true,
      "-c" => run_mode = RunMode::Compile,
      "-o" => output = args.next(),
      "-r" => {
        let bytecode = args
          .next()
          .unwrap_or_else(|| "bytecode.bin".to_string());
        run_mode = RunMode::Run(bytecode);
      }
      "-d" => disasm = true,
      "-D" => {
        DEBUG.store(true, std::sync::atomic::Ordering::Relaxed)
      }
      _ => {
        if source.is_none() {
          source = Some(arg);
        } else {
          println!("More than 1 file name is specified");
          return None;
        }
      }
    }
    args_is_empty = false;
    next_arg = args.next();
  }

  if show_help || args_is_empty {
    println!(
      r#"Usage: {} [options] [source.txt]

Options:
    -c       Compile source file to a bytecode
    -o file  Specify output file
    -r       Run bytecode
    -d       Disassemble compiled code
    -h       Display help"#,
      exe.unwrap_or_else(|| "29-full-stmt".to_string())
    );
    return None;
  }

  Some(Args {
    run_mode,
    source,
    output: output
      .unwrap_or_else(|| "bytecode.bin".to_string()),
    disasm,
  })
}

#[macro_export]
macro_rules! dprintln {
    ($fmt:literal) => {
        if ::rusty_programmer::DEBUG.load(std::sync::atomic::Ordering::Relaxed) {
            println!($fmt);
        }
    };
    ($fmt:literal, $($args:expr),*) => {
        if ::rusty_programmer::DEBUG.load(std::sync::atomic::Ordering::Relaxed) {
            println!($fmt, $($args),*);
        }
    };
}
