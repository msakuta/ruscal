use std::{any::Any, rc::Rc};

use ruscal::{
  ast::TypeDecl,
  bytecode::{FnDecl, NativeFn},
  file_io::{read_program, write_program},
  value::Value,
  vm::{debugger, Vm, YieldResult},
  Args,
};
use wasm_bindgen::prelude::*;

#[wasm_bindgen(module = "/wasm_api.js")]
extern "C" {
  pub(crate) fn wasm_print(s: &str);
  // pub(crate) fn wasm_rectangle(x0: i32, y0: i32, x1: i32, y1: i32);
  // pub(crate) fn wasm_set_fill_style(s: &str);
}

fn s_print(_: &dyn Any, args: &[Value]) -> Value {
  let output = args.iter().map(|v| v.to_string()).fold(
    String::new(),
    |acc, cur| {
      if acc.is_empty() {
        cur
      } else {
        acc + ", " + &cur
      }
    },
  );
  wasm_print(&output);

  wasm_print(&format!("\n"));
  Value::I64(0)
}

fn wasm_functions<'src>(
  mut set_fn: impl FnMut(&'static str, NativeFn<'src>),
) {
  set_fn(
    "print",
    NativeFn::new(
      vec![("arg", TypeDecl::Any)],
      TypeDecl::Any,
      Box::new(s_print),
    ),
  );
}

#[wasm_bindgen]
pub fn type_check(src: &str) -> Result<JsValue, JsValue> {
  Ok(JsValue::from_str("Todo!"))
}

#[wasm_bindgen]
pub fn compile(src: &str) -> Result<Vec<u8>, JsValue> {
  let mut bytes = vec![];
  Ok(bytes)
}

#[wasm_bindgen]
pub fn disasm(src: &str) -> Result<String, JsValue> {
  Ok("Todo!".to_string())
}

#[wasm_bindgen]
pub fn compile_and_run(src: &str) -> Result<(), JsValue> {
  let args = Args::new();
  let mut buf = vec![];

  write_program(
    "<input>",
    src,
    &mut std::io::Cursor::new(&mut buf),
    "<Memory>",
    &args,
  )
  .map_err(|e| {
    JsValue::from_str(&format!("Compile Error: {e}"))
  })?;

  let mut bytecode =
    read_program(&mut std::io::Cursor::new(&mut buf))
      .map_err(|e| JsValue::from(e.to_string()))?;

  wasm_functions(|name, f| {
    bytecode.add_fn(name.to_string(), f)
  });

  let mut vm = Vm::new(Rc::new(bytecode), Box::new(()), false);

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
        if value == Value::Str("break".to_string()) {
          if debugger(&vm) {
            break;
          }
        }
      }
      Err(e) => {
        eprintln!("Runtime error: {e:?}");
        break;
      }
    }
  }

  Ok(())
}
