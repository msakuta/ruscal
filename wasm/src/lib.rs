use std::{any::Any, rc::Rc};

use ruscal::{
  ast::TypeDecl,
  bytecode::NativeFn,
  file_io::{parse_program, read_program, write_program},
  type_checker::{self, TypeCheckContext},
  value::Value,
  vm::{debugger, Vm, YieldResult},
  Args,
};
use wasm_bindgen::prelude::*;

#[wasm_bindgen(module = "/wasm_api.js")]
extern "C" {
  pub(crate) fn wasm_print(s: &str);
  pub(crate) fn wasm_rectangle(
    x0: f64,
    y0: f64,
    x1: f64,
    y1: f64,
  );
  pub(crate) fn wasm_set_fill_style(s: &str);
}

fn print_fn(_: &dyn Any, args: &[Value]) -> Value {
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

fn dbg_fn(_: &dyn Any, values: &[Value]) -> Value {
  wasm_print(&format!("dbg: {:?}\n", values[0]));
  Value::I64(0)
}

fn puts_fn(_: &dyn Any, args: &[Value]) -> Value {
  for arg in args {
    wasm_print(&format!("{}", arg));
  }
  Value::F64(0.)
}

fn rectangle_fn(_: &dyn Any, args: &[Value]) -> Value {
  let mut f64vals = args.iter().take(4).map(|val| {
    if let Ok(v) = val.coerce_f64() {
      Ok(v)
    } else {
      Err("wrong type!".to_string())
    }
  });
  let short = "Input needs to be more than 4 values";
  let x0 = f64vals.next().expect(short).unwrap();
  let y0 = f64vals.next().expect(short).unwrap();
  let x1 = f64vals.next().expect(short).unwrap();
  let y1 = f64vals.next().expect(short).unwrap();
  wasm_rectangle(x0, y0, x1, y1);
  Value::I64(0)
}

fn set_fill_style_fn(_: &dyn Any, vals: &[Value]) -> Value {
  if let [Value::Str(s), ..] = vals {
    wasm_set_fill_style(s);
  }
  Value::I64(0)
}

fn wasm_functions<'src>(
  mut set_fn: impl FnMut(
    &'static str,
    Box<dyn Fn() -> NativeFn<'src>>,
  ),
) {
  set_fn(
    "print",
    Box::new(|| {
      NativeFn::new(
        vec![("arg", TypeDecl::Any)],
        TypeDecl::Any,
        Box::new(print_fn),
      )
    }),
  );
  set_fn(
    "dbg",
    Box::new(|| {
      NativeFn::new(
        vec![("arg", TypeDecl::Any)],
        TypeDecl::Any,
        Box::new(dbg_fn),
      )
    }),
  );
  set_fn(
    "puts",
    Box::new(|| {
      NativeFn::new(
        vec![("arg", TypeDecl::Any)],
        TypeDecl::Any,
        Box::new(puts_fn),
      )
    }),
  );
  set_fn(
    "rectangle",
    Box::new(|| {
      NativeFn::new(
        vec![
          ("x0", TypeDecl::F64),
          ("y0", TypeDecl::F64),
          ("x1", TypeDecl::F64),
          ("y1", TypeDecl::F64),
        ],
        TypeDecl::Any,
        Box::new(rectangle_fn),
      )
    }),
  );
  set_fn(
    "set_fill_style",
    Box::new(|| {
      NativeFn::new(
        vec![("s", TypeDecl::Str)],
        TypeDecl::Any,
        Box::new(set_fill_style_fn),
      )
    }),
  );
}

#[wasm_bindgen]
pub fn type_check(src: &str) -> Result<JsValue, JsValue> {
  let stmts = parse_program("<input>", src).map_err(|e| {
    JsValue::from_str(&format!("Parse Error: {e}"))
  })?;

  let mut tc_ctx = TypeCheckContext::new();

  wasm_functions(|fname, f| {
    tc_ctx.add_fn(fname.to_string(), f())
  });

  type_checker::type_check(&stmts, &mut tc_ctx).map_err(
    |e| JsValue::from_str(&format!("Type Check Error: {e}")),
  )?;

  Ok(JsValue::from_str("Ok"))
}

#[wasm_bindgen]
pub fn compile(src: &str) -> Result<Vec<u8>, JsValue> {
  let mut args = Args::new();

  wasm_functions(|fname, f| {
    args
      .additional_funcs
      .insert(fname.to_string(), Box::new(move || f()));
  });

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

  Ok(buf)
}

#[wasm_bindgen]
pub fn disasm(src: &str) -> Result<String, JsValue> {
  let mut args = Args::new();

  wasm_functions(|fname, f| {
    args
      .additional_funcs
      .insert(fname.to_string(), Box::new(move || f()));
  });

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

  let mut dis_buf = vec![];
  bytecode
    .disasm(&mut dis_buf)
    .map_err(|e| JsValue::from(e.to_string()))?;

  Ok(
    String::from_utf8(dis_buf)
      .map_err(|e| JsValue::from(e.to_string()))?,
  )
}

#[wasm_bindgen]
pub fn compile_and_run(src: &str) -> Result<(), JsValue> {
  let mut args = Args::new();

  wasm_functions(|fname, f| {
    args
      .additional_funcs
      .insert(fname.to_string(), Box::new(move || f()));
  });

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
    bytecode.add_fn(name.to_string(), f())
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
