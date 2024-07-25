# Ruscal

<p align="center">
    <img width="128" src="images/ruscal.png" alt="Material Bread logo">
</p>

RUSty CALculator scripting language learning project

Try it now on your browser! https://msakuta.github.io/ruscal/


## Overview

This repo is the implementation of the example script used in my book:
[Rustで作るプログラミング言語](https://www.amazon.co.jp/dp/4297141922) (Japanese).

This is a sister project of [rustack](https://github.com/msakuta/rustack),
a custom programming language compiler / interpreter.

Ruscal is a stack-based scripting language specifically designed for learning creating a programming language.
As such, its target is not a feature-rich next-gen language.
It is a boring language that intends to help the reader to learn how to make his or her own language.

The process of the language design is split into steps.
Each step has an entry in [examples](examples) directory.
Each step works as an independent program.
Use `--example` switch of cargo to run them.
For example:

```
cargo r --example 00-match
```

## Features

Although its target is not a useful language, it has few notable features:

* `f64`, `i64` and `str` primitive types (which are what you would expect)
* Rust-like syntax and a parser (implemented with nom)
* Basic control flow structures by `if` and `for` statements
* Variable declarations with type annotations
* Static type checking on function arguments, return types and expressions
* Stack-based bytecode interpreter and compiler
* Coroutines and generators


## WebAssembly browser application

You can also build a wasm package and run the interpreter on the browser.

    cd wasm
    npm i
    npm run build

To launch the application, you can use `npx`

    cd dist
    npx serve

and browse http://localhost:5000.


## Scripts

There are number of example scripts in [scripts](scripts) directory, but most of them require specific step of the interpreter to run. Those target step numbers are indicated by prefixes of the file names.

The standard script file extension is `.rscl`, because there is already a language called Rascal and they took `.rsc`.

For example, this is a Mandelbrot set in ASCII art, rendered with [31-mandel.rscl](scripts/31-mandel.rscl):

```raw
******************************************************************************
******************************************************************************
******************************************************************************
******************************************************************************
******************************************************************************
******************************************************************************
******************************************************************************
******************************************************************************
******************************************************************************
*******************************************.+*********************************
******************************************++.+..******************************
*****************************************+++...++*****************************
**************************************++++.    .++****************************
**********************************+++++++..    ..+++++++**********************
********************************+++.  ..          ......+*********************
******************************++++...                 .++*********************
********************+++++++++++++..                   ..++********************
*******************+++........+...                     .++********************
******************+++...       ..                      ..+********************
*************+++++....                                 .+*********************
*******                                              .+++*********************
*************+++++....                                 .+*********************
******************+++...       ..                      ..+********************
*******************+++........+...                     .++********************
********************+++++++++++++..                   ..++********************
******************************++++...                 .++*********************
********************************+++.  ..          ......+*********************
**********************************+++++++..    ..+++++++**********************
**************************************++++.    .++****************************
*****************************************+++...++*****************************
******************************************++.+..******************************
*******************************************.+*********************************
******************************************************************************
******************************************************************************
******************************************************************************
******************************************************************************
******************************************************************************
******************************************************************************
******************************************************************************
******************************************************************************
```
