// #![allow(dead_code)]

mod chunk;
mod closure;
mod compiler;
mod function;
mod native;
mod object;
mod parser;
mod scanner;
mod string;
mod table;
mod upvalue;
mod value;
mod vm;

use std::{env::args, fs::read_to_string, io::Write, process::exit};

use vm::{InterpretError, VM};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let _ = match args().len() {
        1 => repl(),
        2 => run_file(&args().collect::<Vec<String>>()[1]),
        _ => {
            eprintln!("Usage: rlox [path]");
            exit(64);
        }
    };

    Ok(())
}

fn repl() -> Result<(), Box<dyn std::error::Error>> {
    let mut buf = String::new();
    let mut vm = VM::new();

    loop {
        print!("> ");
        std::io::stdout().flush()?;

        buf.clear();
        std::io::stdin().read_line(&mut buf)?;

        if buf == "\n" {
            println!("goodbye");
            return Ok(());
        }

        vm.interpret(&buf)?;
    }
}

fn run_file(path: &str) -> Result<(), Box<dyn std::error::Error>> {
    let source = read_file(path);
    let mut vm = VM::new();

    match vm.interpret(&source) {
        Err(InterpretError::Runtime) => exit(70),
        Err(InterpretError::Compile) => exit(65),
        Ok(()) => Ok(()),
    }
}

fn read_file(path: &str) -> String {
    // taking some liberties with this one already since emulating C's error
    // handing isn't exactly the goal.
    match read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error reading file: {}", e);
            exit(74);
        }
    }
}
