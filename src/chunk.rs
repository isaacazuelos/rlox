use std::convert::{TryFrom, TryInto};

use crate::{function::ObjFunction, value::Value};

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
#[non_exhaustive]
pub enum Opcode {
    Return,
    Constant,
    Nil,
    True,
    False,
    Negate,
    Add,
    Sub,
    Mul,
    Div,
    Not,
    Equal,
    Greater,
    Less,
    Print,
    Pop,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    JumpIfFalse,
    Jump,
    Loop,
    Call,
    Closure,
    GetUpvalue,
    SetUpvalue,
    CloseUpvalue,
    Class,
    GetProperty,
    SetProperty,
    Method,
    Invoke,
    Inherit,
    GetSuper,
    SuperInvoke,
}

impl Opcode {
    pub fn to_str(self) -> &'static str {
        match self {
            Opcode::Return => "Return",
            Opcode::Constant => "Constant",
            Opcode::Nil => "Nil",
            Opcode::True => "True",
            Opcode::False => "False",
            Opcode::Negate => "Negate",
            Opcode::Add => "Add",
            Opcode::Sub => "Sub",
            Opcode::Mul => "Mul",
            Opcode::Div => "Div",
            Opcode::Not => "Not",
            Opcode::Equal => "Equal",
            Opcode::Greater => "Greater",
            Opcode::Less => "Less",
            Opcode::Print => "Print",
            Opcode::Pop => "Pop",
            Opcode::DefineGlobal => "DefineGlobal",
            Opcode::GetGlobal => "GetGlobal",
            Opcode::SetGlobal => "SetGlobal",
            Opcode::GetLocal => "GetLocal",
            Opcode::SetLocal => "SetLocal",
            Opcode::JumpIfFalse => "JumpIfFalse",
            Opcode::Jump => "Jump",
            Opcode::Loop => "Loop",
            Opcode::Call => "Call",
            Opcode::Closure => "Closure",
            Opcode::GetUpvalue => "GetUpvalue",
            Opcode::SetUpvalue => "SetUpvalue",
            Opcode::CloseUpvalue => "CloseUpvalue",
            Opcode::Class => "Class",
            Opcode::GetProperty => "GetProperty",
            Opcode::SetProperty => "SetProperty",
            Opcode::Method => "Method",
            Opcode::Invoke => "Invoke",
            Opcode::Inherit => "Inherit",
            Opcode::GetSuper => "GetSuper",
            Opcode::SuperInvoke => "SuperInvoke",
        }
    }
}

impl From<Opcode> for u8 {
    fn from(op: Opcode) -> u8 {
        op as _
    }
}

impl TryFrom<u8> for Opcode {
    type Error = u8;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Opcode::Return),
            1 => Ok(Opcode::Constant),
            2 => Ok(Opcode::Nil),
            3 => Ok(Opcode::True),
            4 => Ok(Opcode::False),
            5 => Ok(Opcode::Negate),
            6 => Ok(Opcode::Add),
            7 => Ok(Opcode::Sub),
            8 => Ok(Opcode::Mul),
            9 => Ok(Opcode::Div),
            10 => Ok(Opcode::Not),
            11 => Ok(Opcode::Equal),
            12 => Ok(Opcode::Greater),
            13 => Ok(Opcode::Less),
            14 => Ok(Opcode::Print),
            15 => Ok(Opcode::Pop),
            16 => Ok(Opcode::DefineGlobal),
            17 => Ok(Opcode::GetGlobal),
            18 => Ok(Opcode::SetGlobal),
            19 => Ok(Opcode::GetLocal),
            20 => Ok(Opcode::SetLocal),
            21 => Ok(Opcode::JumpIfFalse),
            22 => Ok(Opcode::Jump),
            23 => Ok(Opcode::Loop),
            24 => Ok(Opcode::Call),
            25 => Ok(Opcode::Closure),
            26 => Ok(Opcode::GetUpvalue),
            27 => Ok(Opcode::SetUpvalue),
            28 => Ok(Opcode::CloseUpvalue),
            29 => Ok(Opcode::Class),
            30 => Ok(Opcode::GetProperty),
            31 => Ok(Opcode::SetProperty),
            32 => Ok(Opcode::Method),
            33 => Ok(Opcode::Invoke),
            34 => Ok(Opcode::Inherit),
            35 => Ok(Opcode::GetSuper),
            36 => Ok(Opcode::SuperInvoke),
            n => Err(n),
        }
    }
}

#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub lines: Vec<usize>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            lines: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn write(&mut self, byte: impl Into<u8>, line: usize) {
        self.code.push(byte.into());
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }
}

// Debug helpers
impl Chunk {
    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);

        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset);
        }
    }

    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04}", offset);

        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:>4} ", self.lines[offset]);
        }

        let opcode: Result<Opcode, _> = self.code[offset].try_into();

        match opcode {
            Ok(
                op
                @
                (Opcode::Constant
                | Opcode::DefineGlobal
                | Opcode::GetGlobal
                | Opcode::SetGlobal
                | Opcode::Class
                | Opcode::Method
                | Opcode::GetSuper),
            ) => self.constant_instruction(op, offset),
            Ok(
                op
                @
                (Opcode::GetLocal
                | Opcode::SetLocal
                | Opcode::GetUpvalue
                | Opcode::SetUpvalue
                | Opcode::Call),
            ) => self.byte_instruction(op, offset),
            Ok(op @ (Opcode::Jump | Opcode::JumpIfFalse)) => {
                self.jump_instruction(op, 1, offset)
            }
            Ok(op @ Opcode::Loop) => self.jump_instruction(op, -1, offset),
            Ok(op @ Opcode::Closure) => {
                let constant = self.code[offset + 1] as usize;
                println!(
                    "{:<16} {:4} {}",
                    op.to_str(),
                    constant,
                    self.constants[constant]
                );

                let function =
                    self.constants[constant].as_a::<ObjFunction>().unwrap();
                for _ in 0..function.upvalue_count {
                    let is_local = if self.code[offset + 2] == true as _ {
                        "local"
                    } else {
                        "upvalue"
                    };
                    let index = self.code[offset + 3];
                    println!(
                        "{:04}   |                     {} {}",
                        offset + 1,
                        is_local,
                        index
                    );
                }

                offset + 4
            }
            Ok(op @ (Opcode::Invoke | Opcode::SuperInvoke)) => {
                self.invoke_instruction(op, offset)
            }
            Ok(op) => self.simple_instruction(op, offset),
            Err(n) => {
                println!("Unknown opcode {}", n);
                offset + 1
            }
        }
    }

    fn simple_instruction(&self, op: Opcode, offset: usize) -> usize {
        println!("{}", op.to_str());
        offset + 1
    }

    fn byte_instruction(&self, op: Opcode, offset: usize) -> usize {
        let slot = self.code[offset + 1];
        println!("{:<16} {:4}", op.to_str(), slot);
        offset + 2
    }

    fn constant_instruction(&self, op: Opcode, offset: usize) -> usize {
        let constant = self.code[offset + 1];
        println!(
            "{:<16} {:4} '{}'",
            op.to_str(),
            constant,
            self.constants[constant as usize]
        );
        offset + 2
    }

    fn jump_instruction(
        &self,
        op: Opcode,
        sign: isize,
        offset: usize,
    ) -> usize {
        let jump = {
            let first = self.code[offset + 1];
            let second = self.code[offset + 2];
            i16::from_ne_bytes([first, second])
        };

        println!(
            "{:<16} {:4} -> {}",
            op.to_str(),
            offset,
            (offset as isize + 3) + (sign * jump as isize)
        );
        offset + 3
    }

    fn invoke_instruction(&self, op: Opcode, offset: usize) -> usize {
        let constant = self.code[offset + 1];
        let arg_count = self.code[offset + 2];
        println!(
            "{:<16} ({} args) {:4} '{}'",
            op.to_str(),
            arg_count,
            constant,
            self.constants[constant as usize]
        );
        offset + 3
    }
}
