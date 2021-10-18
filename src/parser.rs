use crate::{
    compiler::Context,
    scanner::{Kind, Scanner, Token},
};

#[derive(Debug)]
pub struct Parser<'a> {
    scanner: Scanner<'a>,
    pub current: Token<'a>,
    pub previous: Token<'a>,
    pub had_error: bool,
    pub panic_mode: bool,
}

impl<'a> Parser<'a> {
    pub fn new(scanner: Scanner<'a>) -> Parser<'a> {
        Parser {
            scanner,
            current: Token::fake(),
            previous: Token::fake(),
            had_error: false,
            panic_mode: false,
        }
    }

    pub fn advance(&mut self) {
        self.previous = self.current;

        loop {
            self.current = self.scanner.token();

            if self.current.kind != Kind::Error {
                break;
            }

            self.error_at_current(self.current.lexeme);
        }
    }

    pub fn consume(&mut self, kind: Kind, message: &str) {
        if self.current.kind == kind {
            self.advance();
        } else {
            self.error_at_current(message)
        }
    }

    pub fn error_at_current(&mut self, message: &str) {
        self.error_at(self.current, message)
    }

    pub fn error(&mut self, message: &str) {
        self.error_at(self.previous, message)
    }

    fn error_at(&mut self, token: Token<'a>, message: &str) {
        if self.panic_mode {
            return;
        }

        self.panic_mode = true;
        eprint!("[line {}] Error", token.line);

        match token.kind {
            Kind::Eof => eprint!(" at end"),
            Kind::Error => {}
            _ => eprint!(" at '{}'", token.lexeme),
        }

        eprintln!(": {}", message);
        self.had_error = true;
    }

    pub fn synchronize(&mut self) {
        use Kind::*;

        self.panic_mode = false;

        while self.current.kind != Kind::Eof {
            if self.previous.kind == Semicolon {
                return;
            }

            match self.current.kind {
                Class | Fun | Var | For | If | While | Print | Return => return,
                _ => self.advance(),
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum Precedence {
    None = 0,
    Assignment = 1,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    pub fn next(self) -> Precedence {
        use Precedence::*;
        match self {
            None => None,
            Assignment => Or,
            Or => And,
            And => Equality,
            Equality => Comparison,
            Comparison => Term,
            Term => Factor,
            Factor => Unary,
            Unary => Call,
            Call => Primary,
            Primary => None,
        }
    }
}

pub type ParseFn<'a, 'i, 'h> = Option<fn(&'a mut Context<'i, 'h>, bool)>;

#[derive(Clone, Copy)]
pub struct Rule<'a, 'i, 'h>(
    pub ParseFn<'a, 'i, 'h>,
    pub ParseFn<'a, 'i, 'h>,
    pub Precedence,
);

impl<'a, 'i, 'h> Rule<'a, 'i, 'h> {
    pub fn prefix(&self) -> ParseFn<'a, 'i, 'h> {
        self.0
    }

    pub fn infix(&self) -> ParseFn<'a, 'i, 'h> {
        self.1
    }

    pub fn precedence(&self) -> Precedence {
        self.2
    }

    pub fn get(kind: Kind) -> Self {
        #[rustfmt::skip]
        let rules = {
            use std::option::Option::{None as N, Some as S};
            use crate::parser::Precedence::*;
            use crate::compiler::Context as C;
            &[
                // Single-character tokens.
                Rule(S(C::group),   S(C::call),   Call),       // LeftParen,
                Rule(N,             N,            None),       // RightParen,
                Rule(N,             N,            None),       // LeftBrace,
                Rule(N,             N,            None),       // RightBrace,
                Rule(N,             N,            None),       // Comma,
                Rule(N,             S(C::dot),    Call),       // Dot,
                Rule(S(C::unary),   S(C::binary), Term),       // Minus,
                Rule(N,             S(C::binary), Term),       // Plus,
                Rule(N,             N,            None),       // Semicolon,
                Rule(N,             S(C::binary), Factor),     // Slash,
                Rule(N,             S(C::binary), Factor),     // Star,

                // One or two character tokens.
                Rule(S(C::unary),   N,            None),       // Bang,
                Rule(N,             S(C::binary), Equality),   // BangEqual,
                Rule(N,             N,            None),       // Equal,
                Rule(N,             S(C::binary), Equality),   // EqualEqual,
                Rule(N,             S(C::binary), Comparison), // Greater,
                Rule(N,             S(C::binary), Comparison), // GreaterEqual,
                Rule(N,             S(C::binary), Comparison), // Less,
                Rule(N,             S(C::binary), Comparison), // LessEqual,

                // Literals.
                Rule(S(C::variable), N, None), // Identifier,
                Rule(S(C::string),   N, None), // String,
                Rule(S(C::number),   N, None), // Number,

                // Keywords.
                Rule(N,             S(C::and), And), // And,
                Rule(N,             N, None), // Class,
                Rule(N,             N, None), // Else,
                Rule(S(C::literal), N, None), // False,
                Rule(N,             N, None), // For,
                Rule(N,             N, None), // Fun,
                Rule(N,             N, None), // If,
                Rule(S(C::literal), N, None), // Nil,
                Rule(N,             S(C::or), Or), // Or,
                Rule(N,             N, None), // Print,
                Rule(N,             N, None), // Return,
                Rule(S(C::super_),  N, None), // Super,
                Rule(S(C::this),    N, None), // This,
                Rule(S(C::literal), N, None), // True,
                Rule(N,             N, None), // Var,
                Rule(N,             N, None), // While,
                Rule(N,             N, None), // Error,
                Rule(N,             N, None), // Eof,
            ]
        };

        rules[kind as usize]
    }
}
