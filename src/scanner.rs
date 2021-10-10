#[derive(Debug)]
pub struct Scanner<'a> {
    start: &'a str,
    current: &'a str,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Scanner<'a> {
        Scanner {
            start: input,
            current: input,
            line: 1,
        }
    }

    pub fn token(&mut self) -> Token<'a> {
        use Kind::*;

        self.skip_whitespace();
        self.start = self.current;

        if self.is_empty() {
            return self.make(Eof);
        }

        let c = self.advance();

        match c {
            // Single-character tokens
            Some('(') => self.make(LeftParen),
            Some(')') => self.make(RightParen),
            Some('{') => self.make(LeftBrace),
            Some('}') => self.make(RightBrace),
            Some(';') => self.make(Semicolon),
            Some(',') => self.make(Comma),
            Some('.') => self.make(Dot),
            Some('-') => self.make(Minus),
            Some('+') => self.make(Plus),
            Some('/') => self.make(Slash), // comments in skip_whitespace above
            Some('*') => self.make(Star),

            // One or two character tokens.
            Some('!') if self.matches('=') => self.make(BangEqual),
            Some('!') => self.make(Bang),

            Some('=') if self.matches('=') => self.make(EqualEqual),
            Some('=') => self.make(Equal),

            Some('<') if self.matches('=') => self.make(LessEqual),
            Some('<') => self.make(Less),

            Some('>') if self.matches('=') => self.make(GreaterEqual),
            Some('>') => self.make(Greater),

            // Literals
            Some('"') => self.string(),
            Some(c) if is_alpha(c) => self.identifier(),
            Some(c) if c.is_digit(10) => self.number(),

            // Anything else is unexpected.
            _ => self.error("Unexpected character."),
        }
    }

    fn is_empty(&self) -> bool {
        self.current.is_empty()
    }

    fn peek(&self) -> Option<char> {
        // We can't rely on returning a null byte safely at the end of the
        // string, we could have a null byte in the middle (even if it's against
        // the grammar), so we need to use `Option` here.
        self.current.chars().next()
    }

    fn peek_next(&self) -> Option<char> {
        self.current.chars().nth(1)
    }

    pub fn advance(&mut self) -> Option<char> {
        let mut chars = self.current.chars();
        let c = chars.next();
        self.current = chars.as_str();
        c
    }

    /// If the next character is `c`, it advances past it and returns `true`.
    /// Returns `false` if the scanner is empty too.
    pub fn matches(&mut self, wanted: char) -> bool {
        match self.current.chars().next() {
            Some(c) if c == wanted => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.is_empty() {
            match self.peek() {
                Some('\n') => {
                    self.line += 1;
                    self.advance();
                }

                Some('/') if self.peek_next() == Some('/') => {
                    // comments go to the end of the line.
                    while self.peek() != Some('\n') && self.peek() != None {
                        // This loop breaks when the _peeked_ character is EOF
                        // of '\n'. It's the outer loop catches the '\n' and
                        // updates `self.line` when it needs to.
                        self.advance();
                    }
                }

                Some(c) if c.is_whitespace() => {
                    self.advance();
                }

                _ => return,
            }
        }
    }

    fn string(&mut self) -> Token<'a> {
        while self.peek() != Some('"') && self.peek() != None {
            if self.peek() == Some('\n') {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_empty() {
            self.error("Unterminated string.")
        } else {
            self.advance(); // closing quote
            self.make(Kind::String)
        }
    }

    fn identifier(&mut self) -> Token<'a> {
        while let Some(c) = self.peek() {
            if is_alpha(c) || c.is_digit(10) {
                self.advance();
            } else {
                break;
            }
        }

        self.make(self.identifier_kind())
    }

    fn peek_digit(&mut self) -> bool {
        if let Some(c) = self.peek() {
            c.is_digit(10)
        } else {
            false
        }
    }

    fn number(&mut self) -> Token<'a> {
        while self.peek_digit() {
            self.advance();
        }

        let next_is_dot = self.peek() == Some('.');
        let more_digits = matches!(self.peek_next(), Some(c) if c.is_digit(10));

        if next_is_dot && more_digits {
            // consume the '.'
            self.advance();

            while self.peek_digit() {
                self.advance();
            }
        }

        self.make(Kind::Number)
    }

    fn identifier_kind(&self) -> Kind {
        let length = self.start.len() - self.current.len();
        let lexeme = &self.start[..length];
        match lexeme {
            "and" => Kind::And,
            "class" => Kind::Class,
            "else" => Kind::Else,
            "false" => Kind::False,
            "for" => Kind::For,
            "fun" => Kind::Fun,
            "if" => Kind::If,
            "nil" => Kind::Nil,
            "or" => Kind::Or,
            "print" => Kind::Print,
            "return" => Kind::Return,
            "super" => Kind::Super,
            "this" => Kind::This,
            "true" => Kind::True,
            "var" => Kind::Var,
            "while" => Kind::While,
            _ => Kind::Identifier,
        }
    }

    fn error(&self, body: &'static str) -> Token<'a> {
        Token {
            kind: Kind::Error,
            lexeme: body,
            line: self.line,
        }
    }

    fn make(&self, kind: Kind) -> Token<'a> {
        let length = self.start.len() - self.current.len();
        Token {
            kind,
            lexeme: &self.start[..length],
            line: self.line,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    // using 'kind' instead of 'type' because the latter is a reserve word in
    // Rust and it'll get annoying.
    pub kind: Kind,
    // Using `lexeme` instead of `start` and `length`, since the `&str` already
    // has a length built into it.
    pub lexeme: &'a str,
    pub line: usize,
}

impl<'a> Token<'a> {
    pub const fn fake() -> Token<'a> {
        Token {
            kind: Kind::Error,
            lexeme: "<error>",
            line: 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum Kind {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    Eof,
}

// our version isn't the same as rust's is_ascii_alpha since we want '_' too.
fn is_alpha(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}
