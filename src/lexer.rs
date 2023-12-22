//! This module provides a lexer for the Pyro programming language.
//!
//! It is a simple state based lexer which scans through the input string
//! and produces tokens from it.
//!
//! # Example usage:
//!
//! ```rust
//! use lexer::Lexer;
//!
//! let input = "let x = 42 + y;".to_string();
//! let lexer = Lexer::new(input);
//!
//! for token in lexer {
//!     println!("{:?}", token);
//! }
//! ```
extern crate lazy_static;

use std::collections::HashMap;
use std::iter::Peekable;
use std::str::Chars;

use lazy_static::lazy_static;

use crate::error::CompilerError;
use crate::error::CompilerErrorType::LexerError;

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum TokenType {
    // Keywords
    Func,
    Let,
    Boolean,
    Integer,
    Float,
    If,
    From,
    To,
    Else,
    String,
    Return,
    Destroy,
    Create,
    Class,
    For,
    While,

    // Identifiers and literals
    SelfIdentifier,
    Identifier(String),
    IntegerLiteral(i64),
    StringLiteral(String),
    FloatLiteral(f64),
    BooleanLiteral(bool),

    // Operators
    Plus,
    Minus,
    Asterisk,
    EqualSign,
    Slash,

    // Delimiters
    SemiColon,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Comma,
    Dot,
    GreaterThan,
    LessThan,
    EqualTo,

    Eof,

    NoToken,
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut map = HashMap::new();
        map.insert("let", TokenType::Let);
        map.insert("func", TokenType::Func);
        map.insert("integer", TokenType::Integer);
        map.insert("float", TokenType::Float);
        map.insert("return", TokenType::Return);
        map.insert("string", TokenType::String);
        map.insert("boolean", TokenType::Boolean);
        map.insert("destroy", TokenType::Destroy);
        map.insert("create", TokenType::Create);
        map.insert("if", TokenType::If);
        map.insert("from", TokenType::From);
        map.insert("to", TokenType::To);
        map.insert("else", TokenType::Else);
        map.insert("for", TokenType::For);
        map.insert("while", TokenType::While);
        map.insert("true", TokenType::BooleanLiteral(true));
        map.insert("false", TokenType::BooleanLiteral(false));
        map.insert("class", TokenType::Class);
        map.insert("self", TokenType::SelfIdentifier);
        map
    };
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Token {
    pub token_type: TokenType,
    pub line_number: u64,
}

impl Token {
    pub fn new(token_type: TokenType, line_number: u64) -> Self {
        Self {
            token_type,
            line_number,
        }
    }
}

impl PartialEq<TokenType> for Token {
    fn eq(&self, other: &TokenType) -> bool {
        self.token_type == *other
    }
}

impl PartialEq<Token> for TokenType {
    fn eq(&self, other: &Token) -> bool {
        *self == other.token_type
    }
}

/// Lexer for the simple Pyro programming language.
///
/// The `Lexer` struct provides a basic lexer that will generate
/// a stream of tokens from an input source string.
pub(crate) struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    current_line: u64,
    eof: bool,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
            current_line: 1,
            eof: false,
        }
    }

    /// Consume input until a non-whitespace
    /// character is next in the sequence.
    fn consume_whitespace(&mut self) {
        while let Some(c) = self.input.peek() {
            if c == &'\n' {
                self.current_line += 1;
            } else if !c.is_whitespace() {
                break;
            }
            self.input.next();
        }
    }

    /// Consume input until a specified character is encountered.
    /// If the target character is not encountered, the entire
    /// input sequence will be consumed.
    fn consume_until(&mut self, target: char) {
        while let Some(c) = self.input.peek() {
            if *c == target {
                break;
            }
            self.input.next();
        }
    }

    /// Scan input for a numeric literal value.
    ///
    /// Will continue scanning until a non-decimal number value is
    /// encountered in the sequence and return the number
    /// represented by the consumed input.
    ///
    /// Returns the number as a TokenType.
    fn scan_number_type(&mut self) -> Result<TokenType, CompilerError> {
        let mut number = String::new();
        let mut is_float = false;
        while let Some(&c) = self.input.peek() {
            if c.is_digit(10) {
                number.push(c);
                self.input.next();
            } else if c == '.' {
                if is_float {
                    return Err(CompilerError::new(
                        LexerError,
                        self.current_line,
                        "Multiple decimal points in number literal",
                    ));
                }
                is_float = true;
                number.push(c);
                self.input.next();
            } else if c.is_alphabetic() {
                return Err(CompilerError::new(
                    LexerError,
                    self.current_line,
                    format!("Invalid number literal: {:?}{:?}", number, c),
                ));
            } else {
                break;
            }
        }

        if is_float {
            let number = number.parse::<f64>();
            if number.is_err() {
                return Err(CompilerError::new(
                    LexerError,
                    self.current_line,
                    "Error when scanning float number literal",
                ));
            }
            Ok(TokenType::FloatLiteral(number.unwrap()))
        } else {
            let number = number.parse::<i64>();
            if number.is_err() {
                return Err(CompilerError::new(
                    LexerError,
                    self.current_line,
                    "Error when scanning integer literal",
                ));
            }
            Ok(TokenType::IntegerLiteral(number.unwrap()))
        }
    }

    /// Scan input for a valid identifier value.
    ///
    /// Will continue scanning until a non-alphanumeric value is
    /// encountered in the sequence and return the string
    /// represented by the consumed input.
    fn scan_identifier(&mut self) -> String {
        let mut identifier = String::new();
        while let Some(&c) = self.input.peek() {
            if c.is_alphanumeric() || c == '_' {
                identifier.push(c);
                self.input.next();
            } else {
                break;
            }
        }
        identifier
    }

    /// Scan the input for a String delimited by '"' characters.
    fn scan_string(&mut self) -> Result<String, CompilerError> {
        let mut string = String::new();
        self.input.next();
        while let Some(&c) = self.input.peek() {
            if c == '"' {
                self.input.next();
                break;
            } else if c == '\\' {
                self.input.next();
                string.push(match self.input.next() {
                    Some('\\') => '\\',
                    Some('"') => '"',
                    Some('n') => '\n',
                    Some('t') => '\t',
                    Some('r') => '\r',
                    Some('0') => '\0',
                    Some(c) => {
                        return Err(CompilerError::new(
                            LexerError,
                            self.current_line,
                            format!("Invalid escape sequence: \\{}", c),
                        ));
                    }
                    None => {
                        return Err(CompilerError::new(
                            LexerError,
                            self.current_line,
                            "Invalid escape sequence: \\",
                        ));
                    }
                });
            } else {
                string.push(c);
                self.input.next();
            }
        }
        Ok(string)
    }

    /// Get the next `Token` in the input sequence.
    ///
    /// This function will consume characters from the input sequence
    /// and return the next `Some<Token>` that is encountered.
    ///
    /// If there are no more tokens available in the sequence, the function
    /// will instead return None.
    ///
    /// # Panics
    /// This function will panic if an unexpected character is encountered in
    /// the sequence.
    pub(crate) fn next_token(&mut self) -> Result<Token, CompilerError> {
        self.consume_whitespace();
        while let Some(&c) = self.input.peek() {
            let return_token;
            match c {
                '#' => {
                    self.consume_until('\n');
                    self.consume_whitespace();
                    self.current_line += 1;
                    continue;
                }
                '0'..='9' => {
                    return_token = self.scan_number_type()?;
                }
                'a'..='z' | 'A'..='Z' => {
                    let identifier = self.scan_identifier();
                    if let Some(token_type) = KEYWORDS.get(&identifier[..]) {
                        return_token = token_type.clone();
                    } else {
                        return_token = TokenType::Identifier(identifier);
                    }
                }
                '"' => {
                    let string = self.scan_string()?;
                    return_token = TokenType::StringLiteral(string);
                }
                '+' => {
                    self.input.next();
                    return_token = TokenType::Plus;
                }
                '-' => {
                    self.input.next();
                    return_token = TokenType::Minus;
                }
                '*' => {
                    self.input.next();
                    return_token = TokenType::Asterisk;
                }
                '/' => {
                    self.input.next();
                    return_token = TokenType::Slash;
                }
                '=' => {
                    self.input.next();
                    if self.input.peek() == Some(&'=') {
                        self.input.next();
                        return_token = TokenType::EqualTo;
                    } else {
                        return_token = TokenType::EqualSign;
                    }
                }
                '(' => {
                    self.input.next();
                    return_token = TokenType::OpenParen;
                }
                '[' => {
                    self.input.next();
                    return_token = TokenType::OpenBracket;
                }
                ']' => {
                    self.input.next();
                    return_token = TokenType::CloseBracket;
                }
                ')' => {
                    self.input.next();
                    return_token = TokenType::CloseParen;
                }
                '{' => {
                    self.input.next();
                    return_token = TokenType::OpenBrace;
                }
                ',' => {
                    self.input.next();
                    return_token = TokenType::Comma;
                }
                '}' => {
                    self.input.next();
                    return_token = TokenType::CloseBrace;
                }
                ';' => {
                    self.input.next();
                    return_token = TokenType::SemiColon;
                }
                '>' => {
                    self.input.next();
                    return_token = TokenType::GreaterThan;
                }
                '<' => {
                    self.input.next();
                    return_token = TokenType::LessThan;
                }
                '.' => {
                    self.input.next();
                    return_token = TokenType::Dot;
                }
                _ => {
                    return Err(CompilerError::new(
                        LexerError,
                        self.current_line,
                        format!("Unexpected character: {}", c),
                    ));
                }
            }
            return Ok(Token::new(return_token, self.current_line));
        }

        Ok(Token::new(TokenType::Eof, self.current_line))
    }
}

/// Iterator implementation for Lexer.
/// Returns tokens in sequence until no more tokens can be found.
impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, CompilerError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.eof {
            return None;
        }
        let token = self.next_token();
        if let Ok(ref some_token) = token {
            if some_token.token_type == TokenType::Eof {
                self.eof = true;
            }
        }

        Some(token)
    }
}
