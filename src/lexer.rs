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
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Number(i32),
    Plus,
    Let,
    Print,
    Assignment,
    Identifier(String),
    SemiColon,
    Eof,
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Token> = {
        let mut map = HashMap::new();
        map.insert("let", Token::Let);
        map.insert("print", Token::Print);
        map
    };
}

/// Lexer for the simple Pyro programming language.
///
/// The `Lexer` struct provides a basic lexer that will generate
/// a stream of tokens from an input source string.
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    eof: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
            eof: false,
        }
    }

    /// Consume input until a non-whitespace
    /// character is next in the sequence.
    fn consume_whitespace(&mut self) {
        while let Some(c) = self.input.peek() {
            if !c.is_whitespace() {
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
    fn scan_number(&mut self) -> i32 {
        let mut number = String::new();
        while let Some(&c) = self.input.peek() {
            if c.is_digit(10) {
                number.push(c);
                self.input.next();
            } else {
                break;
            }
        }
        number.parse().unwrap()
    }

    /// Scan input for a valid identifier value.
    /// 
    /// Will continue scanning until a non-alphanumeric value is
    /// encountered in the sequence and return the string 
    /// represented by the consumed input.
    fn scan_identifier(&mut self) -> String {
        let mut identifier = String::new();
        while let Some(&c) = self.input.peek() {
            if c.is_alphanumeric() {
                identifier.push(c);
                self.input.next();
            } else {
                break;
            }
        }
        identifier
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
    pub fn next_token(&mut self) -> Option<Token> {
        self.consume_whitespace();
        while let Some(&c) = self.input.peek() {
            match c {
                '0'..='9' => {
                    let number = self.scan_number();
                    return Some(Token::Number(number));
                }
                '+' => {
                    self.input.next();
                    return Some(Token::Plus);
                }
                '=' => {
                    self.input.next();
                    return Some(Token::Assignment);
                }
                ';' => {
                    self.input.next();
                    return Some(Token::SemiColon);
                }
                'a'..='z' | 'A'..='Z' => {
                    let identifier = self.scan_identifier();
                    if let Some(token) = KEYWORDS.get(&identifier[..]) {
                        return Some(token.clone());
                    } else {
                        return Some(Token::Identifier(identifier));
                    }
                }
                _ => {
                    panic!("Unexpected character: {}", c);
                }
            }
        }
        if self.eof {
            return None;
        }
        self.eof = true;
        Some(Token::Eof)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    /// Advances the lexer and returns the next token.
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}
