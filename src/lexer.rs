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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    // Keywords
    Func,
    Let,
    Number,
    Return,

    // Identifiers and literals
    Identifier(String),
    NumberLiteral(i64),

    // Operators
    Plus,
    Assignment,

    // Delimiters
    SemiColon,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Comma,
    GreaterThan,

    Eof,

    NoToken,
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Token> = {
        let mut map = HashMap::new();
        map.insert("let", Token::Let);
        map.insert("func", Token::Func);
        map.insert("number", Token::Number);
        map.insert("return", Token::Return);
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
    fn scan_number(&mut self) -> Result<i64, String> {
        let mut number = String::new();
        while let Some(&c) = self.input.peek() {
            if c.is_digit(10) {
                number.push(c);
                self.input.next();
            } else if c.is_alphabetic() {
                return Err(format!("Invalid number literal: {:?}{:?}", number, c));
            } else {
                break;
            }
        }
        Ok(number.parse().unwrap())
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
    pub fn next_token(&mut self) -> Result<Token, String> {
        self.consume_whitespace();
        while let Some(&c) = self.input.peek() {
            let return_token;
            match c {
                '0'..='9' => {
                    let number = self.scan_number()?;
                    return_token = Token::NumberLiteral(number);
                }
                'a'..='z' | 'A'..='Z' => {
                    let identifier = self.scan_identifier();
                    if let Some(token) = KEYWORDS.get(&identifier[..]) {
                        return_token = token.clone();
                    } else {
                        return_token = Token::Identifier(identifier);
                    }
                }
                '+' => {
                    self.input.next();
                    return_token = Token::Plus;
                }
                '=' => {
                    self.input.next();
                    return_token = Token::Assignment;
                }
                '(' => {
                    self.input.next();
                    return_token = Token::OpenParen;
                }
                ')' => {
                    self.input.next();
                    return_token = Token::CloseParen;
                }
                '{' => {
                    self.input.next();
                    return_token = Token::OpenBrace;
                }
                ',' => {
                    self.input.next();
                    return_token = Token::Comma;
                }
                '}' => {
                    self.input.next();
                    return_token = Token::CloseBrace;
                }
                ';' => {
                    self.input.next();
                    return_token = Token::SemiColon;
                }
                '>' => {
                    self.input.next();
                    return_token = Token::GreaterThan;
                }
                _ => {
                    return Err(format!("Unexpected character: {}", c));
                }
            }
            return Ok(return_token);
        }

        Ok(Token::Eof)
    }
}

/// Iterator implementation for Lexer.
/// Returns tokens in sequence until no more tokens can be found.
impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, String>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.eof {
            return None;
        }
        let token = self.next_token();
        match token {
            Ok(Token::Eof) => {
                self.eof = true;
                Some(token)
            }
            _ => Some(token),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lexer_from_str(input: &str) -> Lexer {
        Lexer::new(input)
    }

    /// Run tests for all the individual tokens
    #[test]
    fn test_let_token() {
        let mut lexer = lexer_from_str("let");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::Let);
    }

    #[test]
    fn test_func_token() {
        let mut lexer = lexer_from_str("func");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::Func);
    }

    #[test]
    fn test_identifier_token() {
        let mut lexer = lexer_from_str("foo");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::Identifier("foo".to_string()));
    }

    #[test]
    fn test_number_token() {
        let mut lexer = lexer_from_str("number");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::Number);
    }

    #[test]
    fn test_number_literal_token() {
        let mut lexer = lexer_from_str("42");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::NumberLiteral(42));
    }
    #[test]
    fn test_plus_token() {
        let mut lexer = lexer_from_str("+");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::Plus);
    }

    #[test]
    fn test_assignment_token() {
        let mut lexer = lexer_from_str("=");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::Assignment);
    }

    #[test]
    fn test_open_paren_token() {
        let mut lexer = lexer_from_str("(");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::OpenParen);
    }

    #[test]
    fn test_close_paren_token() {
        let mut lexer = lexer_from_str(")");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::CloseParen);
    }

    #[test]
    fn test_open_brace_token() {
        let mut lexer = lexer_from_str("{");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::OpenBrace);
    }

    #[test]
    fn test_close_brace_token() {
        let mut lexer = lexer_from_str("}");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::CloseBrace);
    }

    #[test]
    fn test_semi_colon_token() {
        let mut lexer = lexer_from_str(";");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::SemiColon);
    }

    #[test]
    fn test_comma_token() {
        let mut lexer = lexer_from_str(",");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::Comma);
    }

    #[test]
    fn test_greater_than_token() {
        let mut lexer = lexer_from_str(">");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::GreaterThan);
    }

    #[test]
    fn test_return_token() {
        let mut lexer = lexer_from_str("return");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::Return);
    }

    #[test]
    fn test_eof_token() {
        let mut lexer = lexer_from_str("");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::Eof);
    }

    #[test]
    fn test_multiple_tokens() {
        let lexer = lexer_from_str("let foo = 42;");
        let tokens: Result<Vec<Token>, String> = lexer.into_iter().collect();

        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Identifier("foo".to_string()),
                Token::Assignment,
                Token::NumberLiteral(42),
                Token::SemiColon,
                Token::Eof
            ]
        );
    }

    #[test]
    fn test_invalid_number_literal() {
        let mut lexer = lexer_from_str("4test32;");
        let token = lexer.next_token();
        assert!(token.is_err());
    }
}
