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
    // Keywords
    Func,
    Let,
    Boolean,
    Integer,
    Float,
    String,
    Return,
    Delete,
    Create,

    // Identifiers and literals
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
    GreaterThan,

    Eof,

    NoToken,
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Token> = {
        let mut map = HashMap::new();
        map.insert("let", Token::Let);
        map.insert("func", Token::Func);
        map.insert("integer", Token::Integer);
        map.insert("float", Token::Float);
        map.insert("return", Token::Return);
        map.insert("string", Token::String);
        map.insert("boolean", Token::Boolean);
        map.insert("delete", Token::Delete);
        map.insert("create", Token::Create);
        map.insert("true", Token::BooleanLiteral(true));
        map.insert("false", Token::BooleanLiteral(false));
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
    fn scan_number(&mut self) -> Result<Token, String> {
        let mut number = String::new();
        let mut is_float = false;
        while let Some(&c) = self.input.peek() {
            if c.is_digit(10) {
                number.push(c);
                self.input.next();
            } else if c == '.' {
                if is_float {
                    return Err(format!("Multiple decimal points in number literal"));
                }
                is_float = true;
                number.push(c);
                self.input.next();
            } else if c.is_alphabetic() {
                return Err(format!("Invalid number literal: {:?}{:?}", number, c));
            } else {
                break;
            }
        }

        if is_float {
            let number = number.parse::<f64>();
            if number.is_err() {
                return Err(format!("Error when scanning floating literal"));
            }
            Ok(Token::FloatLiteral(number.unwrap()))
        } else {
            let number = number.parse::<i64>();
            if number.is_err() {
                return Err(format!("Error when scanning integer literal"));
            }
            Ok(Token::IntegerLiteral(number.unwrap()))
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
    fn scan_string(&mut self) -> Result<String, String> {
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
                        return Err(format!("Invalid escape sequence: \\{}", c));
                    }
                    None => {
                        return Err(String::from("Invalid escape sequence: \\"));
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
    pub fn next_token(&mut self) -> Result<Token, String> {
        self.consume_whitespace();
        while let Some(&c) = self.input.peek() {
            let return_token;
            match c {
                '0'..='9' => {
                    return_token = self.scan_number()?;
                }
                'a'..='z' | 'A'..='Z' => {
                    let identifier = self.scan_identifier();
                    if let Some(token) = KEYWORDS.get(&identifier[..]) {
                        return_token = token.clone();
                    } else {
                        return_token = Token::Identifier(identifier);
                    }
                }
                '"' => {
                    let string = self.scan_string()?;
                    return_token = Token::StringLiteral(string);
                }
                '+' => {
                    self.input.next();
                    return_token = Token::Plus;
                }
                '-' => {
                    self.input.next();
                    return_token = Token::Minus;
                }
                '*' => {
                    self.input.next();
                    return_token = Token::Asterisk;
                }
                '/' => {
                    self.input.next();
                    return_token = Token::Slash;
                }
                '=' => {
                    self.input.next();
                    return_token = Token::EqualSign;
                }
                '(' => {
                    self.input.next();
                    return_token = Token::OpenParen;
                }
                '[' => {
                    self.input.next();
                    return_token = Token::OpenBracket;
                }
                ']' => {
                    self.input.next();
                    return_token = Token::CloseBracket;
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
    fn test_integer_token() {
        let mut lexer = lexer_from_str("integer");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::Integer);
    }

    #[test]
    fn test_string_literal_token() {
        let mut lexer = lexer_from_str("\"foo\"");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::StringLiteral("foo".to_string()));
    }

    #[test]
    fn test_newline_string_literal_toke() {
        let mut lexer = lexer_from_str("\"foo\\nbar\"");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::StringLiteral("foo\nbar".to_string()));
    }

    #[test]
    fn test_tab_string_literal_token() {
        let mut lexer = lexer_from_str("\"foo\\tbar\"");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::StringLiteral("foo\tbar".to_string()));
    }

    #[test]
    fn test_backslash_string_literal_token() {
        let mut lexer = lexer_from_str("\"foo\\\\bar\"");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::StringLiteral("foo\\bar".to_string()));
    }

    #[test]
    fn test_number_literal_token() {
        let mut lexer = lexer_from_str("42");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::IntegerLiteral(42));
    }

    #[test]
    fn test_floating_point_literal_token() {
        let mut lexer = lexer_from_str("42.42");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::FloatLiteral(42.42));
    }

    #[test]
    fn test_plus_token() {
        let mut lexer = lexer_from_str("+");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::Plus);
    }

    #[test]
    fn test_minus_token() {
        let mut lexer = lexer_from_str("-");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::Minus);
    }

    #[test]
    fn test_asterisk_token() {
        let mut lexer = lexer_from_str("*");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::Asterisk);
    }

    #[test]
    fn test_slash_token() {
        let mut lexer = lexer_from_str("/");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::Slash);
    }

    #[test]
    fn test_assignment_token() {
        let mut lexer = lexer_from_str("=");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::EqualSign);
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
    fn test_open_bracket_token() {
        let mut lexer = lexer_from_str("[");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::OpenBracket);
    }

    #[test]
    fn test_close_bracket_token() {
        let mut lexer = lexer_from_str("]");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::CloseBracket);
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
    fn test_string_token() {
        let mut lexer = lexer_from_str("string");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::String);
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
                Token::EqualSign,
                Token::IntegerLiteral(42),
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

    #[test]
    fn test_true_token() {
        let mut lexer = lexer_from_str("true");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::BooleanLiteral(true));
    }

    #[test]
    fn test_false_token() {
        let mut lexer = lexer_from_str("false");
        let token = lexer.next_token().unwrap();
        assert_eq!(token, Token::BooleanLiteral(false));
    }
}
