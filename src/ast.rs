use crate::lexer::Lexer;
use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum VariableType {
    Void,
    Number,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    pub argument_type: VariableType,
    pub identifier: String,
}

// #[derive(Debug, PartialEq)]
// pub enum Operator {
//     Plus,
// }

// TODO: Probably want to create more Structs to represent the
//       tokens. To avoid having to parse mentally what the Box-values
//       and Vecs point to.
#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode {
    Program(Vec<ASTNode>),

    FunctionDeclaration(String, Vec<Argument>, VariableType, Vec<ASTNode>),
    FunctionCall(String, Vec<ASTNode>),

    LetDeclaration(String, Box<ASTNode>),
    ReturnStatement(Box<ASTNode>),

    // Print(Box<ASTNode>),
    // BinaryOp(Box<ASTNode>, Operator, Box<ASTNode>),
    Identifier(String),
    IntegerLiteral(i64),
}

/// Parser for the simple Pyro programming language.
///
/// The `Parser` struct provides a basic recursive descent parser
/// that can parse a stream of tokens generated by the `Lexer` into
/// an abstract syntax tree (AST) representing the program.
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    next_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        let mut parser = Parser {
            lexer,
            current_token: Token::NoToken,
            next_token: Token::NoToken,
        };

        let init_parser = |parser: &mut Parser| -> Result<(), String> {
            parser.advance()?;
            parser.advance()?;
            Ok(())
        };

        if let Err(_err) = init_parser(&mut parser) {
            panic!("Error while initializing parser");
        }

        parser
    }

    /// Advance the internal lexer one step.
    fn advance(&mut self) -> Result<(), String> {
        self.current_token = self.next_token.clone();
        self.next_token = self.lexer.next_token()?;
        Ok(())
    }

    /// Expect a specific token and consume it.
    fn expect(&mut self, expected: Token) -> Result<(), String> {
        match self.current_token.clone() {
            token if token == expected => self.advance(),
            token => Err(format!(
                "Expected token {:?} but received {:?}",
                expected, token
            )),
        }
    }

    /// Expect an identifier Token, consume it, and return the identifier
    fn expect_identifier(&mut self) -> Result<String, String> {
        match self.current_token.clone() {
            Token::Identifier(identifier) => {
                self.advance()?;
                Ok(identifier)
            }
            token => Err(format!(
                "Expected identifier but received token {:?}",
                token
            )),
        }
    }

    // fn expect_number(&mut self) -> Result<i32, String> {
    //     match self.current_token.clone() {
    //         Some(Token::NumberLiteral(number)) => {
    //             self.advance()?;
    //             Ok(number)
    //         }
    //         Some(token) => Err(format!("Expected number but received token {:?}", token)),
    //         None => Err(format!("Expected number but got no token")),
    //     }
    // }

    /// Expect one of the possible non-void Pyro types, consume it, and return the type.
    ///
    /// Returns the VariableType-representation of the encountered type.
    /// Returns an `Err` if any other kind of token was encountered.
    fn expect_variable_type(&mut self) -> Result<VariableType, String> {
        match self.current_token.clone() {
            Token::Number => {
                self.advance()?;
                Ok(VariableType::Number)
            }
            token => Err(format!(
                "Expected variable type token but received token {:?}",
                token
            )),
        }
    }

    /// Expect a Pyro type followed by an identifier, consume them, and
    /// return the resulting `Argument`.
    ///
    /// Returns `Err` if an `Argument` could not be constructed.
    fn expect_argument(&mut self) -> Result<Argument, String> {
        let argument_type = self.expect_variable_type()?;
        let identifier = self.expect_identifier()?;

        Ok(Argument {
            argument_type,
            identifier,
        })
    }

    /// Expect a specific token but do not consume it.
    fn check(&self, expected: Token) -> bool {
        self.current_token == expected
    }

    /// Peek at the current token in the sequence.
    fn peek_current(&self) -> Token {
        self.current_token.clone()
    }

    /// Look up the next token in the sequence
    fn peek_ahead(&self) -> Token {
        self.next_token.clone()
    }

    /// Parses an entire Program from the input lexer.
    ///
    /// Returns an `Err` if the input is an invalid pyro program.
    pub fn parse_program(&mut self) -> Result<ASTNode, String> {
        let mut functions = Vec::new();

        while self.next_token != Token::Eof {
            let function = self.parse_function_declaration()?;
            functions.push(function);
        }

        Ok(ASTNode::Program(functions))
    }

    /// Parse a function declaration from the Tokens generated by the lexer.
    ///
    /// Return `Err` if a valid function couldn't be parsed.
    fn parse_function_declaration(&mut self) -> Result<ASTNode, String> {
        self.expect(Token::Func)?;
        let identifier = self.expect_identifier()?;
        let argument_list = self.parse_argument_list()?;
        let variable_type = self.parse_return_type()?;
        let statements = self.parse_statement_list()?;

        Ok(ASTNode::FunctionDeclaration(
            identifier,
            argument_list,
            variable_type,
            statements,
        ))
    }

    /// Parse function argument list enclosed by parenthesis.
    fn parse_argument_list(&mut self) -> Result<Vec<Argument>, String> {
        let mut arguments = Vec::new();

        self.expect(Token::OpenParen)?;
        if self.check(Token::CloseParen) {
            self.advance()?;
            return Ok(arguments);
        }

        loop {
            let argument = self.expect_argument()?;
            arguments.push(argument);
            match self.peek_current() {
                Token::Comma => {
                    self.advance()?;
                    continue;
                }
                Token::CloseParen => {
                    self.advance()?;
                    return Ok(arguments);
                }
                other => {
                    return Err(format!(
                        "Expected comma (',') or closing paren (')') but got {:?}",
                        other
                    ));
                }
            }
        }
    }

    // Parse the (optional) return type of the currently parsed function, indicating which
    // VariableType will be returned by the function.
    fn parse_return_type(&mut self) -> Result<VariableType, String> {
        match self.peek_current() {
            Token::GreaterThan => {
                self.advance()?;
                self.expect_variable_type()
            }
            _ => Ok(VariableType::Void),
        }
    }

    /// Parse a statement list enclosed by braces.
    fn parse_statement_list(&mut self) -> Result<Vec<ASTNode>, String> {
        let mut statements = Vec::new();

        self.expect(Token::OpenBrace)?;
        if self.check(Token::CloseBrace) {
            self.advance()?;
            return Ok(statements);
        }

        loop {
            let statement = self.parse_statement()?;
            statements.push(statement);
            match self.peek_current() {
                Token::CloseBrace => {
                    self.advance()?;
                    return Ok(statements);
                }
                Token::Eof => {
                    return Err("Reached EOF while scanning for closing brace ('}}')".to_string());
                }
                _other => continue,
            }
        }
    }

    /// Parse a return statement capable of returning any expression.
    fn parse_return_statement(&mut self) -> Result<ASTNode, String> {
        self.expect(Token::Return)?;
        let expression = self.parse_expression()?;

        Ok(ASTNode::ReturnStatement(Box::new(expression)))
    }

    /// Parse a statement in general.
    ///
    /// This method essentially wraps the other statement parsers to use
    /// the correct one at the correct intance.
    fn parse_statement(&mut self) -> Result<ASTNode, String> {
        let return_node;
        match self.peek_current() {
            Token::Let => {
                return_node = self.parse_let_statement()?;
            }
            Token::Return => {
                return_node = self.parse_return_statement()?;
            }
            Token::Eof => {
                return Err("Reached EOF while parsing statements".to_string());
            }
            _ => {
                return_node = self.parse_expression()?;
            }
        }
        self.expect(Token::SemiColon)?;
        Ok(return_node)
    }

    fn parse_let_statement(&mut self) -> Result<ASTNode, String> {
        let identifier;
        let expression;

        self.expect(Token::Let)?;
        identifier = self.expect_identifier()?;
        self.expect(Token::Assignment)?;
        expression = self.parse_expression()?;

        Ok(ASTNode::LetDeclaration(identifier, Box::new(expression)))
    }

    /// Parse a full call to a function, with function name and arguments.
    fn parse_function_call(&mut self) -> Result<ASTNode, String> {
        // TODO: This is VERY similar to parse_argument_list
        // TODO: Would make sense to join the two somehow.
        let mut argument_list = Vec::new();

        let identifier = self.expect_identifier()?;
        self.expect(Token::OpenParen)?;
        if self.check(Token::CloseParen) {
            self.advance()?;
            return Ok(ASTNode::FunctionCall(identifier, argument_list));
        }
        loop {
            let argument = self.parse_expression()?;
            argument_list.push(argument);
            match self.peek_current() {
                Token::Comma => {
                    self.advance()?;
                    continue;
                }
                Token::CloseParen => {
                    self.advance()?;
                    return Ok(ASTNode::FunctionCall(identifier, argument_list));
                }
                other => {
                    return Err(format!(
                        "Expected comma (',') or closing paren (')') but got {:?}",
                        other
                    ));
                }
            }
        }
    }

    /// Parse an expression from the input tokens.
    /// Currently only supports integer literals identifiers, and function calls
    /// as expressions.
    fn parse_expression(&mut self) -> Result<ASTNode, String> {
        match self.peek_current() {
            Token::NumberLiteral(literal) => {
                self.advance()?;
                Ok(ASTNode::IntegerLiteral(literal))
            }
            Token::Identifier(literal) => {
                if self.peek_ahead() == Token::OpenParen {
                    return self.parse_function_call();
                }
                self.advance()?;
                return Ok(ASTNode::Identifier(literal));
            }
            other => Err(format!("Expected expression but got {:?}", other)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_function() {
        let program = "
            func foo() {
            }";
        let lexer = Lexer::new(program);

        let mut parser = super::Parser::new(lexer);
        let ast = parser.parse_program();
        if ast.is_err() {
            panic!("{:?}", ast);
        }

        let ast = ast.unwrap();
        match ast {
            ASTNode::Program(functions) => {
                assert_eq!(functions.len(), 1);
                let function = functions.first().unwrap();
                match function {
                    ASTNode::FunctionDeclaration(name, arguments, return_type, statements) => {
                        assert_eq!(name, "foo");
                        assert_eq!(*return_type, VariableType::Void);
                        assert_eq!(arguments.len(), 0);
                        assert_eq!(statements.len(), 0);
                    }
                    _ => panic!("Expected function declaration"),
                }
            }
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_function_with_arguments() {
        let program = "
            func foo(number x, number y, number z) {
            }";
        let lexer = Lexer::new(program);

        let mut parser = super::Parser::new(lexer);
        let ast = parser.parse_program();
        if ast.is_err() {
            panic!("{:?}", ast);
        }

        let ast = ast.unwrap();
        match ast {
            ASTNode::Program(functions) => {
                assert_eq!(functions.len(), 1);
                let function = functions.first().unwrap();
                match function {
                    ASTNode::FunctionDeclaration(name, arguments, return_type, statements) => {
                        assert_eq!(name, "foo");
                        assert_eq!(*return_type, VariableType::Void);
                        assert_eq!(arguments.len(), 3);
                        assert_eq!(
                            arguments,
                            &vec!(
                                Argument {
                                    argument_type: VariableType::Number,
                                    identifier: "x".to_string(),
                                },
                                Argument {
                                    argument_type: VariableType::Number,
                                    identifier: "y".to_string(),
                                },
                                Argument {
                                    argument_type: VariableType::Number,
                                    identifier: "z".to_string(),
                                },
                            )
                        );
                        assert_eq!(statements.len(), 0);
                    }
                    _ => panic!("Expected function declaration"),
                }
            }
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_function_with_let_statements() {
        let program = "
            func foo() {
                let x = 1;
                let y = x;
            }";
        let lexer = Lexer::new(program);

        let mut parser = super::Parser::new(lexer);
        let ast = parser.parse_program();
        if ast.is_err() {
            panic!("{:?}", ast);
        }

        let ast = ast.unwrap();
        match ast {
            ASTNode::Program(functions) => {
                assert_eq!(functions.len(), 1);
                let function = functions.first().unwrap();
                match function {
                    ASTNode::FunctionDeclaration(name, arguments, return_type, statements) => {
                        assert_eq!(name, "foo");
                        assert_eq!(*return_type, VariableType::Void);
                        assert_eq!(arguments.len(), 0);
                        assert_eq!(statements.len(), 2);

                        match statements.get(0).unwrap() {
                            ASTNode::LetDeclaration(identifier, expression) => {
                                assert_eq!(identifier, "x");
                                assert_eq!(**expression, ASTNode::IntegerLiteral(1));
                            }
                            _ => panic!("Expected let declaration"),
                        }

                        match statements.get(1).unwrap() {
                            ASTNode::LetDeclaration(identifier, expression) => {
                                assert_eq!(identifier, "y");
                                assert_eq!(**expression, ASTNode::Identifier("x".to_string()));
                            }
                            _ => panic!("Expected let declaration"),
                        }
                    }
                    _ => panic!("Expected function declaration"),
                }
            }
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_function_with_print_statement() {
        let program = "
            func foo() {
                print (x);
            }";
        let lexer = Lexer::new(program);

        let mut parser = super::Parser::new(lexer);
        let ast = parser.parse_program();
        if ast.is_err() {
            panic!("{:?}", ast);
        }

        let ast = ast.unwrap();
        match ast {
            ASTNode::Program(functions) => {
                assert_eq!(functions.len(), 1);
                let function = functions.first().unwrap();
                match function {
                    ASTNode::FunctionDeclaration(name, arguments, return_type, statements) => {
                        assert_eq!(name, "foo");
                        assert_eq!(*return_type, VariableType::Void);
                        assert_eq!(arguments.len(), 0);
                        assert_eq!(statements.len(), 1);

                        match statements.get(0).unwrap() {
                            ASTNode::FunctionCall(function_name, arguments) => {
                                assert_eq!(function_name, "print");
                                assert_eq!(arguments.len(), 1);
                                assert_eq!(
                                    *arguments.first().unwrap(),
                                    ASTNode::Identifier("x".to_string())
                                );
                            }
                            _ => panic!("Expected function call to print"),
                        }
                    }
                    _ => panic!("Expected function declaration"),
                }
            }
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_function_with_return_value() {
        let program = "
            func foo(number x) > number {
                return x;
            }";
        let lexer = Lexer::new(program);

        let mut parser = super::Parser::new(lexer);
        let ast = parser.parse_program();
        if ast.is_err() {
            panic!("{:?}", ast);
        }

        let ast = ast.unwrap();
        match ast {
            ASTNode::Program(functions) => {
                assert_eq!(functions.len(), 1);
                let function = functions.first().unwrap();
                match function {
                    ASTNode::FunctionDeclaration(name, arguments, return_type, statements) => {
                        assert_eq!(name, "foo");
                        assert_eq!(arguments.len(), 1);
                        assert_eq!(statements.len(), 1);

                        match statements.get(0).unwrap() {
                            ASTNode::ReturnStatement(expression) => {
                                assert_eq!(**expression, ASTNode::Identifier("x".to_string()));
                            }
                            _ => panic!("Expected ReturnStatement"),
                        }
                    }
                    _ => panic!("Expected function declaration"),
                }
            }
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_error_let_instead_of_expression() {
        let program = "
            func foo() {
                let x = let y;
            }";
        let lexer = Lexer::new(program);

        let mut parser = super::Parser::new(lexer);
        let ast = parser.parse_program();
        assert!(ast.is_err());
    }

    #[test]
    fn test_incorrect_return_value_for_function() {
        let program = "
            func foo(number x) > {
                return x;
            }";
        let lexer = Lexer::new(program);

        let mut parser = super::Parser::new(lexer);
        let ast = parser.parse_program();
        assert!(ast.is_err());
    }
}
