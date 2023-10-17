use crate::lexer::{Lexer, Token};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum VariableType {
    Void,
    Integer,
    Boolean,
    Float,
    String,
    Class(String),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Variable {
    pub variable_type: VariableType,
    pub identifier: String,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Operator {
    Plus,
    Minus,
    Multiplication,
    Division,
}

fn token_to_operator(token: &Token) -> Result<Operator, String> {
    match token {
        Token::Plus => Ok(Operator::Plus),
        Token::Minus => Ok(Operator::Minus),
        Token::Asterisk => Ok(Operator::Multiplication),
        Token::Slash => Ok(Operator::Division),
        _ => Err(format!("Invalid operator: {:?}", token)),
    }
}

// TODO: Probably want to create more Structs to represent the
//       tokens. To avoid having to parse mentally what the Box-values
//       and Vecs point to.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ASTNode {
    Program(Vec<ASTNode>),

    FunctionDeclaration {
        identifier: String,
        arguments: Vec<Variable>,
        return_type: VariableType,
        body: Vec<ASTNode>,
    },
    FunctionCall(String, Vec<ASTNode>),

    IfStatement {
        condition: Box<ASTNode>,
        then_body: Vec<ASTNode>,
        else_body: Vec<ASTNode>,
    },

    ClassDeclaration {
        identifier: String,
        fields: Vec<Variable>,
        methods: Vec<ASTNode>,
    },

    ObjectAllocation(VariableType),
    ObjectFieldAccess {
        object_identifier: String,
        field_identifier: String,
    },
    ObjectFieldAssignment {
        object_identifier: String,
        field_identifier: String,
        value: Box<ASTNode>,
    },

    ArrayAllocation {
        variable_type: VariableType,
        size: Box<ASTNode>,
    },
    ArrayAccess(String, Box<ASTNode>),
    ArrayAssignment(String, Box<ASTNode>, Box<ASTNode>),

    DestroyVariable(String),
    VariableAssignment(String, Box<ASTNode>),
    LetDeclaration(String, Box<ASTNode>),
    ReturnStatement(Box<ASTNode>),

    BinaryOp {
        operator: Operator,
        left: Box<ASTNode>,
        right: Box<ASTNode>,
    },
    Identifier(String),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BooleanLiteral(bool),
}

/// Parser for the simple Pyro programming language.
///
/// The `Parser` struct provides a basic recursive descent parser
/// that can parse a stream of tokens generated by the `Lexer` into
/// an abstract syntax tree (AST) representing the program.
pub(crate) struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    next_token: Token,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(lexer: Lexer<'a>) -> Parser<'a> {
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

    /// Parses an entire Program from the input lexer.
    ///
    /// Returns an `Err` if the input is an invalid pyro program.
    pub(crate) fn parse_program(&mut self) -> Result<ASTNode, String> {
        let mut program_contents = Vec::new();

        while self.next_token != Token::Eof {
            match self.peek_current() {
                Token::Class => {
                    let class = self.parse_class_declaration()?;
                    program_contents.push(class);
                }
                Token::Func => {
                    let function = self.parse_function_declaration()?;
                    program_contents.push(function);
                }
                _ => return Err(format!("Unexpected token: {:?}", self.current_token)),
            }
        }

        Ok(ASTNode::Program(program_contents))
    }

    /// Advance the internal lexer one step.
    fn advance(&mut self) -> Result<(), String> {
        self.current_token = self.next_token.clone();
        self.next_token = self.lexer.next_token()?;
        Ok(())
    }

    /// Expect a specific token and consume it.
    fn expect(&mut self, expected: Token) -> Result<(), String> {
        match self.peek_current() {
            token if token == expected => self.advance(),
            token => Err(format!(
                "Expected token {:?} but received {:?}",
                expected, token
            )),
        }
    }

    /// Expect an identifier Token, consume it, and return the identifier
    fn expect_identifier(&mut self) -> Result<String, String> {
        match self.peek_current() {
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

    /// Expect one of the possible non-void Pyro types, consume it, and return
    /// the type.
    ///
    /// Returns the VariableType-representation of the encountered type.
    /// Returns an `Err` if any other kind of token was encountered.
    fn expect_variable_type(&mut self) -> Result<VariableType, String> {
        match self.peek_current() {
            Token::Integer => {
                self.advance()?;
                Ok(VariableType::Integer)
            }
            Token::Float => {
                self.advance()?;
                Ok(VariableType::Float)
            }
            Token::Boolean => {
                self.advance()?;
                Ok(VariableType::Boolean)
            }
            Token::String => {
                self.advance()?;
                Ok(VariableType::String)
            }
            Token::Identifier(identifier) => {
                self.advance()?;
                Ok(VariableType::Class(identifier))
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
    fn expect_variable(&mut self) -> Result<Variable, String> {
        let argument_type = self.expect_variable_type()?;
        let identifier = self.expect_identifier()?;

        Ok(Variable {
            variable_type: argument_type,
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

    fn parse_class_declaration(&mut self) -> Result<ASTNode, String> {
        self.expect(Token::Class)?;
        let identifier = self.expect_identifier()?;
        self.expect(Token::OpenBrace)?;

        let fields = self.parse_class_variables()?;
        let methods = Vec::new();
        self.expect(Token::CloseBrace)?;

        return Ok(ASTNode::ClassDeclaration {
            identifier,
            fields,
            methods,
        });
    }

    fn parse_class_variables(&mut self) -> Result<Vec<Variable>, String> {
        let mut variables = Vec::new();

        if self.check(Token::CloseBrace) || self.check(Token::Func) {
            return Ok(variables);
        }

        loop {
            let variable = self.expect_variable()?;
            variables.push(variable);
            self.expect(Token::SemiColon)?;

            match self.peek_current() {
                Token::Func | Token::CloseBrace => {
                    return Ok(variables);
                }
                _ => continue,
            }
        }
    }

    fn parse_object_access(&mut self) -> Result<ASTNode, String> {
        let object_identifier = self.expect_identifier()?;
        self.expect(Token::Dot)?;
        let field_identifier = self.expect_identifier()?;

        if self.peek_current() == Token::EqualSign {
            self.expect(Token::EqualSign)?;
            return Ok(ASTNode::ObjectFieldAssignment {
                object_identifier,
                field_identifier,
                value: Box::new(self.parse_expression()?),
            });
        }

        Ok(ASTNode::ObjectFieldAccess {
            object_identifier,
            field_identifier,
        })
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

        Ok(ASTNode::FunctionDeclaration {
            identifier,
            arguments: argument_list,
            return_type: variable_type,
            body: statements,
        })
    }

    /// Parse function argument list enclosed by parenthesis.
    fn parse_argument_list(&mut self) -> Result<Vec<Variable>, String> {
        let mut arguments = Vec::new();

        self.expect(Token::OpenParen)?;
        if self.check(Token::CloseParen) {
            self.advance()?;
            return Ok(arguments);
        }

        loop {
            let argument = self.expect_variable()?;
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

    // Parse the (optional) return type of the currently parsed function, indicating
    // which VariableType will be returned by the function.
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
        self.expect(Token::SemiColon)?;

        Ok(ASTNode::ReturnStatement(Box::new(expression)))
    }

    /// Parse an if statement and an optional else-branch.
    fn parse_if_statement(&mut self) -> Result<ASTNode, String> {
        self.expect(Token::If)?;
        self.expect(Token::OpenParen)?;

        let condition = self.parse_expression()?;

        self.expect(Token::CloseParen)?;
        let true_statements = self.parse_statement_list()?;

        let else_statements;
        if self.peek_current() == Token::Else {
            self.expect(Token::Else)?;
            else_statements = self.parse_statement_list()?;
        } else {
            else_statements = Vec::new();
        }

        Ok(ASTNode::IfStatement {
            condition: Box::new(condition),
            then_body: true_statements,
            else_body: else_statements,
        })
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
            Token::Destroy => {
                return_node = self.parse_destroy_statement()?;
            }
            Token::If => {
                return_node = self.parse_if_statement()?;
            }
            Token::Eof => {
                return Err("Reached EOF while parsing statements".to_string());
            }
            _ => {
                return_node = self.parse_expression()?;
                self.expect(Token::SemiColon)?;
            }
        }
        Ok(return_node)
    }

    fn parse_let_statement(&mut self) -> Result<ASTNode, String> {
        let identifier;
        let expression;

        self.expect(Token::Let)?;
        identifier = self.expect_identifier()?;
        self.expect(Token::EqualSign)?;
        if self.peek_current() == Token::Create {
            expression = self.parse_create()?;
        } else {
            expression = self.parse_expression()?;
        }
        self.expect(Token::SemiColon)?;

        Ok(ASTNode::LetDeclaration(identifier, Box::new(expression)))
    }

    fn parse_array_access(&mut self) -> Result<ASTNode, String> {
        let identifier = self.expect_identifier()?;
        self.expect(Token::OpenBracket)?;
        let index = self.parse_expression()?;
        self.expect(Token::CloseBracket)?;

        if self.peek_current() == Token::EqualSign {
            self.expect(Token::EqualSign)?;
            let value = self.parse_expression()?;

            return Ok(ASTNode::ArrayAssignment(
                identifier,
                Box::new(index),
                Box::new(value),
            ));
        }

        Ok(ASTNode::ArrayAccess(identifier, Box::new(index)))
    }

    fn parse_variable_assignment(&mut self) -> Result<ASTNode, String> {
        let identifier = self.expect_identifier()?;
        self.expect(Token::EqualSign)?;
        let value = self.parse_expression()?;
        Ok(ASTNode::VariableAssignment(identifier, Box::new(value)))
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

    fn parse_create(&mut self) -> Result<ASTNode, String> {
        self.expect(Token::Create)?;
        let variable_type = self.expect_variable_type()?;

        if self.peek_ahead() == Token::OpenBracket {
            self.expect(Token::OpenBracket)?;
            let size = self.parse_expression()?;
            self.expect(Token::CloseBracket)?;

            return Ok(ASTNode::ArrayAllocation {
                variable_type,
                size: Box::new(size),
            });
        }

        Ok(ASTNode::ObjectAllocation(variable_type))
    }

    fn parse_destroy_statement(&mut self) -> Result<ASTNode, String> {
        self.expect(Token::Destroy)?;
        let variable_name = self.expect_identifier()?;
        self.expect(Token::SemiColon)?;
        Ok(ASTNode::DestroyVariable(variable_name))
    }

    fn parse_identifier(&mut self) -> Result<ASTNode, String> {
        let return_node: ASTNode;
        if self.peek_ahead() == Token::OpenParen {
            return_node = self.parse_function_call()?;
        } else if self.peek_ahead() == Token::OpenBracket {
            return_node = self.parse_array_access()?;
        } else if self.peek_ahead() == Token::EqualSign {
            return_node = self.parse_variable_assignment()?;
        } else if self.peek_ahead() == Token::Dot {
            return_node = self.parse_object_access()?
        } else {
            let identifier = self.expect_identifier()?;
            return_node = ASTNode::Identifier(identifier);
        }
        Ok(return_node)
    }

    /// Parse an expression from the input tokens.
    fn parse_expression(&mut self) -> Result<ASTNode, String> {
        let mut left;
        match self.peek_current() {
            Token::IntegerLiteral(literal) => {
                self.advance()?;
                left = ASTNode::IntegerLiteral(literal);
            }
            Token::FloatLiteral(literal) => {
                self.advance()?;
                left = ASTNode::FloatLiteral(literal);
            }
            Token::BooleanLiteral(literal) => {
                self.advance()?;
                left = ASTNode::BooleanLiteral(literal);
            }
            Token::StringLiteral(string) => {
                self.advance()?;
                left = ASTNode::StringLiteral(string);
            }
            Token::Identifier(_) => {
                left = self.parse_identifier()?;
            }
            other => return Err(format!("Expected expression but got {:?}", other)),
        }
        if self.check(Token::Plus)
            || self.check(Token::Minus)
            || self.check(Token::Asterisk)
            || self.check(Token::Slash)
        {
            let operator = token_to_operator(&self.peek_current())?;
            self.advance()?;
            let right = self.parse_expression()?;
            left = ASTNode::BinaryOp {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
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
                    ASTNode::FunctionDeclaration {
                        identifier,
                        arguments,
                        return_type,
                        body,
                    } => {
                        assert_eq!(identifier, "foo");
                        assert_eq!(*return_type, VariableType::Void);
                        assert_eq!(arguments.len(), 0);
                        assert_eq!(body.len(), 0);
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
            func foo(integer x, integer y, integer z) {
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
                    ASTNode::FunctionDeclaration {
                        identifier,
                        arguments,
                        return_type,
                        body,
                    } => {
                        assert_eq!(identifier, "foo");
                        assert_eq!(*return_type, VariableType::Void);
                        assert_eq!(arguments.len(), 3);
                        assert_eq!(
                            arguments,
                            &vec!(
                                Variable {
                                    variable_type: VariableType::Integer,
                                    identifier: "x".to_string(),
                                },
                                Variable {
                                    variable_type: VariableType::Integer,
                                    identifier: "y".to_string(),
                                },
                                Variable {
                                    variable_type: VariableType::Integer,
                                    identifier: "z".to_string(),
                                },
                            )
                        );
                        assert_eq!(body.len(), 0);
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
                    ASTNode::FunctionDeclaration {
                        identifier,
                        arguments,
                        return_type,
                        body,
                    } => {
                        assert_eq!(identifier, "foo");
                        assert_eq!(*return_type, VariableType::Void);
                        assert_eq!(arguments.len(), 0);
                        assert_eq!(body.len(), 2);

                        match body.get(0).unwrap() {
                            ASTNode::LetDeclaration(identifier, expression) => {
                                assert_eq!(identifier, "x");
                                assert_eq!(**expression, ASTNode::IntegerLiteral(1));
                            }
                            _ => panic!("Expected let declaration"),
                        }

                        match body.get(1).unwrap() {
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
                    ASTNode::FunctionDeclaration {
                        identifier,
                        arguments,
                        return_type,
                        body,
                    } => {
                        assert_eq!(identifier, "foo");
                        assert_eq!(*return_type, VariableType::Void);
                        assert_eq!(arguments.len(), 0);
                        assert_eq!(body.len(), 1);

                        match body.get(0).unwrap() {
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
    fn test_function_returning_string() {
        let program = "
        func foo() > string {
            return \"hello world\";
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
                    ASTNode::FunctionDeclaration {
                        identifier,
                        arguments,
                        return_type,
                        body,
                    } => {
                        assert_eq!(identifier, "foo");
                        assert_eq!(*return_type, VariableType::String);
                        assert_eq!(arguments.len(), 0);
                        assert_eq!(body.len(), 1);
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
            func foo(integer x) > integer {
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
                    ASTNode::FunctionDeclaration {
                        identifier,
                        arguments,
                        return_type: _,
                        body,
                    } => {
                        assert_eq!(identifier, "foo");
                        assert_eq!(arguments.len(), 1);
                        assert_eq!(body.len(), 1);

                        match body.get(0).unwrap() {
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
    fn test_plus_expression() {
        let program = "
            func foo() {
                return 1 + 2;
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
                    ASTNode::FunctionDeclaration {
                        identifier,
                        arguments,
                        return_type: _,
                        body,
                    } => {
                        assert_eq!(identifier, "foo");
                        assert_eq!(arguments.len(), 0);
                        assert_eq!(body.len(), 1);

                        match body.get(0).unwrap() {
                            ASTNode::ReturnStatement(expression) => {
                                assert_eq!(
                                    **expression,
                                    ASTNode::BinaryOp {
                                        operator: Operator::Plus,
                                        left: Box::new(ASTNode::IntegerLiteral(1)),
                                        right: Box::new(ASTNode::IntegerLiteral(2)),
                                    }
                                );
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
    fn test_parse_string_assignment() {
        let program = "
            func foo() {
                let x = \"hello\";
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
                    ASTNode::FunctionDeclaration {
                        identifier,
                        arguments,
                        return_type: _,
                        body,
                    } => {
                        assert_eq!(identifier, "foo");
                        assert_eq!(arguments.len(), 0);
                        assert_eq!(body.len(), 1);

                        match body.get(0).unwrap() {
                            ASTNode::LetDeclaration(identifier, expression) => {
                                assert_eq!(identifier, "x");
                                assert_eq!(
                                    **expression,
                                    ASTNode::StringLiteral("hello".to_string())
                                );
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
    fn test_if_conditional() {
        let program = "
            func foo() > boolean {
                if (true) {
                    return true;
                }
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
                    ASTNode::FunctionDeclaration {
                        identifier,
                        arguments,
                        return_type: _,
                        body,
                    } => {
                        assert_eq!(identifier, "foo");
                        assert_eq!(arguments.len(), 0);
                        assert_eq!(body.len(), 1);

                        match body.get(0).unwrap() {
                            ASTNode::IfStatement {
                                condition,
                                then_body,
                                else_body,
                            } => {
                                assert_eq!(**condition, ASTNode::BooleanLiteral(true));
                                assert_eq!(then_body.len(), 1);
                                assert_eq!(
                                    then_body.first().unwrap(),
                                    &ASTNode::ReturnStatement(Box::new(ASTNode::BooleanLiteral(
                                        true
                                    )))
                                );
                            }
                            _ => panic!("Expected if statement"),
                        }
                    }
                    _ => panic!("Expected function declaration"),
                }
            }
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_boolean_literal_value() {
        let program = "
            func foo() {
                let x = true;
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
                    ASTNode::FunctionDeclaration {
                        identifier,
                        arguments,
                        return_type: _,
                        body,
                    } => {
                        assert_eq!(identifier, "foo");
                        assert_eq!(arguments.len(), 0);
                        assert_eq!(body.len(), 1);
                        match body.get(0).unwrap() {
                            ASTNode::LetDeclaration(identifier, expression) => {
                                assert_eq!(identifier, "x");
                                assert_eq!(**expression, ASTNode::BooleanLiteral(true));
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
            func foo(integer x) > {
                return x;
            }";
        let lexer = Lexer::new(program);

        let mut parser = super::Parser::new(lexer);
        let ast = parser.parse_program();
        assert!(ast.is_err());
    }

    #[test]
    fn test_class_declaration() {
        let program = "
            class Foo {
                integer bar;
            }";
        let lexer = Lexer::new(program);
        let mut parser = super::Parser::new(lexer);
        let ast = parser.parse_program();
        if ast.is_err() {
            panic!("{:?}", ast);
        }
        let ast = ast.unwrap();
        match ast {
            ASTNode::Program(contents) => {
                assert_eq!(contents.len(), 1);
                let class = contents.first().unwrap();
                match class {
                    ASTNode::ClassDeclaration {
                        identifier,
                        fields,
                        methods,
                    } => {
                        assert_eq!(identifier, "Foo");
                        assert_eq!(fields.len(), 1);
                        let field = fields.first().unwrap();
                        assert_eq!(field.identifier, "bar");
                        assert_eq!(field.variable_type, VariableType::Integer);
                        assert_eq!(methods.len(), 0);
                    }
                    _ => panic!("Expected class declaration"),
                }
            }
            _ => panic!("Expected program"),
        }
    }
}
