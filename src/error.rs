use std::{error, fmt};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CompilerErrorType {
    LexerError,
    ASTError,
}

impl fmt::Display for CompilerErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompilerErrorType::LexerError => write!(f, "Lexer Error"),
            CompilerErrorType::ASTError => write!(f, "Syntax Error"),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct CompilerError {
    message: String,
    line_number: u64,
    error_type: CompilerErrorType,
}

impl CompilerError {
    pub fn new(
        error_type: CompilerErrorType,
        line_number: u64,
        message: impl Into<String>,
    ) -> Self {
        Self {
            message: message.into(),
            line_number,
            error_type,
        }
    }
}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}: {} on line {}",
            self.error_type, self.message, self.line_number
        )
    }
}

impl From<CompilerError> for String {
    fn from(value: CompilerError) -> Self {
        format!(
            "{}: {} on line {}",
            value.error_type, value.message, value.line_number
        )
    }
}

impl error::Error for CompilerError {}
