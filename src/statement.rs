extern crate inkwell;

use crate::ast::{ASTNode, Operator};
use ::inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use std::collections::HashMap;

use crate::common_utils::{generate_constant_name, into_basic_value_enum};
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{
    AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValueEnum, PointerValue,
};

macro_rules! recursive_statement_compile {
    ($curr_stmt: expr, $eval_stmt: expr) => {
        PyroStatement::compile_statement(
            $curr_stmt.context,
            $curr_stmt.module,
            $curr_stmt.builder,
            $curr_stmt.entry_block,
            $curr_stmt.local_variables,
            $curr_stmt.string_globals,
            $eval_stmt,
        )
    }
}

pub(crate) struct PyroStatement<'a, 'ctx> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    entry_block: Option<BasicBlock<'ctx>>,
    local_variables: &'a mut HashMap<String, PointerValue<'ctx>>,
    string_globals: &'a mut HashMap<String, PointerValue<'ctx>>,
    statement: ASTNode,
}

impl<'a, 'ctx> PyroStatement<'a, 'ctx> {
    pub(crate) fn compile_statement(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,

        entry_block: Option<BasicBlock<'ctx>>,
        local_variables: &'a mut HashMap<String, PointerValue<'ctx>>,
        string_globals: &'a mut HashMap<String, PointerValue<'ctx>>,
        statement: ASTNode,
    ) -> Result<AnyValueEnum<'ctx>, String> {
        let statement = PyroStatement {
            context,
            module,
            builder,

            entry_block,
            local_variables,
            string_globals,
            statement,
        };

        statement.compile()
    }

    fn load_identifier(self) -> Result<AnyValueEnum<'ctx>, String> {
        match self.statement {
            ASTNode::Identifier(variable_name) => {
                let variable_ptr = self.local_variables.get(&variable_name);
                if variable_ptr.is_none() {
                    return Err(format!(
                        "Variable {} can not be found in scope.",
                        variable_name
                    ));
                }
                let variable_ptr = *variable_ptr.unwrap();
                let variable = self.builder.build_load(variable_ptr, &variable_name);
                Ok(variable.as_any_value_enum())
            }
            _ => Err(format!("Not an identifier statement")),
        }
    }

    fn build_literal(self) -> Result<AnyValueEnum<'ctx>, String> {
        match self.statement {
            ASTNode::IntegerLiteral(value) => {
                let const_value = self.context.i64_type().const_int(value as u64, false);
                Ok(const_value.as_any_value_enum())
            }

            ASTNode::FloatLiteral(value) => {
                let const_value = self.context.f64_type().const_float(value as f64);
                Ok(const_value.as_any_value_enum())
            }

            ASTNode::StringLiteral(string) => {
                if self.string_globals.contains_key(string.as_str()) {
                    let ptr = self.string_globals.get(string.as_str()).unwrap();
                    return Ok(ptr.as_any_value_enum());
                }

                let mut const_name;
                loop {
                    const_name = generate_constant_name();
                    if !self.string_globals.contains_key(&const_name) {
                        break;
                    }
                }
                let ptr = self
                    .builder
                    .build_global_string_ptr(&string, const_name.as_str());
                let ptr = ptr.as_pointer_value();
                self.string_globals.insert(string, ptr);
                Ok(ptr.as_any_value_enum())
            }
            _ => Err(format!("{:?} is not a literal", self.statement)),
        }
    }

    fn allocate_stack_variable<T: BasicType<'ctx>>(
        &self,
        name: &str,
        ty: T,
    ) -> Result<PointerValue<'ctx>, String> {
        if self.entry_block.is_some() {
            let local_builder = self.context.create_builder();
            let entry_block = self.entry_block.unwrap();

            match entry_block.get_first_instruction() {
                Some(first_instruction) => local_builder.position_before(&first_instruction),
                None => local_builder.position_at_end(entry_block),
            }

            return Ok(local_builder.build_alloca(ty, name));
        }

        Err(format!("Let statement without an entry block"))
    }

    fn build_assignment(self) -> Result<AnyValueEnum<'ctx>, String> {
        match &self.statement {
            ASTNode::LetDeclaration(variable_name, expression) => {
                let value = recursive_statement_compile!(self, *expression.clone())?;
                let value = into_basic_value_enum(value)?;

                let variable =
                    self.allocate_stack_variable(variable_name.as_str(), value.get_type())?;
                self.local_variables
                    .insert(variable_name.to_string(), variable);

                self.builder.build_store(variable, value);
                Ok(variable.as_any_value_enum())
            }
            _ => Err(format!("Not a valid Let statement")),
        }
    }

    fn build_function_call(self) -> Result<AnyValueEnum<'ctx>, String> {
        match &self.statement {
            ASTNode::FunctionCall(function_name, arguments) => {
                let function = self.module.get_function(function_name.as_str());
                if function.is_none() {
                    return Err(format!("Could not find function `{}`", function_name));
                }

                let function = function.unwrap();
                let arguments = arguments
                    .iter()
                    .map(|a| {recursive_statement_compile!(self, a.clone())
                    })
                    .collect::<Result<Vec<AnyValueEnum>, String>>()?;

                let arguments = arguments
                    .iter()
                    .map(|a| BasicMetadataValueEnum::try_from(*a))
                    .collect::<Result<Vec<BasicMetadataValueEnum>, _>>();

                if arguments.is_err() {
                    return Err("Error when parsing arguments for function call".to_string());
                }

                let arguments = arguments.unwrap();
                let call = self
                    .builder
                    .build_call(function, arguments.as_slice(), "call");

                Ok(call.as_any_value_enum())
            }

            _ => Err(format!("Not valid Function Call")),
        }
    }

    fn build_return(self) -> Result<AnyValueEnum<'ctx>, String> {
        match &self.statement {
            ASTNode::ReturnStatement(expression) => {
                let value = recursive_statement_compile!(self, *expression.clone())?;
                let value = into_basic_value_enum(value)?;

                let return_instruction = self.builder.build_return(Some(&value));
                Ok(return_instruction.as_any_value_enum())
            }
            _ => Err("Not implemented".to_string()),
        }
    }

    fn build_binary_operation(self) -> Result<AnyValueEnum<'ctx>, String> {
        match &self.statement {
            ASTNode::BinaryOp {
                left,
                operator,
                right,
            } => {
                let lhs = recursive_statement_compile!(self, *left.clone())?;
                let rhs = recursive_statement_compile!(self, *right.clone())?;

                let lhs = into_basic_value_enum(lhs)?;
                let rhs = into_basic_value_enum(rhs)?;

                self.binary_op_construction(lhs, rhs, operator.clone())
            }

            _ => Err(format!("Not a valid binary operation")),
        }
    }

    /// Build a binary operation out of the given operands.
    ///
    /// The `left` operand will determine the type of the operation.
    /// The `right` operand needs to be of the same type for operation to work.
    ///
    /// Returns the resulting LLVM MathValue or an error if the types did not match.
    fn binary_op_construction(
        &self,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
        operator: Operator,
    ) -> Result<AnyValueEnum<'ctx>, String> {
        let op_type = left.get_type();

        match op_type {
            BasicTypeEnum::IntType(_) => match right.get_type() {
                BasicTypeEnum::IntType(_) => {
                    let op;
                    let lhs = left.into_int_value();
                    let rhs = right.into_int_value();

                    match operator {
                        Operator::Plus => op = self.builder.build_int_add(lhs, rhs, "intaddition"),
                        Operator::Minus => {
                            op = self.builder.build_int_sub(lhs, rhs, "intsubtraction")
                        }
                        Operator::Multiplication => {
                            op = self.builder.build_int_mul(lhs, rhs, "intmultiplication")
                        }
                        Operator::Division => {
                            op = self.builder.build_int_signed_div(lhs, rhs, "intdivision")
                        }
                    }
                    return Ok(op.as_any_value_enum());
                }
                _ => {
                    return Err(format!(
                        "Cannot perform integer op: {:?} is an integer while {:?} is not.",
                        left, right
                    ))
                }
            },
            BasicTypeEnum::FloatType(_) => match right.get_type() {
                BasicTypeEnum::FloatType(_) => {
                    let op;
                    let lhs = left.into_float_value();
                    let rhs = right.into_float_value();
                    match operator {
                        Operator::Plus => op = self.builder.build_float_add(lhs, rhs, "floataddition"),
                        Operator::Minus => {
                            op = self.builder.build_float_sub(lhs, rhs, "floatsubtraction")
                        }
                        Operator::Multiplication => {
                            op = self.builder.build_float_mul(lhs, rhs, "floatmultiplication")
                        }
                        Operator::Division => {
                            op = self.builder.build_float_div(lhs, rhs, "floatdivision")
                        }
                    }
                    return Ok(op.as_any_value_enum());
                }
                _ => {
                    return Err(format!(
                        "Cannot perform float op: {:?} is an float while {:?} is not.",
                        left, right
                        ))
                        }
                    }

            _ => return Err(format!("Not a valid operand type")),
        }
    }

    fn compile(self) -> Result<AnyValueEnum<'ctx>, String> {
        match &self.statement {
            ASTNode::Program(_) => Err(format!("Cannot declare program as statement")),
            ASTNode::FunctionDeclaration {
                identifier: _,
                arguments: _,
                return_type: _,
                body: _,
            } => Err(format!("Cannot declare function as statement")),
            ASTNode::FunctionCall(_, _) => self.build_function_call(),
            ASTNode::LetDeclaration(_, _) => self.build_assignment(),
            ASTNode::ReturnStatement(_) => self.build_return(),
            ASTNode::Identifier(_) => self.load_identifier(),
            ASTNode::IntegerLiteral(_) | ASTNode::StringLiteral(_) | ASTNode::FloatLiteral(_) => self.build_literal(),
            ASTNode::BinaryOp {
                left: _,
                operator: _,
                right: _,
            } => self.build_binary_operation(),
        }
    }
}
