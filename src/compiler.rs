extern crate inkwell;

use std::collections::HashMap;
use std::env::var;
use std::error::Error;
use std::fmt::Pointer;
use std::hash::Hash;
use std::path::Path;
use std::process::Command;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::support::LLVMString;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetData, TargetMachine,
};
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, PointerType,
};
use inkwell::values::{
    AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue,
    IntValue, PointerValue,
};
use inkwell::{execution_engine, AddressSpace, OptimizationLevel};

use crate::ast::{ASTNode, Argument, Operator, VariableType};

struct FunctionImplementation<'a, 'ctx> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: Builder<'ctx>,

    identifier: String,
    arguments: Vec<Argument>,
    body: Vec<ASTNode>,
    return_type: VariableType,

    constant_idx: u64,
    variables: HashMap<String, PointerValue<'ctx>>,
    string_constants: HashMap<String, PointerValue<'ctx>>,
    fn_value: Option<FunctionValue<'ctx>>,
}

fn into_basic_value_enum(value: AnyValueEnum) -> Result<BasicValueEnum, String> {
    match value.get_type() {
        AnyTypeEnum::IntType(_) => Ok(value.into_int_value().as_basic_value_enum()),
        AnyTypeEnum::FloatType(_) => Ok(value.into_float_value().as_basic_value_enum()),
        AnyTypeEnum::ArrayType(_) => Ok(value.into_array_value().as_basic_value_enum()),
        AnyTypeEnum::PointerType(_) => Ok(value.into_pointer_value().as_basic_value_enum()),
        _ => Err(format!(
            "No basic value could be constructed from {:?}",
            value
        )),
    }
}

impl<'a, 'ctx> FunctionImplementation<'a, 'ctx> {
    fn compile_function(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        function_declaration: ASTNode,
    ) -> Result<FunctionValue<'ctx>, String> {
        let mut function;

        if let ASTNode::FunctionDeclaration {
            identifier,
            arguments,
            return_type,
            body,
        } = function_declaration
        {
            let builder = context.create_builder();

            function = FunctionImplementation {
                context,
                module,
                builder,

                identifier,
                arguments,
                body,
                return_type,

                constant_idx: 0,
                variables: HashMap::new(),
                string_constants: HashMap::new(),
                fn_value: None,
            }
        } else {
            return Err(format!(
                "Not a valid Function Declaration {:?}",
                function_declaration
            ));
        }
        function.compile()
    }

    fn compile(&mut self) -> Result<FunctionValue<'ctx>, String> {
        let function = self.get_function_prototype()?;
        let entry = self.context.append_basic_block(function, "entry");
        self.fn_value = Some(function);

        self.variables.reserve(self.arguments.len());
        self.builder.position_at_end(entry);

        for (i, arg) in function.get_param_iter().enumerate() {
            let argument_name = self.arguments[i].clone().identifier;
            let alloced = self.allocate_entry_stack_block(argument_name.as_str(), arg.get_type());
            self.builder.build_store(alloced, arg);

            self.variables.insert(argument_name, alloced);
        }

        let body = self.build_function_body();

        if self.return_type == VariableType::Void {
            self.builder.build_return(None);
        }
        Ok(function)
    }

    /// Allocate a block of memory on the stack in the function entry block .
    /// NOTE: Currently only allocates memory of size i64.
    fn allocate_entry_stack_block<T: BasicType<'ctx>>(
        &mut self,
        name: &str,
        ty: T,
    ) -> PointerValue<'ctx> {
        let local_builder = self.context.create_builder();

        let entry = self.fn_value.unwrap().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => local_builder.position_before(&first_instr),
            None => local_builder.position_at_end(entry),
        }

        local_builder.build_alloca(ty, name)
    }

    fn get_type_from_variable_type(
        &self,
        variable_type: &VariableType,
    ) -> Option<BasicTypeEnum<'ctx>> {
        match variable_type {
            VariableType::Integer => Some(self.context.i64_type().as_basic_type_enum()),
            VariableType::Void => None,
            _ => unimplemented!(),
        }
    }

    /// Compile the interface defined by a FunctionDeclaration.
    fn get_function_prototype(&mut self) -> Result<FunctionValue<'ctx>, String> {
        let ret_type = self.get_type_from_variable_type(&self.return_type);

        let i64_type = self.context.i64_type(); // TODO: As above

        let args_types = self
            .arguments
            .iter()
            .map(|a| {
                self.get_type_from_variable_type(&a.argument_type)
                    .expect("Invalid Argument type")
                    .into()
            })
            .collect::<Vec<BasicMetadataTypeEnum>>();
        let args_types = args_types.as_slice();

        let fn_type = match ret_type {
            Some(ret_type) => ret_type.fn_type(args_types, false),
            None => self.context.void_type().fn_type(args_types, false),
        };
        let fn_val = self
            .module
            .add_function(self.identifier.as_str(), fn_type, None);
        self.fn_value = Some(fn_val);

        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.set_name(self.arguments[i].identifier.as_str());
        }
        Ok(fn_val)
    }

    /// Build a binary operation out of the given operands.
    ///
    /// The `left` operand will determine the type of the operation.
    /// The `right` operand needs to be of the same type for operation to work.
    ///
    /// Returns the resulting LLVM MathValue or an error if the types did not match.
    fn build_binary_operation(
        &mut self,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
        operator: Operator,
    ) -> Result<AnyValueEnum<'ctx>, String> {
        let lhs;
        let rhs;
        let op;

        let op_type = left.get_type();

        match op_type {
            BasicTypeEnum::IntType(_) => match right.get_type() {
                BasicTypeEnum::IntType(_) => {
                    lhs = left.into_int_value();
                    rhs = right.into_int_value();

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
                }
                _ => {
                    return Err(format!(
                        "{:?} and {:?} are not of the same type for add",
                        left, right
                    ))
                }
            },
            _ => unimplemented!(),
        }

        return Ok(op.as_any_value_enum());
    }

    fn get_next_constant_name(&mut self) -> String {
        let mut string = String::new();
        string.push_str("__pyro_compiler_constant_");
        string.push_str(self.constant_idx.to_string().as_str());
        self.constant_idx += 1;

        string
    }

    /// Evaluate an expression from the AST and build the LLVM code for it.
    /// Returns the final output type of the expression, or `Err` if no expression
    /// could be parsed.
    // TODO: This should be split up in separate methods to be less of an eyesore.
    fn evaluate_statement(&mut self, statement: ASTNode) -> Result<AnyValueEnum<'ctx>, String> {
        match statement {
            ASTNode::IntegerLiteral(value) => {
                let const_value = self.context.i64_type().const_int(value as u64, false);
                Ok(const_value.as_any_value_enum())
            }

            ASTNode::StringLiteral(string) => {
                if self.string_constants.contains_key(string.as_str()) {
                    let ptr = self.string_constants.get(string.as_str()).unwrap();
                    return Ok(ptr.as_any_value_enum());
                }

                let const_name = self.get_next_constant_name();
                let ptr = self
                    .builder
                    .build_global_string_ptr(&string, const_name.as_str());
                let ptr = ptr.as_pointer_value();
                self.string_constants.insert(string, ptr);
                Ok(ptr.as_any_value_enum())
            }

            ASTNode::Identifier(variable_name) => {
                let variable_ptr = self.variables.get(&variable_name);
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

            ASTNode::LetDeclaration(variable_name, expression) => {
                let value = self.evaluate_statement(*expression)?;
                let value = into_basic_value_enum(value)?;

                let variable =
                    self.allocate_entry_stack_block(variable_name.as_str(), value.get_type());
                self.variables.insert(variable_name, variable);

                self.builder.build_store(variable, value);
                Ok(variable.as_any_value_enum())
            }

            ASTNode::BinaryOp {
                operator,
                left,
                right,
            } => {
                let left = self.evaluate_statement(*left)?;
                let right = self.evaluate_statement(*right)?;

                let lhs = into_basic_value_enum(left)?;
                let rhs = into_basic_value_enum(right)?;

                self.build_binary_operation(lhs, rhs, operator)
            }

            ASTNode::FunctionCall(function_name, arguments) => {
                let function = self.module.get_function(function_name.as_str()).unwrap();

                let arguments = arguments
                    .iter()
                    .map(|a| self.evaluate_statement(a.clone()))
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

            ASTNode::ReturnStatement(expression) => {
                let value = self.evaluate_statement(*expression)?;
                let value = into_basic_value_enum(value)?;

                let return_instruction = self.builder.build_return(Some(&value));
                Ok(return_instruction.as_any_value_enum())
            }
            _ => Err("Not implemented".to_string()),
        }
    }

    fn build_function_body(&mut self) -> Result<(), String> {
        let iterator = self.body.to_vec();
        for statement in iterator {
            let value = self.evaluate_statement(statement);
            if value.is_err() {
                return Err(value.unwrap_err());
            }
        }
        Ok(())
    }
}

pub struct Compiler {
    context: Context,
}

impl<'ctx> Compiler {
    pub fn new() -> Self {
        let context = Context::create();
        Self { context }
    }

    fn get_default_target_machine(&self) -> Result<TargetMachine, String> {
        Target::initialize_all(&InitializationConfig::default());
        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple);

        if target.is_err() {
            return Err(target.unwrap_err().to_string());
        }
        let target = target.unwrap();

        let target_machine = target.create_target_machine(
            &triple,
            "x86-64",
            "+avx2",
            OptimizationLevel::Default,
            RelocMode::Default,
            CodeModel::Default,
        );
        if target_machine.is_none() {
            return Err("Error when creating target machine".to_string());
        }

        Ok(target_machine.unwrap())
    }

    // TODO: Make print capable of handling other than i64.
    fn add_print(
        &'ctx self,
        target_machine: &TargetMachine,
        module: &Module<'ctx>,
    ) -> Result<(), String> {
        let void_type = self.context.void_type();

        let char_ptr = self.context.i8_type().ptr_type(AddressSpace::default());
        let print_type = void_type.fn_type(&[char_ptr.into()], true);

        module.add_function("print", print_type, Some(Linkage::External));

        Ok(())
    }

    pub fn compile(
        &'ctx mut self,
        ast: ASTNode,
        output_path: Option<&Path>,
    ) -> Result<Option<LLVMString>, String> {
        let target_machine = self
            .get_default_target_machine()
            .expect("Error when creating target machine");
        let module = self.context.create_module("name");
        if self.add_print(&target_machine, &module).is_err() {
            return Err("Error when adding print function".to_string());
        }

        match ast {
            ASTNode::Program(functions) => {
                for function_declaration in functions {
                    FunctionImplementation::compile_function(
                        &self.context,
                        &module,
                        function_declaration,
                    )?;
                }

                println!("{:?}", module.verify());
                module.print_to_stderr();

                // TODO: Everything below this should be moved around
                //       create functions and remove debug statements.

                if output_path.is_some() {
                    if let Ok(tempfile) = tempfile::NamedTempFile::new() {
                        target_machine
                            .write_to_file(&module, FileType::Object, tempfile.path())
                            .expect("Could not write module to file");

                        let temppath = tempfile
                            .path()
                            .to_str()
                            .expect("Error creating temporary file");
                        let output_path = output_path.unwrap().to_str().unwrap();

                        // TODO: Make this actually look for library instead of hard coded debug path
                        let out_str = Command::new("clang")
                            .args([
                                "-no-pie",
                                tempfile.path().to_str().unwrap(),
                                "-o",
                                output_path,
                                "target/debug/libpyro_st.so",
                            ])
                            .output()
                            .expect("Error compiling");
                    }
                    return Ok(None);
                } else {
                    return Ok(Some(module.print_to_string()));
                }
            }
            _ => unimplemented!(),
        }
    }
}

// Todo: Write tests
// TODO: Make it more reasonable to compare compiler output to expected output.
#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ASTNode, Argument, VariableType};
}
