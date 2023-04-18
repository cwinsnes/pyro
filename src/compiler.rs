extern crate inkwell;

use std::collections::HashMap;
use std::env::var;
use std::fmt::Pointer;
use std::hash::Hash;
use std::path::Path;
use std::process::Command;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::support::LLVMString;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{
    AnyValue, AnyValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue, BasicMetadataValueEnum,
};
use inkwell::{execution_engine, AddressSpace, OptimizationLevel};

use crate::ast::{ASTNode, Argument, VariableType};

struct FunctionImplementation<'a, 'ctx> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: Builder<'ctx>,

    name: String,
    arguments: Vec<Argument>,
    body: Vec<ASTNode>,
    return_type: VariableType,

    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> FunctionImplementation<'a, 'ctx> {
    fn compile_function(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        function_declaration: ASTNode,
    ) -> Result<FunctionValue<'ctx>, String> {
        let mut function;

        if let ASTNode::FunctionDeclaration(name, arguments, return_type, body) =
            function_declaration
        {
            let builder = context.create_builder();

            function = FunctionImplementation {
                context,
                module,
                builder,

                name,
                arguments,
                body,
                return_type,

                variables: HashMap::new(),
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
            let alloced = self.allocate_entry_stack_block(argument_name.as_str());
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
    fn allocate_entry_stack_block(&mut self, name: &str) -> PointerValue<'ctx> {
        let local_builder = self.context.create_builder();

        let entry = self.fn_value.unwrap().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => local_builder.position_before(&first_instr),
            None => local_builder.position_at_end(entry),
        }

        local_builder.build_alloca(self.context.i64_type(), name)
    }

    fn get_type_from_variable_type(
        &self,
        variable_type: &VariableType,
    ) -> Option<BasicTypeEnum<'ctx>> {
        match variable_type {
            VariableType::Number => Some(self.context.i64_type().as_basic_type_enum()),
            VariableType::Void => None,
            _ => unimplemented!(),
        }
    }

    /// Compile the interface defined by a FunctionDeclaration.
    fn get_function_prototype(&mut self) -> Result<FunctionValue<'ctx>, String> {
        let ret_type = self.get_type_from_variable_type(&self.return_type);

        let number_type = self.context.i64_type(); // TODO: As above

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
        let fn_val = self.module.add_function(self.name.as_str(), fn_type, None);
        self.fn_value = Some(fn_val);

        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.set_name(self.arguments[i].identifier.as_str());
        }
        Ok(fn_val)
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
                let variable = self.allocate_entry_stack_block(variable_name.as_str());
                let value = self.evaluate_statement(*expression)?;

                self.variables.insert(variable_name, variable);

                self.builder.build_store(variable, value.into_int_value());
                Ok(variable.as_any_value_enum())
            }

            ASTNode::FunctionCall(function_name, arguments) => {
                let function = self.module.get_function(function_name.as_str()).unwrap();

                let arguments = arguments
                    .iter()
                    .map(|a| self.evaluate_statement(a.clone()))
                    .collect::<Result<Vec<AnyValueEnum>, String>>();

                if arguments.is_err() {
                    return Err("Error while parsing function arguments".to_string());
                }

                let arguments = arguments
                    .unwrap()
                    .iter()
                    .map(|a| BasicMetadataValueEnum::try_from(*a))
                    .collect::<Result<Vec<BasicMetadataValueEnum>, _>>();
                
                if arguments.is_err() {
                    return Err("Error while parsing function arguments".to_string());
                }
                let arguments = arguments.unwrap();

                let call = self.builder.build_call(function, arguments.as_slice(), "call");
                Ok(call.as_any_value_enum())
            }

            ASTNode::ReturnStatement(expression) => {
                let value = self.evaluate_statement(*expression)?;
                let value = BasicValueEnum::try_from(value);
                match value {
                    Ok(value) => {
                        let return_instruction = self.builder.build_return(Some(&value));
                        Ok(return_instruction.as_any_value_enum())
                    }
                    Err(_) => Err("Invalid return expression".to_string()),
                }
            }
            _ => Err("Not implemented".to_string()),
        }
    }

    fn build_function_body(&mut self) -> Result<(), String> {
        let iterator = self.body.to_vec();
        for statement in iterator {
            let value = self.evaluate_statement(statement)?;
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

    pub fn compile(
        &'ctx mut self,
        ast: ASTNode,
        output_path: Option<&Path>,
    ) -> Result<Option<LLVMString>, String> {
        let module = self.context.create_module("name");

        match ast {
            ASTNode::Program(functions) => {
                for function_declaration in functions {
                    FunctionImplementation::compile_function(
                        &self.context,
                        &module,
                        function_declaration,
                    )?;
                }

                // TODO: Everything below this should be moved around
                //       create functions and remove debug statements.

                if output_path.is_some() {
                    Target::initialize_all(&InitializationConfig::default());
                    let triple = TargetMachine::get_default_triple();
                    let target = Target::from_triple(&triple)
                        .expect("couldn't create target from target triple");

                    let target_machine = target
                        .create_target_machine(
                            &triple,
                            "x86-64",
                            "+avx2",
                            OptimizationLevel::Default,
                            RelocMode::Default,
                            CodeModel::Default,
                        )
                        .expect("Could not create target machine");

                    if let Ok(tempfile) = tempfile::NamedTempFile::new() {
                        target_machine
                            .write_to_file(&module, FileType::Object, tempfile.path())
                            .expect("Could not write module to file");

                        let temppath = tempfile.path().to_str().unwrap();
                        println!("{}", temppath);
                        let output_path = output_path.unwrap().to_str().unwrap();

                        Command::new("clang")
                            .args([temppath, "-o", output_path])
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


// Todo: Write more comprehensive tests
#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ASTNode, Argument, VariableType};

    #[test]
    fn test_compilation_of_single_function() {
        let mut compiler = Compiler::new();

        let ast = ASTNode::Program(vec![ASTNode::FunctionDeclaration(
            "test".to_string(),
            vec![Argument {
                identifier: "x".to_string(),
                argument_type: VariableType::Number,
            }],
            VariableType::Number,
            vec![ASTNode::ReturnStatement(Box::new(ASTNode::Identifier(
                "x".to_string(),
            )))],
        )]);

        let compiler_out = compiler.compile(ast, None);
        assert!(compiler_out.is_ok());
        let compiler_out = compiler_out.unwrap();
        assert!(compiler_out.is_some());
        let compiler_out = compiler_out.unwrap().to_string();
        assert_eq!(compiler_out, "; ModuleID = 'name'\nsource_filename = \"name\"\n\ndefine i64 @test(i64 %x) {\nentry:\n  %x1 = alloca i64, align 8\n  store i64 %x, i64* %x1, align 4\n  %x2 = load i64, i64* %x1, align 4\n  ret i64 %x2\n}\n");
    }

    fn test_compilation_of_multiple_functions() {
        let mut compiler = Compiler::new();

        let ast = ASTNode::Program(vec![
            ASTNode::FunctionDeclaration(
                "test".to_string(),
                vec![Argument {
                    identifier: "x".to_string(),
                    argument_type: VariableType::Number,
                }],
                VariableType::Number,
                vec![ASTNode::ReturnStatement(Box::new(ASTNode::Identifier(
                    "x".to_string(),
                )))],
            ),
            ASTNode::FunctionDeclaration(
                "test2".to_string(),
                vec![Argument {
                    identifier: "x".to_string(),
                    argument_type: VariableType::Number,
                }],
                VariableType::Number,
                vec![ASTNode::ReturnStatement(Box::new(ASTNode::Identifier(
                    "x".to_string(),
                )))],
            ),
        ]);

        let compiler_out = compiler.compile(ast, None);
        assert!(compiler_out.is_ok());
        let compiler_out = compiler_out.unwrap();
        assert!(compiler_out.is_some());
        let compiler_out = compiler_out.unwrap().to_string();
        assert_eq!(compiler_out, "; ModuleID = 'name'\nsource_filename = \"name\"\n\ndefine i64 @test(i64 %x) {\nentry:\n  %x1 = alloca i64, align 8\n  store i64 %x, i64* %x1, align 4\n  %x2 = load i64, i64* %x1, align 4\n  ret i64 %x2\n}\n\ndefine i64 @test2(i64 %x) {\nentry:\n  %x1 = alloca i64, align 8\n  store i64 %x, i64* %x1, align 4\n  %x2 = load i64, i64* %x1, align 4\n  ret i64 %x2\n}\n");
    }
}
    

