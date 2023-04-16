extern crate inkwell;

use std::collections::HashMap;
use std::fmt::Pointer;
use std::hash::Hash;
use std::path::Path;

use inkwell::{AddressSpace, execution_engine};
use inkwell::context::Context;
use inkwell::module::{Module, Linkage};
use inkwell::support::LLVMString;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{FunctionValue, PointerValue, AnyValueEnum, IntValue, AnyValue};
use inkwell::{builder::Builder, types::BasicTypeEnum};

use crate::ast::{ASTNode, Argument, VariableType};

struct FunctionImplementation<'a, 'ctx> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: Builder<'ctx>,

    name: String,
    arguments: Vec<Argument>,
    body: Vec<ASTNode>,

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

        if let ASTNode::FunctionDeclaration(name, arguments, body) = function_declaration {
            let builder = context.create_builder();

            function = FunctionImplementation {
                context,
                module,
                builder,

                name,
                arguments,
                body,

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
        }

        let body = self.build_function_body();
        self.builder.build_return(None); // TODO: Change this when adding return types to functions
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

    /// Compile the interface defined by a FunctionDeclaration.
    fn get_function_prototype(&mut self) -> Result<FunctionValue<'ctx>, String> {
        let ret_type = self.context.void_type(); // TODO: Implement other types as well
        let number_type = self.context.i64_type(); // TODO: As above

        let args_types = self
            .arguments
            .iter()
            .map(|a| number_type.clone().into())
            .collect::<Vec<BasicMetadataTypeEnum>>();
        let args_types = args_types.as_slice();

        let fn_type = ret_type.fn_type(args_types, false);
        let fn_val = self.module.add_function(self.name.as_str(), fn_type, None);

        self.fn_value = Some(fn_val);

        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.set_name(self.arguments[i].identifier.as_str());
        }
        Ok(fn_val)
    }

    fn evaluate_statement(&mut self, statement: ASTNode) -> Result<AnyValueEnum<'ctx>, String> {
        match statement {
            ASTNode::IntegerLiteral(value) => {
                let const_value = self.context.i64_type().const_int(value as u64, false);
                Ok(const_value.as_any_value_enum())
            },
            ASTNode::LetDeclaration(variable_name, expression) => {
                let variable = self.allocate_entry_stack_block(variable_name.as_str());
                let value = self.evaluate_statement(*expression)?;

                self.builder.build_store(variable, value.into_int_value());
                Ok(AnyValueEnum::from(variable))
            },
            _ => Err("Not implemented".to_string())
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
        Self {
            context,
        }
    }

    pub fn compile(&'ctx mut self, ast: ASTNode, output_path: &Path) -> Result<(), String> {
        let module = self.context.create_module("name");

        match ast {
            ASTNode::Program(functions) => {
                for function_declaration in functions {
                    FunctionImplementation::compile_function(&self.context, &module, function_declaration)?;
                }

                module.write_bitcode_to_path(output_path);
            },
            _ => unimplemented!(),
        }
        Ok(())
    }
}
