extern crate inkwell;
use std::collections::HashMap;

use crate::ast::{ASTNode, Argument, VariableType};
use crate::common_utils::get_type_from_variable_type;
use crate::statement::PyroStatement;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use inkwell::values::{FunctionValue, PointerValue};

pub struct PyroFunction<'a, 'ctx> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    string_globals: &'a mut HashMap<String, PointerValue<'ctx>>,

    identifier: String,
    arguments: Vec<Argument>,
    body: Vec<ASTNode>,
    return_type: VariableType,
    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> PyroFunction<'a, 'ctx> {
    pub fn compile_function(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
        string_globals: &'a mut HashMap<String, PointerValue<'ctx>>,
        function_declaration: ASTNode,
    ) -> Result<FunctionValue<'ctx>, String> {
        if let ASTNode::FunctionDeclaration {
            identifier,
            arguments,
            return_type,
            body,
        } = function_declaration
        {
            let function = Self {
                context,
                module,
                builder,
                string_globals,

                identifier,
                arguments,
                body,
                return_type,
                variables: HashMap::new(),
                fn_value: None,
            };

            function.compile()
        } else {
            Err(format!(
                "{:?} is not a valid function declaration",
                function_declaration
            ))
        }
    }

    /// Generate the LLVM code for the function.
    /// Returns an Err if the function could not be written correctly,
    /// otherwise returns the Inkwell FunctionValue that represents the
    /// finalized function.
    fn compile(mut self) -> Result<FunctionValue<'ctx>, String> {
        let function = self.get_function_prototype()?;
        let entry = self.context.append_basic_block(function, "entry");
        self.fn_value = Some(function);

        self.variables.reserve(self.arguments.len());
        self.builder.position_at_end(entry);
        self.build_argument_stack(&function);

        self.build_function_body()?;

        if self.return_type == VariableType::Void {
            self.builder.build_return(None);
        }

        Ok(function)
    }

    #[inline]
    fn entry_block(&self) -> Option<BasicBlock<'ctx>> {
        self.fn_value.unwrap().get_first_basic_block()
    }

    fn build_argument_stack(&mut self, function: &FunctionValue<'ctx>) {
        for (i, arg) in function.get_param_iter().enumerate() {
            let argument_name = self.arguments[i].clone().identifier;
            let alloced = self.allocate_entry_stack_block(argument_name.as_str(), arg.get_type());
            self.builder.build_store(alloced, arg);

            self.variables.insert(argument_name, alloced);
        }
    }

    fn build_function_body(&mut self) -> Result<(), String> {
        let iterator = self.body.to_vec();
        for statement in iterator {
            PyroStatement::compile_statement(
                self.context,
                self.module,
                self.builder,
                self.entry_block(),
                &mut self.variables,
                self.string_globals,
                statement,
            )?;
        }

        Ok(())
    }

    /// Allocate a block of memory on the stack in the function entry block .
    ///
    /// NOTE: As it currently stands, the function NEEDS to have a basic
    /// block or this function will fail.
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

    /// Compile the interface defined by a FunctionDeclaration.
    fn get_function_prototype(&mut self) -> Result<FunctionValue<'ctx>, String> {
        let ret_type = get_type_from_variable_type(self.context, &self.return_type);
        let args_types = self
            .arguments
            .iter()
            .map(|a| {
                get_type_from_variable_type(self.context, &a.argument_type)
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
}
