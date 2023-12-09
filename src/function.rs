extern crate inkwell;
use std::collections::HashMap;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use inkwell::values::{FunctionValue, PointerValue};

use crate::ast::{ASTNode, Argument, VariableType};
use crate::common_utils::get_type_from_variable_type;
use crate::statement;

struct PyroFunction<'a, 'ctx> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    string_globals: &'a mut HashMap<String, PointerValue<'ctx>>,
    class_fields: &'a mut HashMap<String, HashMap<String, u32>>,

    identifier: String,
    arguments: Vec<Argument>,
    body: Vec<ASTNode>,
    return_type: VariableType,
    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value: Option<FunctionValue<'ctx>>,
}

pub(crate) fn compile_function<'a, 'ctx>(
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    string_globals: &'a mut HashMap<String, PointerValue<'ctx>>,
    class_fields: &'a mut HashMap<String, HashMap<String, u32>>,

    function_declaration: ASTNode,
) -> Result<FunctionValue<'ctx>, String> {
    if let ASTNode::FunctionDeclaration {
        identifier,
        arguments,
        return_type,
        body,
    } = function_declaration
    {
        let mut function = PyroFunction {
            context,
            module,
            builder,
            string_globals,
            class_fields,

            identifier,
            arguments,
            body,
            return_type,
            variables: HashMap::new(),
            fn_value: None,
        };

        compile_function_llvm(&mut function)
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
fn compile_function_llvm<'a, 'ctx>(
    function: &mut PyroFunction<'a, 'ctx>,
) -> Result<FunctionValue<'ctx>, String> {
    let function_value = get_function_prototype(function)?;
    let entry = function.context.append_basic_block(function_value, "entry");
    function.fn_value = Some(function_value);

    function.variables.reserve(function.arguments.len());
    function.builder.position_at_end(entry);
    build_argument_stack(function, &function_value);

    build_function_body(function)?;

    if function.return_type == VariableType::Void {
        function.builder.build_return(None);
    }

    Ok(function_value)
}

#[inline]
fn entry_block<'a, 'ctx>(function: &PyroFunction<'a, 'ctx>) -> Option<BasicBlock<'ctx>> {
    function.fn_value.unwrap().get_first_basic_block()
}

fn build_argument_stack<'a, 'ctx>(
    function: &mut PyroFunction<'a, 'ctx>,
    function_value: &FunctionValue<'ctx>,
) {
    for (i, arg) in function_value.get_param_iter().enumerate() {
        let argument_name = function.arguments[i].clone().identifier;
        let alloced = allocate_entry_stack_block(function, argument_name.as_str(), arg.get_type());
        function.builder.build_store(alloced, arg);

        function.variables.insert(argument_name, alloced);
    }
}

fn build_function_body<'a, 'ctx>(function: &mut PyroFunction<'a, 'ctx>) -> Result<(), String> {
    let iterator = function.body.to_vec();
    for pyro_statement in iterator {
        statement::compile_statement(
            function.context,
            function.module,
            function.builder,
            function.string_globals,
            function.class_fields,
            entry_block(function),
            &mut function.variables,
            pyro_statement,
        )?;
    }

    Ok(())
}

/// Allocate a block of memory on the stack in the function entry block .
///
/// NOTE: As it currently stands, the function NEEDS to have a basic
/// block or this function will fail.
fn allocate_entry_stack_block<'a, 'ctx, T: BasicType<'ctx>>(
    function: &mut PyroFunction<'a, 'ctx>,
    name: &str,
    ty: T,
) -> PointerValue<'ctx> {
    let local_builder = function.context.create_builder();
    let entry = function.fn_value.unwrap().get_first_basic_block().unwrap();

    match entry.get_first_instruction() {
        Some(first_instr) => local_builder.position_before(&first_instr),
        None => local_builder.position_at_end(entry),
    }
    local_builder.build_alloca(ty, name)
}

/// Compile the interface defined by a FunctionDeclaration.
fn get_function_prototype<'a, 'ctx>(
    function: &mut PyroFunction<'a, 'ctx>,
) -> Result<FunctionValue<'ctx>, String> {
    let ret_type = get_type_from_variable_type(function.context, &function.return_type);
    let args_types = function
        .arguments
        .iter()
        .map(|a| {
            get_type_from_variable_type(function.context, &a.variable_type)
                .expect("Invalid Argument type")
                .into()
        })
        .collect::<Vec<BasicMetadataTypeEnum>>();
    let args_types = args_types.as_slice();

    let fn_type = match ret_type {
        Some(ret_type) => ret_type.fn_type(args_types, false),
        None => function.context.void_type().fn_type(args_types, false),
    };
    let fn_val = function
        .module
        .add_function(function.identifier.as_str(), fn_type, None);
    function.fn_value = Some(fn_val);

    for (i, arg) in fn_val.get_param_iter().enumerate() {
        arg.set_name(function.arguments[i].identifier.as_str());
    }
    Ok(fn_val)
}
