extern crate inkwell;

use std::collections::HashMap;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{AnyTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{
    AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValueEnum, PointerValue,
};

use crate::ast::{ASTNode, Operator, VariableType};
use crate::common_utils::{
    generate_constant_name, get_type_from_variable_type, into_basic_value_enum,
};

macro_rules! recursive_statement_compile {
    ($curr_stmt: expr, $eval_stmt: expr) => {
        compile_statement(
            $curr_stmt.context,
            $curr_stmt.module,
            $curr_stmt.builder,
            $curr_stmt.string_globals,
            $curr_stmt.entry_block,
            $curr_stmt.local_variables,
            $eval_stmt,
        )
    };
}

struct PyroStatement<'a, 'ctx> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    string_globals: &'a mut HashMap<String, PointerValue<'ctx>>,

    entry_block: Option<BasicBlock<'ctx>>,
    local_variables: &'a mut HashMap<String, PointerValue<'ctx>>,
    statement: ASTNode,
}

pub(crate) fn compile_statement<'a, 'ctx>(
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    string_globals: &'a mut HashMap<String, PointerValue<'ctx>>,

    entry_block: Option<BasicBlock<'ctx>>,
    local_variables: &'a mut HashMap<String, PointerValue<'ctx>>,
    statement: ASTNode,
) -> Result<AnyValueEnum<'ctx>, String> {
    let mut pyro_statement = PyroStatement {
        context,
        module,
        builder,
        string_globals,

        entry_block,
        local_variables,
        statement,
    };

    compile_statement_llvm(&mut pyro_statement)
}

fn compile_statement_llvm<'a, 'ctx>(
    pyro_statement: &mut PyroStatement<'a, 'ctx>,
) -> Result<AnyValueEnum<'ctx>, String> {
    match &pyro_statement.statement {
        ASTNode::Program(_) => Err(format!("Cannot declare program as statement")),
        ASTNode::ArrayAllocation {
            variable_type: _,
            size: _,
        } => build_array_allocation(pyro_statement),
        ASTNode::FunctionDeclaration {
            identifier: _,
            arguments: _,
            return_type: _,
            body: _,
        } => Err(format!("Cannot declare function as statement")),
        ASTNode::ClassDeclaration {
            identifier: _,
            methods: _,
            fields: _,
        } => Err(format!("Cannot declare class as statement")),
        ASTNode::ObjectAllocation(_) => build_object_allocation(pyro_statement),
        ASTNode::FunctionCall(_, _) => build_function_call(pyro_statement),
        ASTNode::LetDeclaration(_, _)
        | ASTNode::ArrayAssignment(_, _, _)
        | ASTNode::VariableAssignment(_, _) => build_assignment(pyro_statement),
        ASTNode::ReturnStatement(_) => build_return(pyro_statement),
        ASTNode::DestroyVariable(_) => build_memory_deallocation(pyro_statement),
        ASTNode::Identifier(_) | ASTNode::ArrayAccess(_, _) => load_identifier(pyro_statement),
        ASTNode::IntegerLiteral(_)
        | ASTNode::StringLiteral(_)
        | ASTNode::FloatLiteral(_)
        | ASTNode::BooleanLiteral(_) => build_literal(pyro_statement),
        ASTNode::BinaryOp {
            left: _,
            operator: _,
            right: _,
        } => build_binary_operation(pyro_statement),
    }
}

fn build_array_allocation<'a, 'ctx>(
    pyro_statement: &mut PyroStatement<'a, 'ctx>,
) -> Result<AnyValueEnum<'ctx>, String> {
    if let ASTNode::ArrayAllocation {
        variable_type,
        size,
    } = pyro_statement.statement.clone()
    {
        let variable_type = get_type_from_variable_type(pyro_statement.context, &variable_type);
        if variable_type.is_none() {
            return Err(format!("Cannot allocate array of void type"));
        }
        let variable_type = variable_type.unwrap();

        let size = recursive_statement_compile!(pyro_statement, *size.clone())?;

        if !size.is_int_value() {
            return Err(format!("Size is not an integer"));
        }
        let size = size.into_int_value();

        let ptr;
        match variable_type {
            BasicTypeEnum::ArrayType(_) => todo!(),
            BasicTypeEnum::FloatType(_) => {
                ptr = pyro_statement.builder.build_array_malloc(
                    variable_type.into_float_type(),
                    size,
                    "array_malloc",
                );
            }
            BasicTypeEnum::IntType(_) => {
                ptr = pyro_statement.builder.build_array_malloc(
                    variable_type.into_int_type(),
                    size,
                    "array_malloc",
                );
            }
            BasicTypeEnum::PointerType(_) => {
                ptr = pyro_statement.builder.build_array_malloc(
                    variable_type.into_pointer_type(),
                    size,
                    "array_malloc",
                );
            }
            BasicTypeEnum::StructType(_) => todo!(),
            BasicTypeEnum::VectorType(_) => todo!(),
        }

        if ptr.is_err() {
            return Err(format!("Could not allocate memory"));
        }

        return Ok(ptr.unwrap().as_any_value_enum());
    }

    Err(format!("Not a memory allocation"))
}

fn build_object_allocation<'a, 'ctx>(
    pyro_statement: &mut PyroStatement<'a, 'ctx>,
) -> Result<AnyValueEnum<'ctx>, String> {
    if let ASTNode::ObjectAllocation(VariableType::Class(class_name)) =
        pyro_statement.statement.clone()
    {
        let struct_type = pyro_statement.context.get_struct_type(&class_name);

        if struct_type.is_none() {
            return Err(format!("Cannot find class of type `{}`", class_name));
        }
        let struct_type = struct_type.unwrap();

        let ptr = pyro_statement
            .builder
            .build_malloc(struct_type, format!("{}_malloc", class_name).as_str());

        if ptr.is_err() {
            return Err(format!("Could not allocate memory"));
        }
        return Ok(ptr.unwrap().as_any_value_enum());
    }

    Err(format!("Not a valid object allocation"))
}

fn build_function_call<'a, 'ctx>(
    pyro_statement: &mut PyroStatement<'a, 'ctx>,
) -> Result<AnyValueEnum<'ctx>, String> {
    match &pyro_statement.statement {
        ASTNode::FunctionCall(function_name, arguments) => {
            let function = pyro_statement.module.get_function(function_name.as_str());
            if function.is_none() {
                return Err(format!("Could not find function `{}`", function_name));
            }

            let function = function.unwrap();
            let arguments = arguments
                .iter()
                .map(|a| recursive_statement_compile!(pyro_statement, a.clone()))
                .collect::<Result<Vec<AnyValueEnum>, String>>()?;

            let arguments = arguments
                .iter()
                .map(|a| BasicMetadataValueEnum::try_from(*a))
                .collect::<Result<Vec<BasicMetadataValueEnum>, _>>();

            if arguments.is_err() {
                return Err("Error when parsing arguments for function call".to_string());
            }

            let arguments = arguments.unwrap();
            let call = pyro_statement
                .builder
                .build_call(function, arguments.as_slice(), "call");

            Ok(call.as_any_value_enum())
        }

        _ => Err(format!("Not valid Function Call")),
    }
}

fn allocate_stack_variable<'a, 'ctx, T: BasicType<'ctx>>(
    pyro_statement: &mut PyroStatement<'a, 'ctx>,
    name: &str,
    ty: T,
) -> Result<PointerValue<'ctx>, String> {
    if pyro_statement.entry_block.is_some() {
        let local_builder = pyro_statement.context.create_builder();
        let entry_block = pyro_statement.entry_block.unwrap();

        match entry_block.get_first_instruction() {
            Some(first_instruction) => local_builder.position_before(&first_instruction),
            None => local_builder.position_at_end(entry_block),
        }

        return Ok(local_builder.build_alloca(ty, name));
    }

    Err(format!("Let statement without an entry block"))
}

fn build_assignment<'a, 'ctx>(
    pyro_statement: &mut PyroStatement<'a, 'ctx>,
) -> Result<AnyValueEnum<'ctx>, String> {
    match pyro_statement.statement.clone() {
        ASTNode::LetDeclaration(variable_name, expression) => {
            let value = recursive_statement_compile!(pyro_statement, *expression.clone())?;
            let value = into_basic_value_enum(value)?;

            let variable =
                allocate_stack_variable(pyro_statement, variable_name.as_str(), value.get_type())?;
            pyro_statement
                .local_variables
                .insert(variable_name.to_string(), variable);

            pyro_statement.builder.build_store(variable, value);
            Ok(variable.as_any_value_enum())
        }
        ASTNode::VariableAssignment(variable_name, expression) => {
            let value = recursive_statement_compile!(pyro_statement, *expression.clone())?;
            let value = into_basic_value_enum(value)?;

            let variable = pyro_statement.local_variables.get(&variable_name);
            if variable.is_none() {
                return Err(format!("No variable named {}", variable_name));
            }
            let variable = *variable.unwrap();
            pyro_statement.builder.build_store(variable, value);

            Ok(variable.as_any_value_enum())
        }
        ASTNode::ArrayAssignment(variable_name, index, expression) => {
            let ptr = pyro_statement.local_variables.get(&variable_name);
            if ptr.is_none() {
                return Err(format!("{} is not an accessible variable", variable_name));
            }
            let ptr = *ptr.unwrap();
            let ptr = pyro_statement
                .builder
                .build_load(ptr, &variable_name)
                .into_pointer_value();

            let index = recursive_statement_compile!(pyro_statement, *index.clone())?;
            let value = recursive_statement_compile!(pyro_statement, *expression.clone())?;

            let assign_ptr;
            let instruction;

            unsafe {
                assign_ptr = pyro_statement.builder.build_in_bounds_gep(
                    ptr,
                    &[index.into_int_value()],
                    format!("{}_gep_assign", variable_name).as_str(),
                );

                match value.get_type() {
                    AnyTypeEnum::ArrayType(_) => todo!(),
                    AnyTypeEnum::FloatType(_) => {
                        instruction = pyro_statement
                            .builder
                            .build_store(assign_ptr, value.into_float_value());
                    }
                    AnyTypeEnum::FunctionType(_) => todo!(),
                    AnyTypeEnum::IntType(_) => {
                        instruction = pyro_statement
                            .builder
                            .build_store(assign_ptr, value.into_int_value());
                    }
                    AnyTypeEnum::PointerType(_) => {
                        instruction = pyro_statement
                            .builder
                            .build_store(assign_ptr, value.into_pointer_value());
                    }
                    AnyTypeEnum::StructType(_) => todo!(),
                    AnyTypeEnum::VectorType(_) => todo!(),
                    AnyTypeEnum::VoidType(_) => todo!(),
                }
            }

            Ok(instruction.as_any_value_enum())
        }
        _ => Err(format!("Not a valid Let statement")),
    }
}

fn build_return<'a, 'ctx>(
    pyro_statement: &mut PyroStatement<'a, 'ctx>,
) -> Result<AnyValueEnum<'ctx>, String> {
    match pyro_statement.statement.clone() {
        ASTNode::ReturnStatement(expression) => {
            let value = recursive_statement_compile!(pyro_statement, *expression.clone())?;
            let value = into_basic_value_enum(value)?;

            let return_instruction = pyro_statement.builder.build_return(Some(&value));
            Ok(return_instruction.as_any_value_enum())
        }
        _ => Err("Not implemented".to_string()),
    }
}

fn load_identifier<'a, 'ctx>(
    pyro_statement: &mut PyroStatement<'a, 'ctx>,
) -> Result<AnyValueEnum<'ctx>, String> {
    match pyro_statement.statement.clone() {
        ASTNode::Identifier(variable_name) => {
            let variable_ptr = pyro_statement.local_variables.get(&variable_name);
            if variable_ptr.is_none() {
                return Err(format!(
                    "Variable {} can not be found in scope.",
                    variable_name
                ));
            }
            let variable_ptr = *variable_ptr.unwrap();
            let variable = pyro_statement
                .builder
                .build_load(variable_ptr, &variable_name);
            Ok(variable.as_any_value_enum())
        }
        ASTNode::ArrayAccess(variable_name, index) => {
            let ptr = pyro_statement.local_variables.get(&variable_name);
            if ptr.is_none() {
                return Err(format!("{} is not an accessible variable", variable_name));
            }
            let ptr = *ptr.unwrap();
            let ptr = pyro_statement
                .builder
                .build_load(ptr, &variable_name)
                .into_pointer_value();
            let index = recursive_statement_compile!(pyro_statement, *index.clone())?;

            let load_ptr;
            unsafe {
                load_ptr = pyro_statement.builder.build_gep(
                    ptr,
                    &[index.into_int_value()],
                    format!("{}_gep_load", variable_name).as_str(),
                );
            }
            let variable = pyro_statement.builder.build_load(load_ptr, &variable_name);
            Ok(variable.as_any_value_enum())
        }
        _ => Err(format!("Not an identifier statement")),
    }
}

fn build_literal<'a, 'ctx>(
    pyro_statement: &mut PyroStatement<'a, 'ctx>,
) -> Result<AnyValueEnum<'ctx>, String> {
    match pyro_statement.statement.clone() {
        ASTNode::IntegerLiteral(value) => {
            let const_value = pyro_statement
                .context
                .i64_type()
                .const_int(value as u64, true);
            Ok(const_value.as_any_value_enum())
        }

        ASTNode::FloatLiteral(value) => {
            let const_value = pyro_statement.context.f64_type().const_float(value as f64);
            Ok(const_value.as_any_value_enum())
        }

        ASTNode::BooleanLiteral(value) => {
            let const_value = pyro_statement
                .context
                .bool_type()
                .const_int(value as u64, false);
            Ok(const_value.as_any_value_enum())
        }

        ASTNode::StringLiteral(string) => {
            if pyro_statement.string_globals.contains_key(string.as_str()) {
                let ptr = pyro_statement.string_globals.get(string.as_str()).unwrap();
                return Ok(ptr.as_any_value_enum());
            }

            let mut const_name;
            loop {
                const_name = generate_constant_name();
                if !pyro_statement.string_globals.contains_key(&const_name) {
                    break;
                }
            }
            let ptr = pyro_statement
                .builder
                .build_global_string_ptr(&string, const_name.as_str());
            let ptr = ptr.as_pointer_value();
            pyro_statement.string_globals.insert(string, ptr);
            Ok(ptr.as_any_value_enum())
        }

        _ => Err(format!("{:?} is not a literal", pyro_statement.statement)),
    }
}

fn build_binary_operation<'a, 'ctx>(
    pyro_statement: &mut PyroStatement<'a, 'ctx>,
) -> Result<AnyValueEnum<'ctx>, String> {
    match &pyro_statement.statement {
        ASTNode::BinaryOp {
            left,
            operator,
            right,
        } => {
            let lhs = recursive_statement_compile!(pyro_statement, *left.clone())?;
            let rhs = recursive_statement_compile!(pyro_statement, *right.clone())?;

            let lhs = into_basic_value_enum(lhs)?;
            let rhs = into_basic_value_enum(rhs)?;

            binary_op_construction(pyro_statement, lhs, rhs, operator.clone())
        }

        _ => Err(format!("Not a valid binary operation")),
    }
}

/// Build a binary operation out of the given operands.
///
/// The `left` operand will determine the type of the operation.
/// The `right` operand needs to be of the same type for operation to work.
///
/// Returns the resulting LLVM MathValue or an error if the types did not
/// match.
fn binary_op_construction<'a, 'ctx>(
    pyro_statement: &mut PyroStatement<'a, 'ctx>,
    left: BasicValueEnum<'ctx>,
    right: BasicValueEnum<'ctx>,
    operator: Operator,
) -> Result<AnyValueEnum<'ctx>, String> {
    let op_type = left.get_type();
    if op_type != right.get_type() {
        return Err(format!("{:?} is not the same type as {:?}", left, right));
    }

    match op_type {
        BasicTypeEnum::IntType(_) => {
            let op;
            let lhs = left.into_int_value();
            let rhs = right.into_int_value();

            if lhs.get_type().get_bit_width() == 1 || rhs.get_type().get_bit_width() == 1 {
                return Err(format!("Cannot perform binary operation on a bool"));
            }

            if lhs.get_type().get_bit_width() != rhs.get_type().get_bit_width() {
                return Err(format!(
                    "Cannot perform binary operation on ints of varying bit widths."
                ));
            }

            match operator {
                Operator::Plus => {
                    op = pyro_statement
                        .builder
                        .build_int_add(lhs, rhs, "intaddition")
                }
                Operator::Minus => {
                    op = pyro_statement
                        .builder
                        .build_int_sub(lhs, rhs, "intsubtraction")
                }
                Operator::Multiplication => {
                    op = pyro_statement
                        .builder
                        .build_int_mul(lhs, rhs, "intmultiplication")
                }
                Operator::Division => {
                    op = pyro_statement
                        .builder
                        .build_int_signed_div(lhs, rhs, "intdivision")
                }
            }
            return Ok(op.as_any_value_enum());
        }
        BasicTypeEnum::FloatType(_) => {
            let op;
            let lhs = left.into_float_value();
            let rhs = right.into_float_value();

            match operator {
                Operator::Plus => {
                    op = pyro_statement
                        .builder
                        .build_float_add(lhs, rhs, "floataddition")
                }
                Operator::Minus => {
                    op = pyro_statement
                        .builder
                        .build_float_sub(lhs, rhs, "floatsubtraction")
                }
                Operator::Multiplication => {
                    op = pyro_statement
                        .builder
                        .build_float_mul(lhs, rhs, "floatmultiplication")
                }
                Operator::Division => {
                    op = pyro_statement
                        .builder
                        .build_float_div(lhs, rhs, "floatdivision")
                }
            }
            return Ok(op.as_any_value_enum());
        }
        _ => return Err(format!("Not a valid operand type")),
    }
}

fn build_memory_deallocation<'a, 'ctx>(
    pyro_statement: &mut PyroStatement<'a, 'ctx>,
) -> Result<AnyValueEnum<'ctx>, String> {
    if let ASTNode::DestroyVariable(variable_name) = pyro_statement.statement.clone() {
        let variable_ptr = pyro_statement.local_variables.get(&variable_name);

        if variable_ptr.is_none() {
            return Err(format!("Variable `{}` not found", variable_name));
        }
        let variable_ptr = *variable_ptr.unwrap();
        let variable_ptr = pyro_statement.builder.build_load(variable_ptr, "load");

        if !variable_ptr.is_pointer_value() {
            return Err(format!(
                "Variable `{}` is not a destroyable variable",
                variable_name
            ));
        }
        let variable_ptr = variable_ptr.into_pointer_value();
        let free_instruction = pyro_statement.builder.build_free(variable_ptr);
        pyro_statement.local_variables.remove(&variable_name);

        return Ok(free_instruction.as_any_value_enum());
    }

    Err(format!("Not a valid delete"))
}
