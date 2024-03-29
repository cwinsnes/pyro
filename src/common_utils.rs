extern crate inkwell;
use inkwell::context::Context;
use inkwell::types::{AnyTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{AnyValueEnum, BasicValue, BasicValueEnum};
use inkwell::AddressSpace;
use rand::distributions::Alphanumeric;
use rand::Rng;

use crate::ast::VariableType;

pub(crate) fn get_type_from_variable_type<'ctx>(
    context: &'ctx Context,
    variable_type: &VariableType,
) -> Option<BasicTypeEnum<'ctx>> {
    match variable_type {
        VariableType::Integer => Some(context.i64_type().as_basic_type_enum()),
        VariableType::Float => Some(context.f64_type().as_basic_type_enum()),
        VariableType::Boolean => Some(context.bool_type().as_basic_type_enum()),
        VariableType::String => Some(
            context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum(),
        ),
        VariableType::Class(identifier) => {
            let return_type = context.get_struct_type(&identifier.as_str());
            if return_type.is_some() {
                return Some(return_type.unwrap().as_basic_type_enum());
            }
            // TODO: Fix this so that an invalid identifier gives Err
            // rather than same as Void type.
            None
        }
        VariableType::Void => None,
    }
}

pub(crate) fn generate_constant_name() -> String {
    let s: String = rand::thread_rng()
        .sample_iter(&Alphanumeric)
        .take(15)
        .map(char::from)
        .collect();

    return format!("__pyro_compiler_constant_{}", s);
}

/// Turn an AnyValueEnum into a BasicValueEnum if possible.
///
/// If there is no relevant BasicValueEnum for the given AnyValueEnum,
/// an Err is returned.
///
/// # Note
/// At the current moment, this functions is not implemented for all
/// possible BasicValueEnums.
pub(crate) fn into_basic_value_enum(value: AnyValueEnum) -> Result<BasicValueEnum, String> {
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
