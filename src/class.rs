use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicTypeEnum, StructType};
use inkwell::values::PointerValue;

use crate::ast::{ASTNode, Variable};
use crate::common_utils::get_type_from_variable_type;

struct PyroClass<'a, 'ctx> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    string_globals: &'a mut HashMap<String, PointerValue<'ctx>>,

    identifier: String,
    fields: Vec<Variable>,
}

pub(crate) fn define_class<'a, 'ctx>(
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    string_globals: &'a mut HashMap<String, PointerValue<'ctx>>,
    class_declaration: ASTNode,
) -> Result<StructType<'ctx>, String> {
    if let ASTNode::ClassDeclaration {
        fields,
        methods,
        identifier,
    } = class_declaration
    {
        let mut class = PyroClass {
            context,
            module,
            builder,
            string_globals,

            identifier,
            fields,
        };

        return declare_class(&mut class);
    }

    Err(format!(
        "Invalid class declaration: {:?}",
        class_declaration
    ))
}

/// Use the compiler Module to declare the class as a named LLVM struct
/// with
fn declare_class<'a, 'ctx>(class: &mut PyroClass<'a, 'ctx>) -> Result<StructType<'ctx>, String> {
    let struct_type = class.context.opaque_struct_type(&class.identifier);

    let field_types = class
        .fields
        .iter()
        .map(|field| {
            get_type_from_variable_type(class.context, &field.variable_type)
                .expect("Not a valid type for a field")
        })
        .collect::<Vec<BasicTypeEnum>>();
    let field_types = field_types.as_slice();
    struct_type.set_body(&field_types, false);

    return Ok(struct_type);
}
