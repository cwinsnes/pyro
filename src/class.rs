use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicTypeEnum, StructType};
use inkwell::values::PointerValue;

use crate::ast::{ASTNode, Variable};
use crate::common_utils::get_type_from_variable_type;
use crate::compiler::Compiler;

pub(crate) struct PyroClass<'a, 'ctx> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    string_globals: &'a mut HashMap<String, PointerValue<'ctx>>,

    identifier: String,
    fields: Vec<Variable>,
}

impl<'a, 'ctx> PyroClass<'a, 'ctx> {
    pub(crate) fn define_class(
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
            let mut class = Self {
                context,
                module,
                builder,
                string_globals,

                identifier,
                fields,
            };

            return class.declare_class();
        }

        Err(format!(
            "Invalid class declaration: {:?}",
            class_declaration
        ))
    }

    /// Use the compiler Module to declare the class as a named LLVM struct
    /// with
    fn declare_class(&mut self) -> Result<StructType<'ctx>, String> {
        let struct_type = self.context.opaque_struct_type(&self.identifier);

        let field_types = self
            .fields
            .iter()
            .map(|field| {
                get_type_from_variable_type(self.context, &field.variable_type)
                    .expect("Not a valid type for a field")
            })
            .collect::<Vec<BasicTypeEnum>>();
        let field_types = field_types.as_slice();
        struct_type.set_body(&field_types, false);

        return Ok(struct_type);
    }
}
