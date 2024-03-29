extern crate inkwell;

use std::collections::HashMap;
use std::path::Path;
use std::process::Command;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::support::LLVMString;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::values::PointerValue;
use inkwell::{AddressSpace, OptimizationLevel};

use crate::ast::ASTNode;
use crate::{class, function};

/// Compiler for the Pyro programming language.
pub(crate) struct Compiler<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

    string_constants: HashMap<String, PointerValue<'ctx>>,
    class_fields: HashMap<String, HashMap<String, u32>>,
}

impl<'ctx> Compiler<'ctx> {
    /// Create a new compiler using the provided Inkwell LLVM context.
    ///
    /// The LLVM module used throughout compilation will use the name
    /// `module_name`.
    pub(crate) fn new(module_name: String, context: &'ctx Context) -> Self {
        Self {
            context,
            module: context.create_module(&module_name),
            builder: context.create_builder(),

            string_constants: HashMap::new(),
            class_fields: HashMap::new(),
        }
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

    fn add_print(&self) {
        let void_type = self.context.void_type();
        let char_ptr = self.context.i8_type().ptr_type(AddressSpace::default());
        let print_type = void_type.fn_type(&[char_ptr.into()], true);
        self.module
            .add_function("print", print_type, Some(Linkage::External));
    }

    pub(crate) fn compile(
        mut self,
        ast: ASTNode,
        output_path: Option<&Path>,
    ) -> Result<Option<LLVMString>, String> {
        let target_machine = self
            .get_default_target_machine()
            .expect("Error when creating target machine");

        self.add_print();

        match ast {
            ASTNode::Program(program_contents) => {
                for content in program_contents {
                    match content {
                        ASTNode::FunctionDeclaration { .. } => {
                            function::compile_function(
                                self.context,
                                &self.module,
                                &self.builder,
                                &mut self.string_constants,
                                &mut self.class_fields,
                                content,
                            )?;
                        }
                        ASTNode::ClassDeclaration { .. } => {
                            class::define_class(
                                self.context,
                                &self.module,
                                &self.builder,
                                &mut self.string_constants,
                                &mut self.class_fields,
                                content,
                            )?;
                        }
                        _ => todo!(),
                    }
                }

                // TODO: Everything below this should be moved around
                //       create functions and remove debug statements.

                if output_path.is_some() {
                    if let Ok(tempfile) = tempfile::NamedTempFile::new() {
                        target_machine
                            .write_to_file(&self.module, FileType::Object, tempfile.path())
                            .expect("Could not write module to file");

                        let temppath = tempfile
                            .path()
                            .to_str()
                            .expect("Error creating temporary file");
                        let output_path = output_path.unwrap().to_str().unwrap();

                        // TODO: Make this actually look for library instead of hard coded debug
                        // path
                        Command::new("clang")
                            .args([
                                "-no-pie",
                                temppath,
                                "-o",
                                output_path,
                                "target/debug/libpyro_st.so",
                            ])
                            .output()
                            .expect("Error compiling");
                    } else {
                        return Err("Error creating temporary file".to_string());
                    }
                    return Ok(None);
                } else {
                    return Ok(Some(self.module.print_to_string()));
                }
            }
            _ => return Err("Not a program AST".to_string()),
        }
    }
}
