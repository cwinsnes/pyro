mod ast;
mod common_utils;
mod compiler;
mod function;
mod lexer;
mod statement;
use std::path::Path;

use ast::Parser;
use compiler::Compiler;
use inkwell::context::Context;
use lexer::Lexer;

fn main() {
    let input = "
    func other(integer x) > integer {
        return x;
    }
    func main() > integer {
        let test_string = \"Test!\";
        print(\"int\");
        print(\"int\");
        print(\"This is a {int}-{string}-{string}\", 321, test_string, \"asd\");
        print(\"This is a {string} string\", test_string);
        print(\"{string}\", \"other test string\");
        return other(123);
    }
    "
    .to_string();
    let lexer = Lexer::new(&input);

    let mut parser = Parser::new(lexer);
    let ast = parser.parse_program().unwrap();

    let compiler_context = Context::create();
    let compiler = Compiler::new("module_0".to_string(), &compiler_context);

    let output_path = Path::new("output.o");
    let res = compiler.compile(ast, Some(output_path));
    if res.is_err() {
        println!("{:?}", res);
    }
}
