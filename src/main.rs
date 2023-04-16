mod ast;
mod code_generator;
mod lexer;
use std::path::Path;

use ast::Parser;
use lexer::Lexer;
use code_generator::Compiler;

fn main() {
    let input = "
    func main(number x, number y) {
        let x = 32;
    }
    
    func other() {
        let s = 123;
    }"
    .to_string();
    let lexer = Lexer::new(&input);

    let mut parser = Parser::new(lexer);
    let ast =  parser.parse_program().unwrap();
    let compiler = Compiler::new();

    println!("{:?}", ast);
    let mut compiler = compiler;
    
    let output_path = Path::new("output.o");
    compiler.compile(ast, output_path);
}
