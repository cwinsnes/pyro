mod ast;
mod compiler;
mod lexer;
use std::path::Path;

use ast::Parser;
use compiler::Compiler;
use lexer::Lexer;

fn main() {
    let input = "
    func other(number x) > number {
        return x;
    }
    func main() > number {
        print(other(32));
        return other(123);
    }
    "
    .to_string();
    let lexer = Lexer::new(&input);

    let mut parser = Parser::new(lexer);
    let ast = parser.parse_program().unwrap();
    let compiler = Compiler::new();

    let mut compiler = compiler;

    let output_path = Path::new("output.o");
    let res = compiler.compile(ast, Some(output_path));
    if res.is_err() {
        println!("{:?}", res);
    }
}
