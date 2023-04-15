mod ast;
mod lexer;
use ast::Parser;
use lexer::Lexer;

fn main() {
    let input = "func main() {} ".to_string();
    let lexer = Lexer::new(&input);

    let mut parser = Parser::new(lexer);
    println!("{:?}", parser.parse_program());
}
