mod lexer;
use lexer::Lexer;

fn main() {
    let input = "let x = 42 + y;".to_string();
    let lexer = Lexer::new(&input);

    for token in lexer {
        println!("{:?}", token);
    }
}
