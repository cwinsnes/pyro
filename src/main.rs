mod ast;
mod class;
mod common_utils;
mod compiler;
mod error;
mod function;
mod lexer;
mod statement;
use std::path::PathBuf;

use ast::Parser;
use clap::Parser as ClapParser;
use compiler::Compiler;
use inkwell::context::Context;
use lexer::Lexer;

#[derive(ClapParser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    filename: PathBuf,

    #[arg(short, long, help = "Write output to file. Defaults to ./a.out")]
    output_path: Option<PathBuf>,

    #[arg(
        long,
        help = "Print LLVM IR to stdout. If enabled, output path is ignored"
    )]
    print_llvm: bool,
}

fn main() {
    let args = Args::parse();
    let filename = args.filename;

    let filename = filename.as_path();
    let input_file = std::fs::read_to_string(filename)
        .expect(format!("Error reading file {:?}", filename).as_str());

    let lexer = Lexer::new(&input_file);

    let mut parser = Parser::new(lexer);
    let ast = parser.parse_program();

    if ast.is_err() {
        println!("{}", ast.unwrap_err());
        return;
    }

    let ast = ast.unwrap();

    let compiler_context = Context::create();
    let compiler = Compiler::new("module_0".to_string(), &compiler_context);

    let output_path = args.output_path.unwrap_or(PathBuf::from("a.out"));
    let res;
    if args.print_llvm {
        res = compiler.compile(ast, None);
        if res.is_ok() {
            println!(
                "{}",
                res.unwrap()
                    .expect("Invalid LLVM String generated")
                    .to_string()
            );
        } else {
            println!("{}", res.unwrap_err());
        }
    } else {
        res = compiler.compile(ast, Some(output_path.as_path()));
        if res.is_err() {
            println!("{:?}", res);
        }
    }
}
