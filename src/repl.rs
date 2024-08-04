use crate::ast::Node;
use crate::evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io;
use std::io::Write;

static PROMPT: &str = ">>> ";

pub fn start() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    loop {
        print!("{}", PROMPT);
        stdout.flush().expect("Failed to flush stdout");
        let mut input = String::new();
        stdin.read_line(&mut input).expect("Invalid input");
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        match program {
            Ok(program) => {
                let evaluated = evaluator::eval(Node::Program(&program));
                println!("{}", evaluated.inspect());
            },
            Err(errors) => println!("Parse errors: {:?}", errors),
        }
    }
}
