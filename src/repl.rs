use crate::ast::Node;
use crate::evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::value::Environment;
use std::cell::RefCell;
use std::io;
use std::io::Write;
use std::rc::Rc;

static PROMPT: &str = ">>> ";

pub fn start() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let env = Rc::new(RefCell::new(Environment::new()));
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
                let evaluated = evaluator::eval(Node::Program(program), env.clone());
                println!("{}", evaluated.inspect());
            },
            Err(errors) => println!("Parse errors: {:?}", errors),
        }
    }
}
