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
        if let Err(errors) = parser.parse_program() {
            println!("Parse errors: {:?}", errors)
        }
    }
}
