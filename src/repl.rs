use crate::lexer::Lexer;
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
        for token in lexer {
            println!("{}", token);
        }
    }
}
