#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Illegal,

    Ident(String),
    Int(isize),

    Assign,
    Plus,

    Comma,
    Semicolon,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    Function,
    Let,
}

impl From<&char> for Token {
    fn from(character: &char) -> Token {
        match character {
            '=' => Token::Assign,
            ';' => Token::Semicolon,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            _ => Token::Illegal,
        }
    }
}