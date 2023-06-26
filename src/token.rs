use phf::phf_map;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Illegal,

    Ident(String),
    Int(isize),

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LessThan,
    GreaterThan,

    Comma,
    Semicolon,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    Function,
    Let,
}

static KEYWORDS: phf::Map<&'static str, Token> = phf_map! {
    "fungsi" => Token::Function,
    "misal" => Token::Let,
};

impl Token {
    pub fn parse_keyword(keyword: String) -> Token {
        if let Some(token) = KEYWORDS.get(&keyword) {
            return token.to_owned();
        }
        Token::Ident(keyword)
    }
}
