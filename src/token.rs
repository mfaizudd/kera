use derive_more::Display;
use phf::phf_map;

#[derive(Debug, Display, PartialEq, Clone)]
pub enum Token {
    Illegal,

    #[display(fmt = r#"Ident("{}")"#, _0)]
    Ident(String),
    #[display(fmt = "Int({})", _0)]
    Int(isize),

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LessThan,
    GreaterThan,
    Equal,
    NotEqual,

    Comma,
    Semicolon,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

static KEYWORDS: phf::Map<&'static str, Token> = phf_map! {
    "fungsi" => Token::Function,
    "misal" => Token::Let,
    "benar" => Token::True,
    "salah" => Token::False,
    "jika" => Token::If,
    "lainnya" => Token::Else,
    "kembalikan" => Token::Return,
};

impl Token {
    pub fn parse_keyword(keyword: String) -> Token {
        if let Some(token) = KEYWORDS.get(&keyword) {
            return token.to_owned();
        }
        Token::Ident(keyword)
    }
}
