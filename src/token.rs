use derive_more::Display;
use phf::phf_map;

#[derive(Debug, Display, PartialEq, Clone)]
pub enum Token {
    Illegal,

    #[display(fmt = r#"Ident("{}")"#, _0)]
    Ident(String),
    #[display(fmt = "Int({})", _0)]
    Int(i64),

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

#[derive(Eq, Hash, PartialEq)]
pub enum TokenType {
    Illegal,

    Ident,
    Int,

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

impl From<Token> for TokenType {
    fn from(value: Token) -> Self {
        match value {
            Token::Illegal => TokenType::Illegal,
            Token::Ident(_) => TokenType::Ident,
            Token::Int(_) => TokenType::Int,
            Token::Assign => TokenType::Assign,
            Token::Plus => TokenType::Plus,
            Token::Minus => TokenType::Minus,
            Token::Bang => TokenType::Bang,
            Token::Asterisk => TokenType::Asterisk,
            Token::Slash => TokenType::Slash,
            Token::LessThan => TokenType::LessThan,
            Token::GreaterThan => TokenType::GreaterThan,
            Token::Equal => TokenType::Equal,
            Token::NotEqual => TokenType::NotEqual,
            Token::Comma => TokenType::Comma,
            Token::Semicolon => TokenType::Semicolon,
            Token::LeftParen => TokenType::LeftParen,
            Token::RightParen => TokenType::RightParen,
            Token::LeftBrace => TokenType::LeftBrace,
            Token::RightBrace => TokenType::RightBrace,
            Token::Function => TokenType::Function,
            Token::Let => TokenType::Let,
            Token::True => TokenType::True,
            Token::False => TokenType::False,
            Token::If => TokenType::If,
            Token::Else => TokenType::Else,
            Token::Return => TokenType::Return,
        }
    }
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

    pub fn to_type(&self) -> TokenType {
        TokenType::from(self.clone())
    }
}
