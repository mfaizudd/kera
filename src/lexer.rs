use crate::token::Token;

pub struct Lexer {
    input: Vec<char>,
    position: usize,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let lexer = Lexer {
            input: input.chars().collect(),
            position: 0,
        };
        lexer
    }

    fn increment(&mut self) {
        self.position += 1
    }

    fn char(&self) -> Option<&char> {
        return self.input.get(self.position);
    }

    fn peek_char(&self) -> Option<&char> {
        return self.input.get(self.position + 1);
    }

    fn read_identifier(&mut self) -> String {
        let mut result = String::new();
        while self.char().is_some_and(is_letter) {
            result.push(self.char().unwrap().to_owned());
            self.increment();
        }
        result
    }

    fn read_number(&mut self) -> i64 {
        let mut result = String::new();
        while self.char().is_some_and(|c| c.is_ascii_digit()) {
            result.push(self.char().unwrap().to_owned());
            self.increment();
        }
        result.parse::<i64>().unwrap()
    }

    fn skip_whitespace(&mut self) {
        while self.char().is_some_and(|c| c.is_whitespace()) {
            self.increment()
        }
    }

    fn read_string(&mut self) -> String {
        let mut result = String::new();

        // Skip the first double-quote
        self.increment();
        while self.char().is_some_and(|c| *c != '"') {
            result.push(*self.char().unwrap());
            self.increment();
        }

        // Skip the last double-quote
        self.increment();
        result
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        if let Some(character) = self.char() {
            let token = match character {
                '=' if self.peek_char().is_some_and(|c| c == &'=') => {
                    self.increment();
                    Token::Equal
                }
                '=' => Token::Assign,
                ';' => Token::Semicolon,
                ':' => Token::Colon,
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                ',' => Token::Comma,
                '+' => Token::Plus,
                '-' => Token::Minus,
                '!' if self.peek_char().is_some_and(|c| c == &'=') => {
                    self.increment();
                    Token::NotEqual
                }
                '!' => Token::Bang,
                '/' => Token::Slash,
                '%' => Token::Percent,
                '*' => Token::Asterisk,
                '<' => Token::LessThan,
                '>' => Token::GreaterThan,
                '{' => Token::LeftBrace,
                '}' => Token::RightBrace,
                '[' => Token::LeftBracket,
                ']' => Token::RightBracket,
                '"' => {
                    let string = self.read_string();
                    return Some(Token::String(string));
                }
                ch if is_letter(ch) => {
                    let identifier = self.read_identifier();
                    return Some(Token::parse_keyword(identifier));
                }
                ch if ch.is_ascii_digit() => {
                    let number = self.read_number();
                    return Some(Token::Int(number));
                }
                _ => Token::Illegal,
            };
            self.increment();
            return Some(token);
        }
        None
    }
}

fn is_letter(character: &char) -> bool {
    character.is_ascii_alphabetic() || character == &'_'
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn test_next_token() {
        let input = String::from(
            r#"misal lima = 5;
            misal sepuluh = 10;
            misal tambah = fungsi(x, y) {
                x + y;
            };
            misal hasil = tambah(lima, sepuluh);
            !-/*5;
            5 < 10 > 5;
            jika (5 < 10) {
                kembalikan benar;
            } lainnya {
                kembalikan salah;
            }

            10 == 10;
            10 != 9;
            "foobar"
            "foo bar"
            [10, 2];
            {"foo": "bar"}
            "#,
        );
        let tests = vec![
            Some(Token::Let),
            Some(Token::Ident("lima".into())),
            Some(Token::Assign),
            Some(Token::Int(5)),
            Some(Token::Semicolon),
            Some(Token::Let),
            Some(Token::Ident("sepuluh".into())),
            Some(Token::Assign),
            Some(Token::Int(10)),
            Some(Token::Semicolon),
            Some(Token::Let),
            Some(Token::Ident("tambah".into())),
            Some(Token::Assign),
            Some(Token::Function),
            Some(Token::LeftParen),
            Some(Token::Ident("x".into())),
            Some(Token::Comma),
            Some(Token::Ident("y".into())),
            Some(Token::RightParen),
            Some(Token::LeftBrace),
            Some(Token::Ident("x".into())),
            Some(Token::Plus),
            Some(Token::Ident("y".into())),
            Some(Token::Semicolon),
            Some(Token::RightBrace),
            Some(Token::Semicolon),
            Some(Token::Let),
            Some(Token::Ident("hasil".into())),
            Some(Token::Assign),
            Some(Token::Ident("tambah".into())),
            Some(Token::LeftParen),
            Some(Token::Ident("lima".into())),
            Some(Token::Comma),
            Some(Token::Ident("sepuluh".into())),
            Some(Token::RightParen),
            Some(Token::Semicolon),
            Some(Token::Bang),
            Some(Token::Minus),
            Some(Token::Slash),
            Some(Token::Asterisk),
            Some(Token::Int(5)),
            Some(Token::Semicolon),
            Some(Token::Int(5)),
            Some(Token::LessThan),
            Some(Token::Int(10)),
            Some(Token::GreaterThan),
            Some(Token::Int(5)),
            Some(Token::Semicolon),
            Some(Token::If),
            Some(Token::LeftParen),
            Some(Token::Int(5)),
            Some(Token::LessThan),
            Some(Token::Int(10)),
            Some(Token::RightParen),
            Some(Token::LeftBrace),
            Some(Token::Return),
            Some(Token::True),
            Some(Token::Semicolon),
            Some(Token::RightBrace),
            Some(Token::Else),
            Some(Token::LeftBrace),
            Some(Token::Return),
            Some(Token::False),
            Some(Token::Semicolon),
            Some(Token::RightBrace),
            Some(Token::Int(10)),
            Some(Token::Equal),
            Some(Token::Int(10)),
            Some(Token::Semicolon),
            Some(Token::Int(10)),
            Some(Token::NotEqual),
            Some(Token::Int(9)),
            Some(Token::Semicolon),
            Some(Token::String("foobar".into())),
            Some(Token::String("foo bar".into())),
            Some(Token::LeftBracket),
            Some(Token::Int(10)),
            Some(Token::Comma),
            Some(Token::Int(2)),
            Some(Token::RightBracket),
            Some(Token::Semicolon),
            Some(Token::LeftBrace),
            Some(Token::String("foo".into())),
            Some(Token::Colon),
            Some(Token::String("bar".into())),
            Some(Token::RightBrace),
            None,
        ];
        let mut lexer = Lexer::new(input);
        for (i, test_token) in tests.iter().enumerate() {
            let token = lexer.next();
            assert_eq!(token, test_token.to_owned(), "Test #{}", i);
        }
    }
}
