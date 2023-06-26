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

    fn read_identifier(&mut self) -> String {
        let mut result = String::new();
        while self.char().is_some() && is_letter(self.char().unwrap()) {
            result.push(self.char().unwrap().to_owned());
            self.increment();
        }
        result
    }

    fn read_number(&mut self) -> isize {
        let mut result = String::new();
        while self.char().is_some() && self.char().unwrap().is_numeric() {
            result.push(self.char().unwrap().to_owned());
            self.increment();
        }
        result.parse::<isize>().unwrap()
    }

    fn skip_whitespace(&mut self) {
        while self.char().is_some() && self.char().unwrap().is_whitespace() {
            self.increment()
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        if let Some(character) = self.char() {
            let token = match character {
                '=' => Token::Assign,
                ';' => Token::Semicolon,
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                ',' => Token::Comma,
                '+' => Token::Plus,
                '{' => Token::LeftBrace,
                '}' => Token::RightBrace,
                ch => {
                    if is_letter(ch) {
                        let identifier = self.read_identifier();
                        return Some(Token::parse_keyword(identifier));
                    } else if ch.is_numeric() {
                        let number = self.read_number();
                        return Some(Token::Int(number));
                    } else {
                        Token::Illegal
                    }
                }
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
            misal hasil = tambah(lima, sepuluh);"#,
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
            None,
        ];
        let mut lexer = Lexer::new(input);
        for (i, test_token) in tests.iter().enumerate() {
            let token = lexer.next();
            assert_eq!(token, test_token.to_owned(), "Test #{}", i);
        }
    }
}
