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
}


impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(character) = self.input.get(self.position) {
            let token = Token::from(character);
            self.position += 1;
            return Some(token);
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn test_next_token() {
        let input = String::from("=+(){},;");
        let tests = vec![
            Some(Token::Assign),
            Some(Token::Plus),
            Some(Token::LeftParen),
            Some(Token::RightParen),
            Some(Token::LeftBrace),
            Some(Token::RightBrace),
            Some(Token::Comma),
            Some(Token::Semicolon),
            None,
        ];
        let mut lexer = Lexer::new(input);
        for test_token in tests {
            let token = lexer.next();
            assert_eq!(token, test_token);
        }
    }
}