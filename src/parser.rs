use crate::{
    ast::{Expression, Identifier, LetStatement, Program, Statement},
    lexer::Lexer,
    token::Token,
};

pub struct Parser {
    lexer: Lexer,
    current_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            current_token: None,
            peek_token: None,
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next();
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        if let Some(token) = self.current_token.as_ref() {
            match token {
                Token::Let => {
                    return self
                        .parse_let_statement()
                        .map(|s| Statement::LetStatement(s))
                }
                _ => return None,
            }
        }
        None
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let Some(Token::Let) = self.current_token.as_ref() else {
            return None;
        };

        // Get identifier
        let Some(Token::Ident(name)) = self.peek_token.as_ref() else {
            return None;
        };
        let name = Identifier {
            token: Token::Ident(name.into()),
        };
        self.next_token();

        // Check for assign token
        let Some(Token::Assign) = self.peek_token.as_ref() else {
            return None;
        };
        self.next_token();

        // Skip to semicolon
        while let Some(t) = self.peek_token.as_ref() {
            let Token::Semicolon = t else {
                self.next_token();
                continue
            };
            break;
        }
        self.next_token();

        let expression = Expression::Identifier(name.clone());
        Some(LetStatement {
            token: Token::Let,
            name,
            value: expression,
        })
    }

    fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program { statements: vec![] };
        while let Some(Token::Let) = self.current_token.as_ref() {
            let statement = self.parse_statement();
            if let Some(statement) = statement {
                program.statements.push(statement);
            }
            self.next_token();
        }
        Some(program)
    }
}

#[cfg(test)]
mod tests {
    use core::panic;

    use crate::{ast::Statement, lexer::Lexer, token::Token};

    use super::Parser;

    #[test]
    fn test_let_statements() {
        let input = r#"
        misal x = 5;
        misal y = 10;
        misal foobar = 838383;
        "#;
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser
            .parse_program()
            .expect("parser.parse_program() returns None");
        assert_eq!(
            program.statements().len(),
            3,
            "program.statements does not contain 3 statements, got {}",
            program.statements().len()
        );

        let expecteds = vec!["x", "y", "foobar"];
        for (i, expected) in expecteds.iter().enumerate() {
            let statement = program.statements().get(i);
            let statement =
                statement.expect(&format!("Expected a statement, got a {:?}", statement));
            test_let_statement(statement, &expected)
        }
    }

    fn test_let_statement(statement: &Statement, name: &str) {
        if let Statement::LetStatement(s) = statement {
            assert_eq!(
                s.name.token,
                Token::Ident(name.into()),
                "Expected statement.name.value '{}', got '{}'",
                name,
                s.name.token
            )
        } else {
            panic!("Not a let statement, got '{:?}'", statement)
        }
    }
}
