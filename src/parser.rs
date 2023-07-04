use std::rc::Rc;

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
        let let_token = self.current_token.clone();

        // Get identifier
        if self.peek_token.is_none() {
            return None;
        }
        let peek_token = self.peek_token.as_ref().unwrap();
        let name = if let Token::Ident(name) = peek_token {
            let identifier = Identifier {
                token: peek_token.clone(),
                value: name.into(),
            };
            self.next_token();
            identifier
        } else {
            return None;
        };

        // Check for assign token
        self.next_token();
        let assign = self.current_token.as_ref();
        if let Some(Token::Assign) = assign {
            self.next_token();
        } else {
            return None;
        }

        // Skip to semicolon
        while let Some(t) = self.current_token.as_ref() {
            let Token::Semicolon = t else {
                self.next_token();
                continue
            };
            break;
        }

        let expression = Expression::Identifier(name.clone());
        let_token.and_then(|t| {
            if let Token::Let = t {
                return Some(LetStatement {
                    name,
                    token: t,
                    value: expression,
                })
            }
            None
        })
    }

    fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program { statements: vec![] };
        while self
            .current_token
            .as_ref()
            .is_some_and(|t| matches!(t, Token::Let))
        {
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

    use crate::{ast::Statement, lexer::Lexer};

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
                s.name.value, name,
                "Expected statement.name.value '{}', got '{}'",
                name, s.name.value
            )
        } else {
            panic!("Not a let statement, got '{:?}'", statement)
        }
    }
}