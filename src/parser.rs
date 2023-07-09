use std::collections::HashMap;

use crate::{
    ast::{
        Expression, ExpressionStatement, Identifier, LetStatement, Program, ReturnStatement,
        Statement,
    },
    lexer::Lexer,
    token::{Token, TokenType},
};

pub struct Parser {
    lexer: Lexer,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<String>,
    prefix_parse_functions: HashMap<TokenType, fn(&Parser) -> Option<Expression>>,
    _infix_parse_functions: HashMap<TokenType, fn(Expression) -> Expression>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            current_token: None,
            peek_token: None,
            errors: vec![],
            prefix_parse_functions: HashMap::new(),
            _infix_parse_functions: HashMap::new(),
        };
        parser.next_token();
        parser.next_token();

        parser.register_prefix(TokenType::Ident, Parser::parse_identifier);

        parser
    }

    fn parse_identifier(&self) -> Option<Expression> {
        let Some(Token::Ident(ident)) = self.current_token.as_ref() else {
            return None
        };
        Some(Expression::Identifier(Identifier {
            token: Token::Ident(ident.into()),
            value: ident.into(),
        }))
    }

    fn register_prefix(
        &mut self,
        token_type: TokenType,
        function: fn(&Parser) -> Option<Expression>,
    ) {
        self.prefix_parse_functions.insert(token_type, function);
    }

    fn _register_infix(&mut self, token_type: TokenType, function: fn(Expression) -> Expression) {
        self._infix_parse_functions.insert(token_type, function);
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
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
                Token::Return => {
                    return self
                        .parse_return_statement()
                        .map(|s| Statement::ReturnStatement(s))
                }
                _ => {
                    return self
                        .parse_expression_statement()
                        .map(|s| Statement::ExpressionStatement(s))
                }
            }
        }
        None
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let Some(Token::Let) = self.current_token.as_ref() else {
            self.errors.push(format!("Expected {}, got {:?}", Token::Let, self.current_token));
            return None;
        };

        // Get identifier
        let Some(Token::Ident(name)) = self.peek_token.as_ref() else {
            self.errors.push(format!("Expected Identifier, got {:?}", self.peek_token));
            return None;
        };
        let name = Identifier {
            token: Token::Ident(name.into()),
            value: name.into(),
        };
        self.next_token();

        // Check for assign token
        let Some(Token::Assign) = self.peek_token.as_ref() else {
            self.errors.push(format!("Expected {}, got {:?}", Token::Assign, self.peek_token));
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

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        let Some(Token::Return) = self.current_token.as_ref() else {
            self.errors.push(format!("Expected return statement, found {:?}", self.current_token));
            return None;
        };
        // Skip to semicolon
        while let Some(t) = self.peek_token.as_ref() {
            let Token::Semicolon = t else {
                self.next_token();
                continue
            };
            break;
        }
        self.next_token();

        Some(ReturnStatement {
            token: Token::Return,
            return_value: Expression::Identifier(Identifier {
                token: Token::Ident("name".into()),
                value: "name".into(),
            }),
        })
    }

    fn parse_expression_statement(&mut self) -> Option<ExpressionStatement> {
        let token = self.current_token.clone()?;
        let prefix = self.prefix_parse_functions.get(&token.to_type())?;
        let expression = prefix(self)?;
        let statement = ExpressionStatement {
            token: token.clone(),
            expression,
        };

        // Skip to semicolon
        while let Some(t) = self.peek_token.as_ref() {
            let Token::Semicolon = t else {
                self.next_token();
                continue
            };
            break;
        }
        self.next_token();

        Some(statement)
    }

    pub fn parse_program(&mut self) -> Result<Program, &Vec<String>> {
        let mut program = Program { statements: vec![] };
        while let Some(_) = self.current_token.as_ref() {
            let statement = self.parse_statement();
            if let Some(statement) = statement {
                program.statements.push(statement);
            }
            self.next_token();
        }
        if self.errors().len() > 0 {
            return Err(self.errors());
        }
        Ok(program)
    }
}

#[cfg(test)]
mod tests {
    use core::panic;

    use crate::{
        ast::{Node, Statement},
        lexer::Lexer,
        token::Token,
    };

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
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(errors) => {
                panic!("Parser has errors: {:?}", errors)
            }
        };
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

    #[test]
    fn test_return_statements() {
        let input = r#"
        kembalikan 5;
        kembalikan 10;
        kembalikan 112233;
        "#;
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(errors) => {
                panic!("Parser has errors: {:?}", errors)
            }
        };
        assert_eq!(
            program.statements().len(),
            3,
            "program.statements does not contain 3 statements, got {}",
            program.statements().len()
        );

        for statement in program.statements() {
            let Statement::ReturnStatement(_) = statement else {
                panic!("Statement is not return statement, got '{:?}'", statement)
            };
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        assert_eq!(
            1,
            program.statements().len(),
            "Parsed program: {:?}",
            program
        );
        let statement = program.statements().get(0).unwrap();
        let Statement::ExpressionStatement(expression) = statement else {
            panic!("Expected an expression statement, found {:?}", statement)
        };
        assert_eq!(&Token::Ident("foobar".into()), expression.token())
    }
}
