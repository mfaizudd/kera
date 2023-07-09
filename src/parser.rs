use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{
        Expression, Identifier, IntegerLiteral, LetStatement, Prefix, Program, ReturnStatement,
        Statement,
    },
    lexer::Lexer,
    token::{Token, TokenType},
};

#[allow(dead_code)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

pub struct Parser {
    lexer: Lexer,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<String>,
    prefix_parse_functions: HashMap<TokenType, fn(&mut Parser) -> Option<Expression>>,
    _infix_parse_functions: HashMap<TokenType, fn(Expression) -> Expression>,
}

macro_rules! expect_current {
    ($self:ident, $result:pat_param, $type:expr) => {
        let Some(token) = $self.current_token.as_ref() else {
                            $self.errors.push(format!("Expected {}", $type));
                            return None
                        };
        let $result = token else {
                            $self.errors.push(format!("Expected {}, got {:?}", $type, token));
                            return None
                        };
    };
}

macro_rules! expect_peek {
    ($self:ident, $result:pat_param) => {
        expect_peek!($self, $result, $result)
    };
    ($self:ident, $result:pat_param, $type:expr) => {
        let Some(token) = $self.peek_token.as_ref() else {
                            $self.errors.push(format!("Expected {}", $type));
                            return None;
                        };
        let $result = token else {
                            $self.errors.push(format!("Expected {}, got {:?}", $type, token));
                            return None
                        };
    };
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
        parser.register_prefix(TokenType::Int, Parser::parse_integer_literal);
        parser.register_prefix(TokenType::Bang, Parser::parse_prefix_expression);
        parser.register_prefix(TokenType::Minus, Parser::parse_prefix_expression);

        parser
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        expect_current!(self, Token::Ident(ident), "identifier");

        Some(Expression::Identifier(Identifier {
            token: Token::Ident(ident.into()),
            value: ident.into(),
        }))
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        expect_current!(self, Token::Int(literal), "integer");

        Some(Expression::IntegerLiteral(IntegerLiteral {
            token: Token::Int(*literal),
            value: *literal,
        }))
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let Some(token) = self.current_token.clone() else {
            self.errors.push("Expected expression".into());
            return None;
        };
        self.next_token();
        let Some(right) = self.parse_expression(Precedence::Prefix) else {
            self.errors.push("Expected expression".into());
            return None;
        };
        Some(Expression::Prefix(Prefix {
            token: token.clone(),
            right: Rc::new(right),
        }))
    }

    fn register_prefix(
        &mut self,
        token_type: TokenType,
        function: fn(&mut Parser) -> Option<Expression>,
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
                        .map(|s| Statement::Expression(s))
                }
            }
        }
        None
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        expect_current!(self, Token::Let, "let");

        // Get identifier
        expect_peek!(self, Token::Ident(name), "identifier");
        let name = Identifier {
            token: Token::Ident(name.into()),
            value: name.into(),
        };
        self.next_token();

        // Check for assign token
        expect_peek!(self, Token::Assign, Token::Assign);
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
        expect_current!(self, Token::Return, "return statement");

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

    fn parse_expression(&mut self, _precedence: Precedence) -> Option<Expression> {
        let token = self.current_token.clone()?;
        let Some(prefix) = self.prefix_parse_functions.get(&token.to_type()) else {
            self.errors.push(format!("Expected expression, found {:?}", token));
            return None;
        };
        prefix(self)
    }

    fn parse_expression_statement(&mut self) -> Option<Expression> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        if let Some(Token::Semicolon) = self.peek_token {
            self.next_token();
        }

        Some(expression)
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
        ast::{Expression, Node, Statement},
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
        let Statement::Expression(expression) = statement else {
            panic!("Expected an expression statement, found {:?}", statement)
        };
        assert_eq!(&Token::Ident("foobar".into()), expression.token())
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
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
        let Statement::Expression(expression) = statement else {
            panic!("Expected an expression statement, found {:?}", statement)
        };
        assert_eq!(&Token::Int(5), expression.token())
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        struct TestCase<'a> {
            input: &'a str,
            operator: Token,
            value: i64,
        }
        let tests = vec![
            TestCase {
                input: "!5;",
                operator: Token::Bang,
                value: 5,
            },
            TestCase {
                input: "-15;",
                operator: Token::Minus,
                value: 15,
            },
        ];
        for case in tests {
            let lexer = Lexer::new(case.input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().unwrap();
            assert_eq!(
                1,
                program.statements().len(),
                "Parsed program: {:?}",
                program
            );
            let statement = program.statements().get(0).unwrap();
            let Statement::Expression(expression) = statement else {
                panic!("Expected an expression statement, found {:?}", statement)
            };
            let Expression::Prefix(prefix) = expression else {
                panic!("Expected a prefix, found {:?}", statement)
            };
            assert_eq!(&case.operator, prefix.token());
            let Expression::IntegerLiteral(value) = &*prefix.right else {
                panic!("Expected an integer literal, found {:?}", prefix.right)
            };
            assert_eq!(case.value, value.value)
        }
    }
}
