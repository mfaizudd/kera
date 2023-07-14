use std::{collections::HashMap, rc::Rc};

use anyhow::anyhow;

use crate::{
    ast::{
        Boolean, Expression, Identifier, Infix, IntegerLiteral, Let, Prefix, Program, Return,
        Statement,
    },
    lexer::Lexer,
    token::{Token, TokenType},
};

#[derive(PartialEq, PartialOrd)]
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

impl TryFrom<Token> for Precedence {
    type Error = anyhow::Error;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::Equal => Ok(Precedence::Equals),
            Token::NotEqual => Ok(Precedence::Equals),
            Token::LessThan => Ok(Precedence::LessGreater),
            Token::GreaterThan => Ok(Precedence::LessGreater),
            Token::Plus => Ok(Precedence::Sum),
            Token::Minus => Ok(Precedence::Sum),
            Token::Slash => Ok(Precedence::Product),
            Token::Asterisk => Ok(Precedence::Product),
            _ => Err(anyhow!("No precedence found for this token")),
        }
    }
}

type PrefixParseFunction = fn(&mut Parser) -> Option<Expression>;
type InfixParseFunction = fn(&mut Parser, Expression) -> Option<Expression>;

pub struct Parser {
    lexer: Lexer,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<String>,
    prefix_parse_functions: HashMap<TokenType, PrefixParseFunction>,
    infix_parse_functions: HashMap<TokenType, InfixParseFunction>,
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
            infix_parse_functions: HashMap::new(),
        };
        parser.next_token();
        parser.next_token();

        parser.register_prefix(TokenType::Ident, Parser::parse_identifier);
        parser.register_prefix(TokenType::Int, Parser::parse_integer_literal);
        parser.register_prefix(TokenType::True, Parser::parse_boolean_literal);
        parser.register_prefix(TokenType::False, Parser::parse_boolean_literal);
        parser.register_prefix(TokenType::Bang, Parser::parse_prefix_expression);
        parser.register_prefix(TokenType::Minus, Parser::parse_prefix_expression);

        parser.register_infix(TokenType::Plus, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Minus, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Slash, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Asterisk, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Equal, Parser::parse_infix_expression);
        parser.register_infix(TokenType::NotEqual, Parser::parse_infix_expression);
        parser.register_infix(TokenType::LessThan, Parser::parse_infix_expression);
        parser.register_infix(TokenType::GreaterThan, Parser::parse_infix_expression);

        parser
    }

    fn peek_precedence(&self) -> Precedence {
        let Some(token) = self.peek_token.clone() else {
            return Precedence::Lowest;
        };
        token.try_into().unwrap_or(Precedence::Lowest)
    }

    fn current_precedence(&self) -> Precedence {
        let Some(token) = self.current_token.clone() else {
            return Precedence::Lowest;
        };
        token.try_into().unwrap_or(Precedence::Lowest)
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

    fn parse_boolean_literal(&mut self) -> Option<Expression> {
        match self.current_token.as_ref() {
            Some(Token::True) => Some(Expression::Boolean(Boolean {
                token: Token::True,
                value: true,
            })),
            Some(Token::False) => Some(Expression::Boolean(Boolean {
                token: Token::False,
                value: false,
            })),
            _ => None,
        }
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
            token,
            right: Rc::new(right),
        }))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let Some(token) = self.current_token.clone() else {
            self.errors.push("Expected expression".into());
            return None;
        };
        let precedence = self.current_precedence();
        self.next_token();
        let Some(right) = self.parse_expression(precedence) else {
            self.errors.push("Expected expression".into());
            return None;
        };
        Some(Expression::Infix(Infix {
            left: Rc::new(left),
            token,
            right: Rc::new(right),
        }))
    }

    fn register_prefix(&mut self, token_type: TokenType, function: PrefixParseFunction) {
        self.prefix_parse_functions.insert(token_type, function);
    }

    fn register_infix(&mut self, token_type: TokenType, function: InfixParseFunction) {
        self.infix_parse_functions.insert(token_type, function);
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
                Token::Let => return self.parse_let_statement().map(Statement::Let),
                Token::Return => return self.parse_return_statement().map(Statement::Return),
                _ => return self.parse_expression_statement().map(Statement::Expression),
            }
        }
        None
    }

    fn parse_let_statement(&mut self) -> Option<Let> {
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

        // Parse expression check the current token first so let's go to the next token
        self.next_token();
        let Some(expression) = self.parse_expression(Precedence::Lowest) else {
            self.errors.push("Expected expression".into());
            return None;
        };

        if let Some(Token::Semicolon) = self.peek_token {
            self.next_token();
        }

        Some(Let {
            token: Token::Let,
            name,
            value: expression,
        })
    }

    fn parse_return_statement(&mut self) -> Option<Return> {
        expect_current!(self, Token::Return, "return statement");
        self.next_token();

        let Some(expression) = self.parse_expression(Precedence::Lowest) else {
            self.errors.push("Expected expression".into());
            return None;
        };

        if let Some(Token::Semicolon) = self.peek_token {
            self.next_token();
        }

        Some(Return {
            token: Token::Return,
            return_value: expression,
        })
    }

    /// Parse the current token for expression
    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let token = self.current_token.clone()?;
        let Some(prefix) = self.prefix_parse_functions.get(&token.to_type()) else {
            self.errors.push(format!("No prefix parse function for {} found", token));
            return None;
        };
        let mut left = prefix(self)?;
        while self.peek_token.is_some() && precedence < self.peek_precedence() {
            match self.peek_token.as_ref() {
                Some(Token::Semicolon) => return Some(left),
                Some(token) if precedence < self.peek_precedence() => {
                    let Some(infix) = self.infix_parse_functions.get(&token.to_type()) else {
                        self.errors.push(format!("No infix parse function for {} found", token));
                        return Some(left);
                    };
                    let infix = *infix;
                    self.next_token();
                    left = infix(self, left)?;
                }
                _ => return Some(left),
            };
        }
        return Some(left);
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
        while self.current_token.is_some() {
            let statement = self.parse_statement();
            if let Some(statement) = statement {
                program.statements.push(statement);
            }
            self.next_token();
        }
        if !self.errors().is_empty() {
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
        if let Statement::Let(s) = statement {
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
            let Statement::Return(_) = statement else {
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

    #[test]
    fn test_parsing_infix_expressions() {
        struct TestCase<'a> {
            input: &'a str,
            left_value: i64,
            operator: Token,
            right_value: i64,
        }
        let tests = vec![
            TestCase {
                input: "5 + 5;",
                left_value: 5,
                operator: Token::Plus,
                right_value: 5,
            },
            TestCase {
                input: "5 - 5;",
                left_value: 5,
                operator: Token::Minus,
                right_value: 5,
            },
            TestCase {
                input: "5 * 5;",
                left_value: 5,
                operator: Token::Asterisk,
                right_value: 5,
            },
            TestCase {
                input: "5 / 5;",
                left_value: 5,
                operator: Token::Slash,
                right_value: 5,
            },
            TestCase {
                input: "5 > 5;",
                left_value: 5,
                operator: Token::GreaterThan,
                right_value: 5,
            },
            TestCase {
                input: "5 < 5;",
                left_value: 5,
                operator: Token::LessThan,
                right_value: 5,
            },
            TestCase {
                input: "5 == 5;",
                left_value: 5,
                operator: Token::Equal,
                right_value: 5,
            },
            TestCase {
                input: "5 != 5;",
                left_value: 5,
                operator: Token::NotEqual,
                right_value: 5,
            },
        ];

        for case in tests {
            let lexer = Lexer::new(case.input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().unwrap();
            assert_eq!(
                1,
                program.statements().len(),
                "Parsed program ({}): {:?}",
                case.input,
                program
            );

            let statement = program.statements().get(0).unwrap();
            let Statement::Expression(expression) = statement else {
                panic!("Expected an expression statement, found {:?}", statement)
            };

            let Expression::Infix(infix) = expression else {
                panic!("Expected a prefix, found {:?}", statement)
            };
            assert_eq!(&case.operator, infix.token());

            let Expression::IntegerLiteral(left) = &*infix.left else {
                panic!("Expected an integer literal, found {:?}", infix.right)
            };
            assert_eq!(case.left_value, left.value);

            let Expression::IntegerLiteral(right) = &*infix.right else {
                panic!("Expected an integer literal, found {:?}", infix.right)
            };
            assert_eq!(case.right_value, right.value);
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        struct TestCase<'a> {
            input: &'a str,
            expected: &'a str,
        }
        let tests = vec![
            TestCase {
                input: "-a * b",
                expected: "((-a) * b)",
            },
            TestCase {
                input: "!-a",
                expected: "(!(-a))",
            },
            TestCase {
                input: "a + b + c",
                expected: "((a + b) + c)",
            },
            TestCase {
                input: "a + b - c",
                expected: "((a + b) - c)",
            },
            TestCase {
                input: "a * b * c",
                expected: "((a * b) * c)",
            },
            TestCase {
                input: "a * b / c",
                expected: "((a * b) / c)",
            },
            TestCase {
                input: "a + b / c",
                expected: "(a + (b / c))",
            },
            TestCase {
                input: "a + b * c + d / e - f",
                expected: "(((a + (b * c)) + (d / e)) - f)",
            },
        ];
        for test in tests {
            let lexer = Lexer::new(test.input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().expect(test.input);
            let actual = format!("{}", program);
            assert_eq!(test.expected, actual);
        }
    }
}
