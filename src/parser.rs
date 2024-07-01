use std::{collections::HashMap, rc::Rc};

use anyhow::anyhow;

use crate::{
    ast::{
        Block, BooleanLiteral, Expression, FunctionLiteral, Identifier, If, Infix, IntegerLiteral,
        Let, Prefix, Program, Return, Statement,
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
            return None;
        };
        let $result = token else {
            $self
                .errors
                .push(format!("Expected {}, got {:?}", $type, token));
            return None;
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
            $self
                .errors
                .push(format!("Expected {}, got {:?}", $type, token));
            return None;
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
        parser.register_prefix(TokenType::LeftParen, Parser::parse_grouped_expression);
        parser.register_prefix(TokenType::If, Parser::parse_if_expression);
        parser.register_prefix(TokenType::Function, Parser::parse_function_literal);

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

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();
        let expression = self.parse_expression(Precedence::Lowest);
        let Some(Token::RightParen) = self.peek_token.as_ref() else {
            return None;
        };
        self.next_token();
        expression
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        expect_current!(self, Token::If, "if");
        self.next_token();
        let condition = Rc::new(self.parse_expression(Precedence::Lowest)?);
        self.next_token();
        println!("next token: {:?}", self.current_token);
        let consequence = self.parse_block_statement()?;
        self.next_token();
        println!("and next token again: {:?}", self.current_token);
        let alternative = if let Some(Token::Else) = self.current_token {
            self.next_token();
            self.parse_block_statement()
        } else {
            None
        };
        Some(Expression::If(If {
            token: Token::If,
            condition,
            consequence,
            alternative,
        }))
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        expect_current!(self, Token::Function, "fungsi");
        expect_peek!(self, Token::LeftParen, "(");
        self.next_token();
        let parameters = self.parse_function_parameters()?;
        self.next_token();
        let body = self.parse_block_statement()?;
        let function = FunctionLiteral {
            token: Token::Function,
            parameters,
            body,
        };
        Some(Expression::FunctionLiteral(function))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        expect_current!(self, Token::LeftParen, "(");
        let mut parameters = Vec::new();
        if let Some(Token::RightParen) = self.peek_token {
            self.next_token();
            return Some(parameters);
        }

        self.next_token();
        let Some(Token::Ident(ident)) = self.current_token.as_ref() else {
            self.errors.push("Expected identifier".into());
            return None;
        };
        parameters.push(Identifier {
            token: Token::Ident(ident.into()),
            value: ident.into(),
        });

        while let Some(Token::Comma) = self.peek_token {
            self.next_token();
            self.next_token();
            let Some(Token::Ident(ident)) = self.current_token.as_ref() else {
                self.errors.push("Expected identifier".into());
                return None;
            };
            parameters.push(Identifier {
                token: Token::Ident(ident.into()),
                value: ident.into(),
            });
        }

        expect_peek!(self, Token::RightParen, ")");
        self.next_token();
        Some(parameters)
    }

    fn parse_block_statement(&mut self) -> Option<Block> {
        expect_current!(self, Token::LeftBrace, "{");
        self.next_token();
        let mut statements = Vec::new();
        while self
            .current_token
            .as_ref()
            .is_some_and(|t| !matches!(t, Token::RightBrace))
        {
            let statement = self.parse_statement();
            if let Some(statement) = statement {
                statements.push(statement);
            }
            self.next_token()
        }
        expect_current!(self, Token::RightBrace, "}");
        Some(Block {
            token: Token::LeftBrace,
            statements,
        })
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
        let Some(token) = self.current_token.as_ref() else {
            self.errors.push("Expected boolean".into());
            return None;
        };
        match token {
            Token::True | Token::False => Some(Expression::BooleanLiteral(BooleanLiteral {
                token: token.clone(),
                value: matches!(token, Token::True),
            })),
            _ => {
                self.errors.push("Expected boolean".into());
                None
            }
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
            self.errors
                .push(format!("No prefix parse function for {} found", token));
            return None;
        };
        let mut left = prefix(self)?;
        while self.peek_token.is_some() && precedence < self.peek_precedence() {
            match self.peek_token.as_ref() {
                Some(Token::Semicolon) => return Some(left),
                Some(token) if precedence < self.peek_precedence() => {
                    let Some(infix) = self.infix_parse_functions.get(&token.to_type()) else {
                        self.errors
                            .push(format!("No infix parse function for {} found", token));
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
            left_value: Token,
            operator: Token,
            right_value: Token,
        }
        let tests = vec![
            TestCase {
                input: "benar == benar",
                left_value: Token::True,
                operator: Token::Equal,
                right_value: Token::True,
            },
            TestCase {
                input: "benar != salah",
                left_value: Token::True,
                operator: Token::NotEqual,
                right_value: Token::False,
            },
            TestCase {
                input: "salah == salah",
                left_value: Token::False,
                operator: Token::Equal,
                right_value: Token::False,
            },
            TestCase {
                input: "5 + 5;",
                left_value: Token::Int(5),
                operator: Token::Plus,
                right_value: Token::Int(5),
            },
            TestCase {
                input: "5 - 5;",
                left_value: Token::Int(5),
                operator: Token::Minus,
                right_value: Token::Int(5),
            },
            TestCase {
                input: "5 * 5;",
                left_value: Token::Int(5),
                operator: Token::Asterisk,
                right_value: Token::Int(5),
            },
            TestCase {
                input: "5 / 5;",
                left_value: Token::Int(5),
                operator: Token::Slash,
                right_value: Token::Int(5),
            },
            TestCase {
                input: "5 > 5;",
                left_value: Token::Int(5),
                operator: Token::GreaterThan,
                right_value: Token::Int(5),
            },
            TestCase {
                input: "5 < 5;",
                left_value: Token::Int(5),
                operator: Token::LessThan,
                right_value: Token::Int(5),
            },
            TestCase {
                input: "5 == 5;",
                left_value: Token::Int(5),
                operator: Token::Equal,
                right_value: Token::Int(5),
            },
            TestCase {
                input: "5 != 5;",
                left_value: Token::Int(5),
                operator: Token::NotEqual,
                right_value: Token::Int(5),
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
                panic!("Expected a infix, found {:?}", statement)
            };
            assert_eq!(&case.operator, infix.token());

            test_literal_expression(case.left_value, &*infix.left);
            test_literal_expression(case.right_value, &*infix.right);
        }
    }

    macro_rules! assert_literal {
        ($expected:expr, $actual:expr) => {
            assert_eq!(
                $expected, $actual.value,
                "Expected {}, got {}",
                $expected, $actual.value
            )
        };
    }

    fn test_literal_expression(expected: Token, actual: &Expression) {
        match expected {
            Token::Int(expected) => {
                let Expression::IntegerLiteral(actual) = actual else {
                    panic!("Expected an integer literal, found {}", actual)
                };
                assert_literal!(expected, actual)
            }
            Token::True | Token::False => {
                let Expression::BooleanLiteral(actual) = actual else {
                    panic!("Expected a boolean literal, found {}", actual)
                };
                let expected = matches!(expected, Token::True);
                assert_literal!(expected, actual)
            }
            Token::Ident(expected) => {
                let Expression::Identifier(actual) = actual else {
                    panic!("Expected a identifier, found {}", actual)
                };
                assert_literal!(expected, actual)
            }
            _ => panic!("Expected a literal expression, found {}", actual),
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
                input: "1 + (2 + 3) + 4",
                expected: "((1 + (2 + 3)) + 4)",
            },
            TestCase {
                input: "(5 + 5) * 2",
                expected: "((5 + 5) * 2)",
            },
            TestCase {
                input: "2 / (5 + 5)",
                expected: "(2 / (5 + 5))",
            },
            TestCase {
                input: "benar",
                expected: "benar",
            },
            TestCase {
                input: "salah",
                expected: "salah",
            },
            TestCase {
                input: "3 > 5 == salah",
                expected: "((3 > 5) == salah)",
            },
            TestCase {
                input: "3 < 5 == benar",
                expected: "((3 < 5) == benar)",
            },
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

    #[test]
    fn test_if_expression() {
        let input = "jika x < y { x }";
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
            panic!("Expected an expression statement, found: {:?}", statement)
        };
        let Expression::If(if_expression) = expression else {
            panic!("Expected an if expression, found: {:?}", expression)
        };
        let Expression::Infix(condition) = &*if_expression.condition else {
            panic!(
                "Expected condition to be an infix expression, found: {:?}",
                if_expression.condition
            )
        };
        assert_eq!(Token::LessThan, condition.token);
        test_literal_expression(Token::Ident("x".into()), &condition.left);
        test_literal_expression(Token::Ident("y".into()), &condition.right);
        assert_eq!(
            1,
            if_expression.consequence.statements.len(),
            "Consequence is not 1 statement. got: {:?}",
            if_expression.consequence.statements.len()
        );
        let statement = if_expression.consequence.statements.get(0).unwrap();
        let Statement::Expression(Expression::Identifier(consequence)) = statement else {
            panic!("Expected an identifier, found: {:?}", statement)
        };
        assert_eq!("x", consequence.value);
        assert!(if_expression.alternative.is_none());
    }

    #[test]
    fn test_if_else_expression() {
        let input = "jika x < y { x } lainnya { y }";
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
            panic!("Expected an expression statement, found: {:?}", statement)
        };
        let Expression::If(if_expression) = expression else {
            panic!("Expected an if expression, found: {:?}", expression)
        };
        let Expression::Infix(condition) = &*if_expression.condition else {
            panic!(
                "Expected condition to be an infix expression, found: {:?}",
                if_expression.condition
            )
        };
        assert_eq!(Token::LessThan, condition.token);
        test_literal_expression(Token::Ident("x".into()), &condition.left);
        test_literal_expression(Token::Ident("y".into()), &condition.right);
        assert_eq!(
            1,
            if_expression.consequence.statements.len(),
            "Consequence is not 1 statement. got: {:?}",
            if_expression.consequence.statements.len()
        );
        let statement = if_expression.consequence.statements.get(0).unwrap();
        let Statement::Expression(Expression::Identifier(consequence)) = statement else {
            panic!("Expected an identifier, found: {:?}", statement)
        };
        assert_eq!("x", consequence.value);
        assert!(if_expression.alternative.is_some());
        assert_eq!(
            1,
            if_expression.alternative.as_ref().unwrap().statements.len(),
            "Consequence is not 1 statement. got: {:?}",
            if_expression.alternative.as_ref().unwrap().statements.len()
        );
        let statement = if_expression
            .alternative
            .as_ref()
            .unwrap()
            .statements
            .get(0)
            .unwrap();
        let Statement::Expression(Expression::Identifier(alternative)) = statement else {
            panic!("Expected an identifier, found: {:?}", statement)
        };
        assert_eq!("y", alternative.value);
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fungsi(x, y) { x + y; }";
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
            panic!("Expected an expression statement, found: {:?}", statement)
        };
        let Expression::FunctionLiteral(function) = expression else {
            panic!("Expected a function literal, found: {:?}", expression)
        };
        assert_eq!(function.parameters[0].value, "x");
        assert_eq!(function.parameters[1].value, "y");
        assert_eq!(function.body.statements.len(), 1);
        let Statement::Expression(body) = &function.body.statements[0] else {
            panic!(
                "Function body statement is not expression statement, found: {:?}",
                &function.body.statements[0]
            )
        };
        let Expression::Infix(infix) = body else {
            panic!("Expression is not an infix expression")
        };
        assert_eq!(infix.left.token(), &Token::Ident("x".into()));
        assert_eq!(infix.token(), &Token::Plus);
        assert_eq!(infix.right.token(), &Token::Ident("y".into()));
    }

    #[test]
    fn test_function_paramter_parsing() {
        struct TestCase<'a> {
            input: &'a str,
            expected: Vec<&'a str>,
        }
        let tests = vec![
            TestCase {
                input: "fungsi() {};",
                expected: vec![],
            },
            TestCase {
                input: "fungsi(x) {};",
                expected: vec!["x"],
            },
            TestCase {
                input: "fungsi(x,y,z) {};",
                expected: vec!["x", "y", "z"],
            },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input.into());
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
                panic!("Expected an expression statement, found: {:?}", statement)
            };
            let Expression::FunctionLiteral(function) = expression else {
                panic!("Expected a function literal, found: {:?}", expression)
            };
            assert_eq!(function.parameters.len(), test.expected.len());
            for (i, parameter) in function.parameters.iter().enumerate() {
                assert_eq!(parameter.value, test.expected[i]);
            }
        }
    }
}
