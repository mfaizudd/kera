use anyhow::anyhow;
use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{
        ArrayLiteral, Block, BooleanLiteral, Call, Expression, FunctionLiteral, HashLiteral,
        HashPair, Identifier, If, Index, Infix, IntegerLiteral, Let, Prefix, Program, Return,
        Statement, StringLiteral,
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
    Index,
}

impl TryFrom<Token> for Precedence {
    type Error = anyhow::Error;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::LeftParen => Ok(Precedence::Call),
            Token::Equal => Ok(Precedence::Equals),
            Token::NotEqual => Ok(Precedence::Equals),
            Token::LessThan => Ok(Precedence::LessGreater),
            Token::GreaterThan => Ok(Precedence::LessGreater),
            Token::Plus => Ok(Precedence::Sum),
            Token::Minus => Ok(Precedence::Sum),
            Token::Slash => Ok(Precedence::Product),
            Token::Asterisk => Ok(Precedence::Product),
            Token::Percent => Ok(Precedence::Product),
            Token::LeftBracket => Ok(Precedence::Index),
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
        parser.register_prefix(TokenType::String, Parser::parse_string_literal);
        parser.register_prefix(TokenType::LeftBracket, Parser::parse_array_literal);
        parser.register_prefix(TokenType::LeftBrace, Parser::parse_hash_literal);

        parser.register_infix(TokenType::Plus, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Minus, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Slash, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Asterisk, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Percent, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Equal, Parser::parse_infix_expression);
        parser.register_infix(TokenType::NotEqual, Parser::parse_infix_expression);
        parser.register_infix(TokenType::LessThan, Parser::parse_infix_expression);
        parser.register_infix(TokenType::GreaterThan, Parser::parse_infix_expression);
        parser.register_infix(TokenType::LeftParen, Parser::parse_call_expression);
        parser.register_infix(TokenType::LeftBracket, Parser::parse_index_expression);

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
        let consequence = self.parse_block_statement()?;
        self.next_token();
        let alternative = if let Some(Token::Else) = self.current_token {
            self.next_token();
            self.parse_block_statement()
        } else {
            None
        };
        Some(Expression::If(If {
            token: Token::If,
            condition,
            consequence: Rc::new(Statement::Block(consequence)),
            alternative: alternative.map(|a| Rc::new(Statement::Block(a))),
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
            parameters: parameters.into(),
            body: Rc::new(Statement::Block(body)),
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

    fn parse_string_literal(&mut self) -> Option<Expression> {
        expect_current!(self, Token::String(literal), "string");

        Some(Expression::StringLiteral(StringLiteral {
            token: Token::String(literal.clone()),
            value: literal.clone(),
        }))
    }

    fn parse_array_literal(&mut self) -> Option<Expression> {
        expect_current!(self, Token::LeftBracket, "[");
        let elements = self.parse_expression_list(Token::RightBracket)?;
        Some(Expression::ArrayLiteral(ArrayLiteral {
            token: Token::LeftBracket,
            elements,
        }))
    }

    fn parse_hash_literal(&mut self) -> Option<Expression> {
        expect_current!(self, Token::LeftBrace, "{");
        let mut pairs = vec![];
        self.next_token();

        // empty map
        if self
            .current_token
            .as_ref()
            .is_some_and(|t| *t == Token::RightBrace)
        {
            let literal = Expression::HashLiteral(HashLiteral {
                token: Token::LeftBrace,
                pairs,
            });
            return Some(literal);
        }

        // first entry
        let key = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token.as_ref().is_none_or(|t| *t != Token::Colon) {
            return None;
        }
        self.next_token();
        self.next_token();
        let val = self.parse_expression(Precedence::Lowest)?;
        pairs.push(HashPair(key.into(), val.into()));

        // subsequent entries
        while let Some(Token::Comma) = self.peek_token {
            self.next_token();
            self.next_token();
            let key = self.parse_expression(Precedence::Lowest)?;
            if self.peek_token.as_ref().is_none_or(|t| *t != Token::Colon) {
                return None;
            }
            self.next_token();
            self.next_token();
            let val = self.parse_expression(Precedence::Lowest)?;
            pairs.push(HashPair(key.into(), val.into()));
        }

        // end maps
        expect_peek!(self, Token::RightBrace, "}");
        self.next_token();
        let literal = Expression::HashLiteral(HashLiteral {
            token: Token::LeftBrace,
            pairs,
        });
        Some(literal)
    }

    fn parse_expression_list(&mut self, end: Token) -> Option<Vec<Rc<Expression>>> {
        let mut list = Vec::new();

        if self.peek_token.as_ref().is_some_and(|t| t == &end) {
            self.next_token();
            return Some(list);
        }

        self.next_token();
        list.push(self.parse_expression(Precedence::Lowest)?.into());

        while let Some(Token::Comma) = self.peek_token {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedence::Lowest)?.into());
        }

        if self.peek_token.as_ref().is_none_or(|t| t != &end) {
            self.errors
                .push(format!("Expected {}, got {:?}", end, self.peek_token));
            return None;
        }

        self.next_token();
        Some(list)
    }

    fn parse_call_expression(&mut self, function_ident: Expression) -> Option<Expression> {
        expect_current!(self, Token::LeftParen, "(");
        let arguments = self.parse_expression_list(Token::RightParen)?;
        Some(Expression::Call(Call {
            token: Token::LeftParen,
            function_ident: Rc::new(function_ident),
            arguments,
        }))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
        expect_current!(self, Token::LeftBracket, "[");
        self.next_token();
        let index = self.parse_expression(Precedence::Lowest)?;
        expect_peek!(self, Token::RightBracket, "]");
        self.next_token();
        Some(Expression::Index(Index {
            token: Token::LeftBracket,
            left: left.into(),
            index: index.into(),
        }))
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
                _ => {
                    return self
                        .parse_expression_statement()
                        .map(|e| Statement::Expression(Rc::new(e)))
                }
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
            value: Rc::new(expression),
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
            return_value: Rc::new(expression),
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
        Some(left)
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
        ast::{Expression, Statement},
        lexer::Lexer,
        token::{Token, TokenContainer},
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
        let expressions = vec!["5", "10", "838383"];
        for (i, expected) in expecteds.iter().enumerate() {
            let statement = program.statements().get(i);
            let statement =
                statement.expect(&format!("Expected a statement, got a {:?}", statement));
            test_let_statement(statement, &expected);
            let Statement::Let(let_statement) = statement else {
                panic!("Expected a let statement");
            };
            assert_eq!(let_statement.value.to_string(), expressions[i]);
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

        let expressions = vec!["5", "10", "112233"];
        for (i, statement) in program.statements().iter().enumerate() {
            let Statement::Return(ret) = statement else {
                panic!("Statement is not return statement, got '{:?}'", statement)
            };
            assert_eq!(ret.return_value.to_string(), expressions[i]);
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
            let Expression::Prefix(prefix) = &**expression else {
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

            let Expression::Infix(infix) = &**expression else {
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
            TestCase {
                input: "add(1 + a) + b / c",
                expected: "(add((1 + a)) + (b / c))",
            },
            TestCase {
                input: "a * [1, 2, 3, 4][b * c] * d",
                expected: "((a * ([1, 2, 3, 4][(b * c)])) * d)",
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
        let Expression::If(if_expression) = &**expression else {
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
        let Statement::Block(consequence) = &*if_expression.consequence else {
            panic!("Consequence is not a block statement")
        };
        assert_eq!(
            1,
            consequence.statements.len(),
            "Consequence is not 1 statement. got: {:?}",
            consequence.statements.len()
        );
        let statement = consequence.statements.get(0).unwrap();
        let Statement::Expression(expression) = statement else {
            panic!("Expected an expression statement, found: {:?}", statement)
        };
        let Expression::Identifier(consequence) = &**expression else {
            panic!("Expeced an identifier, found: {:?}", expression)
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
        let Expression::If(if_expression) = &**expression else {
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
        let Statement::Block(consequence) = &*if_expression.consequence else {
            panic!("Consequence is not a block statement")
        };
        assert_eq!(
            1,
            consequence.statements.len(),
            "Consequence is not 1 statement. got: {:?}",
            consequence.statements.len()
        );
        let statement = consequence.statements.get(0).unwrap();
        let Statement::Expression(expression) = statement else {
            panic!("Expected an expression statement, found: {:?}", statement)
        };
        let Expression::Identifier(consequence) = &**expression else {
            panic!("Expeced an identifier, found: {:?}", expression)
        };
        assert_eq!("x", consequence.value);
        assert!(if_expression.alternative.is_some());
        let Statement::Block(alternative) = &**if_expression.alternative.as_ref().unwrap() else {
            panic!("Consequence is not a block statement")
        };
        assert_eq!(
            1,
            alternative.statements.len(),
            "Consequence is not 1 statement. got: {:?}",
            alternative.statements.len()
        );
        let statement = alternative.statements.get(0).unwrap();
        let Statement::Expression(expression) = statement else {
            panic!("Expected an expression statement, found: {:?}", statement)
        };
        let Expression::Identifier(alternative) = &**expression else {
            panic!("Expeced an identifier, found: {:?}", expression)
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
        let Expression::FunctionLiteral(function) = &**expression else {
            panic!("Expected a function literal, found: {:?}", expression)
        };
        assert_eq!(function.parameters[0].value, "x");
        assert_eq!(function.parameters[1].value, "y");
        let Statement::Block(body) = &*function.body else {
            panic!("Expected a blocks statement, found: {:?}", function.body)
        };
        assert_eq!(body.statements.len(), 1);
        let Statement::Expression(body) = &body.statements[0] else {
            panic!(
                "Function body statement is not expression statement, found: {:?}",
                &body.statements[0]
            )
        };
        let Expression::Infix(infix) = &**body else {
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
            let Expression::FunctionLiteral(function) = &**expression else {
                panic!("Expected a function literal, found: {:?}", expression)
            };
            assert_eq!(function.parameters.len(), test.expected.len());
            for (i, parameter) in function.parameters.iter().enumerate() {
                assert_eq!(parameter.value, test.expected[i]);
            }
        }
    }

    #[test]
    fn test_call_expression() {
        let input = "add(1,2*3,4+5)";
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
        let Expression::Call(call) = &**expression else {
            panic!("Expected a function literal, found: {:?}", expression)
        };
        assert_eq!(call.function_ident.token(), &Token::Ident("add".into()));
        assert_eq!(call.arguments.len(), 3);
        assert_eq!(call.arguments[0].to_string(), "1");
        assert_eq!(call.arguments[1].to_string(), "(2 * 3)");
        assert_eq!(call.arguments[2].to_string(), "(4 + 5)");
    }

    #[test]
    fn test_call_expression_parameter_parsing() {
        struct TestCase<'a> {
            input: &'a str,
            expected: Vec<&'a str>,
        }
        let tests = vec![
            TestCase {
                input: "add()",
                expected: vec![],
            },
            TestCase {
                input: "add(1, b)",
                expected: vec!["1", "b"],
            },
            TestCase {
                input: "add(1, 4, 2 * 3)",
                expected: vec!["1", "4", "(2 * 3)"],
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
            let Expression::Call(function) = &**expression else {
                panic!("Expected a function literal, found: {:?}", expression)
            };
            assert_eq!(function.arguments.len(), test.expected.len());
            for (i, parameter) in function.arguments.iter().enumerate() {
                assert_eq!(parameter.to_string(), test.expected[i]);
            }
        }
    }

    #[test]
    fn test_string_literal_expression() {
        let input = "\"test string\"";
        let program = Parser::new(Lexer::new(input.into()))
            .parse_program()
            .unwrap();
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
        let Expression::StringLiteral(literal) = &**expression else {
            panic!("Expected a string literal, found: {:?}", expression)
        };
        assert_eq!(literal.value, "test string")
    }

    #[test]
    fn test_array_literal_expression() {
        let input = "[1, 2 * 3, 4 + 5]";
        let program = Parser::new(Lexer::new(input.into()))
            .parse_program()
            .unwrap();
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
        let Expression::ArrayLiteral(literal) = &**expression else {
            panic!("Expected a string literal, found: {:?}", expression)
        };
        assert_eq!(literal.elements.len(), 3);
        let Expression::IntegerLiteral(first) = &*literal.elements[0] else {
            panic!(
                "Expected an integer literal, found: {:?}",
                literal.elements[0]
            )
        };
        assert_eq!(first.value, 1);
        test_infix_expression(
            &literal.elements[1],
            &Token::Int(2),
            &Token::Asterisk,
            &Token::Int(3),
        );
        test_infix_expression(
            &literal.elements[2],
            &Token::Int(4),
            &Token::Plus,
            &Token::Int(5),
        );
    }

    #[test]
    fn test_parsing_index_expressions() {
        let input = "arr[1 + 1]";
        let program = Parser::new(Lexer::new(input.into()))
            .parse_program()
            .unwrap();
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
        let Expression::Index(index) = &**expression else {
            panic!("Expected an index expression, found: {:?}", expression)
        };
        let Expression::Identifier(identifier) = &*index.left else {
            panic!("Expected an identifier, found: {:?}", index)
        };
        assert_eq!(identifier.value, "arr");
        test_infix_expression(&index.index, &Token::Int(1), &Token::Plus, &Token::Int(1));
    }

    fn test_infix_expression(
        expression: &Expression,
        left: &Token,
        operator: &Token,
        right: &Token,
    ) {
        let Expression::Infix(infix) = expression else {
            panic!("Expected an infix expression, found: {:?}", expression)
        };
        assert_eq!(infix.left.token(), left);
        assert_eq!(infix.token(), operator);
        assert_eq!(infix.right.token(), right);
    }

    #[test]
    fn test_parsing_hash_literals_string_keys() {
        let input = r#"{"one": 1, "two": 2, "three": 3}"#;
        let program = Parser::new(Lexer::new(input.into()))
            .parse_program()
            .unwrap();
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
        let Expression::HashLiteral(hash_literal) = &**expression else {
            panic!("Expected a hash literal, found: {:?}", expression)
        };
        assert_eq!(hash_literal.pairs.len(), 3);
        let expected = vec![("one", 1), ("two", 2), ("three", 3)];
        for (i, pair) in hash_literal.pairs.iter().enumerate() {
            let Expression::StringLiteral(key) = &*pair.0 else {
                panic!(
                    "Hash map doesn't contain the expected key, found: {:?}",
                    pair.0
                )
            };
            let Expression::IntegerLiteral(value) = &*pair.1 else {
                panic!("Expected integer literal, found: {:?}", pair.1)
            };
            assert_eq!(key.value, expected.get(i).unwrap().0);
            assert_eq!(value.value, expected.get(i).unwrap().1);
        }
    }

    #[test]
    fn test_parsing_hash_literals_int_keys() {
        let input = r#"{1: "one", 2: "two", 3: "three"}"#;
        let program = Parser::new(Lexer::new(input.into()))
            .parse_program()
            .unwrap();
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
        let Expression::HashLiteral(hash_literal) = &**expression else {
            panic!("Expected a hash literal, found: {:?}", expression)
        };
        assert_eq!(hash_literal.pairs.len(), 3);
        let expected = vec![(1, "one"), (2, "two"), (3, "three")];
        for (i, pair) in hash_literal.pairs.iter().enumerate() {
            let Expression::IntegerLiteral(key) = &*pair.0 else {
                panic!(
                    "Hash map doesn't contain the expected key, found: {:?}",
                    pair.0
                )
            };
            let Expression::StringLiteral(value) = &*pair.1 else {
                panic!("Expected an integer literal, found: {:?}", pair.1)
            };
            assert_eq!(key.value, expected.get(i).unwrap().0);
            assert_eq!(value.value, expected.get(i).unwrap().1);
        }
    }

    #[test]
    fn test_parsing_hash_literals_boolean_keys() {
        let input = r#"{benar: "one", salah: "two"}"#;
        let program = Parser::new(Lexer::new(input.into()))
            .parse_program()
            .unwrap();
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
        let Expression::HashLiteral(hash_literal) = &**expression else {
            panic!("Expected a hash literal, found: {:?}", expression)
        };
        assert_eq!(hash_literal.pairs.len(), 2);
        let expected = vec![(true, "one"), (false, "two")];
        for (i, pair) in hash_literal.pairs.iter().enumerate() {
            let Expression::BooleanLiteral(key) = &*pair.0 else {
                panic!(
                    "Hash map doesn't contain the expected key, found: {:?}",
                    pair.0
                )
            };
            let Expression::StringLiteral(value) = &*pair.1 else {
                panic!("Expected an integer literal, found: {:?}", pair.1)
            };
            assert_eq!(key.value, expected.get(i).unwrap().0);
            assert_eq!(value.value, expected.get(i).unwrap().1);
        }
    }

    #[test]
    fn test_parsing_hash_literals_with_expressions() {
        let input = r#"{"one": 0 + 1, "two": 1 * 2, "three": 9 / 3}"#;
        let program = Parser::new(Lexer::new(input.into()))
            .parse_program()
            .unwrap();
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
        let Expression::HashLiteral(hash_literal) = &**expression else {
            panic!("Expected a hash literal, found: {:?}", expression)
        };
        assert_eq!(hash_literal.pairs.len(), 3);
        let expected = vec![
            (
                "one",
                Box::new(|e| test_infix_expression(e, &Token::Int(0), &Token::Plus, &Token::Int(1)))
                    as Box<dyn Fn(_)>,
            ),
            (
                "two",
                Box::new(|e| {
                    test_infix_expression(e, &Token::Int(1), &Token::Asterisk, &Token::Int(2))
                }),
            ),
            (
                "three",
                Box::new(|e| {
                    test_infix_expression(e, &Token::Int(9), &Token::Slash, &Token::Int(3))
                }),
            ),
        ];
        for (i, pair) in hash_literal.pairs.iter().enumerate() {
            let Expression::StringLiteral(key) = &*pair.0 else {
                panic!(
                    "Hash map doesn't contain the expected key, found: {:?}",
                    pair.0
                )
            };
            assert_eq!(key.value, expected.get(i).unwrap().0);
            expected.get(i).unwrap().1(&pair.1);
        }
    }

    #[test]
    fn test_parsing_empty_hash_literal() {
        let input = r#"{}"#;
        let program = Parser::new(Lexer::new(input.into()))
            .parse_program()
            .unwrap();
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
        let Expression::HashLiteral(hash_literal) = &**expression else {
            panic!("Expected a hash literal, found: {:?}", expression)
        };
        assert_eq!(hash_literal.pairs.len(), 0)
    }
}
