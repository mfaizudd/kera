use crate::{
    ast::{Expression, Node, Statement},
    token::Token,
    value::{self, Value},
};

pub fn eval(node: Node) -> Value {
    match node {
        Node::Program(program) => eval_statements(&program.statements),
        Node::Statement(Statement::Expression(expression)) => eval(Node::Expression(expression)),
        Node::Expression(Expression::IntegerLiteral(literal)) => Value::Integer(literal.value),
        Node::Expression(Expression::BooleanLiteral(literal)) => literal.value.into(),
        Node::Expression(Expression::Prefix(prefix)) => {
            let right = eval(Node::Expression(&*prefix.right));
            eval_prefix_expression(&prefix.token, right)
        }
        _ => panic!("Unsupported yet"),
    }
}

fn eval_statements(statements: &Vec<Statement>) -> Value {
    let mut result = Value::None;
    for statement in statements {
        result = eval(Node::Statement(&statement))
    }
    result
}

fn eval_prefix_expression(operator: &Token, right: Value) -> Value {
    match operator {
        Token::Bang => eval_bang_operator_expression(right),
        Token::Minus => eval_minus_prefix_operator_expression(right),
        _ => value::NONE,
    }
}

fn eval_bang_operator_expression(right: Value) -> Value {
    match right {
        Value::Boolean(true) => value::FALSE,
        Value::Boolean(false) => value::TRUE,
        Value::None => value::TRUE,
        _ => value::FALSE,
    }
}

fn eval_minus_prefix_operator_expression(right: Value) -> Value {
    let Value::Integer(value) = right else {
        return value::NONE;
    };

    Value::Integer(-value)
}

#[cfg(test)]
mod tests {
    use crate::{ast::Node, lexer::Lexer, parser::Parser, value::Value};

    use super::eval;

    fn test_eval(input: String) -> Value {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(errors) => {
                panic!("Parser has errors: {:?}", errors)
            }
        };
        eval(Node::Program(&program))
    }

    fn test_integer_value(value: Value, expected: i64) {
        let Value::Integer(integer) = value else {
            panic!("Value not integer, got: {:?}", value);
        };
        assert_eq!(integer, expected);
    }

    fn test_boolean_value(value: Value, expected: bool) {
        let Value::Boolean(boolean) = value else {
            panic!("Value not boolean, got: {:?}", value)
        };
        assert_eq!(boolean, expected);
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![("5", 5), ("10", 10), ("-5", -5), ("-10", -10)];
        for case in tests {
            let evaluated = test_eval(case.0.into());
            test_integer_value(evaluated, case.1);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![("benar", true), ("salah", false)];
        for case in tests {
            let evaluated = test_eval(case.0.into());
            test_boolean_value(evaluated, case.1);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!benar", false),
            ("!salah", true),
            ("!5", false),
            ("!!benar", true),
            ("!!5", true),
        ];
        for case in tests {
            let evaluated = test_eval(case.0.into());
            test_boolean_value(evaluated, case.1)
        }
    }
}
