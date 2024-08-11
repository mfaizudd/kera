use std::rc::Rc;

use crate::{
    ast::{Block, Expression, Identifier, If, Node, Program, Statement},
    token::{Token, TokenContainer},
    value::{self, Environment, Value},
};

pub fn eval(node: Node, env: &mut Environment) -> Value {
    match node {
        Node::Program(program) => eval_program(program, env),
        Node::Statement(statement) => match &*statement {
            Statement::Expression(expression) => eval(Node::Expression(expression.clone()), env),
            Statement::Block(block) => eval_block_statement(block, env),
            Statement::Return(rv) => {
                let val = eval(Node::Expression(rv.return_value.clone()), env);
                if let Value::Error(_) = val {
                    return val
                }
                Value::Return(Rc::new(val))
            }
            Statement::Let(statement) => {
                let val = eval(Node::Expression(statement.value.clone()), env);
                if let Value::Error(_) = val {
                    return val
                }
                env.set(statement.name.value.clone(), val.clone());
                val
            }
        },
        Node::Expression(expression) => match &*expression {
            Expression::IntegerLiteral(literal) => Value::Integer(literal.value),
            Expression::BooleanLiteral(literal) => literal.value.into(),
            Expression::Prefix(prefix) => {
                let right = eval(Node::Expression(prefix.right.clone()), env);
                if let Value::Error(_) = right {
                    return right
                }
                eval_prefix_expression(prefix.token(), right)
            }
            Expression::Infix(infix) => {
                let left = eval(Node::Expression(infix.left.clone()), env);
                if let Value::Error(_) = left {
                    return left
                }
                let right = eval(Node::Expression(infix.right.clone()), env);
                if let Value::Error(_) = right {
                    return right
                }
                eval_infix_expression(infix.token(), left, right)
            }
            Expression::If(ifelse) => eval_if_expression(ifelse, env),
            Expression::Identifier(identifier) => eval_identifier(identifier, env),
            _ => panic!("Unsupported yet"),
        },
    }
}

fn eval_program(program: Program, env: &mut Environment) -> Value {
    let mut result = Value::None;
    for statement in program.statements {
        result = eval(Node::Statement(&statement), env);
        match result {
            Value::Return(val) => return (*val).clone(),
            Value::Error(_) => return result,
            _ => (),
        }
    }
    result
}

fn eval_block_statement(block: &Block, env: &mut Environment) -> Value {
    let mut result = Value::None;
    for statement in &block.statements {
        result = eval(Node::Statement(&statement), env);
        match result {
            Value::Return(_) => return result,
            Value::Error(_) => return result,
            _ => (),
        }
    }
    result
}

fn eval_prefix_expression(operator: &Token, right: Value) -> Value {
    match operator {
        Token::Bang => eval_bang_operator_expression(right),
        Token::Minus => eval_minus_prefix_operator_expression(right),
        _ => Value::Error(format!(
            "Operator tidak dikenal: {}{}",
            operator.literal(),
            right.value_type()
        )),
    }
}

fn eval_infix_expression(operator: &Token, left: Value, right: Value) -> Value {
    match (left, right) {
        (Value::Integer(left), Value::Integer(right)) => {
            eval_integer_infix_expression(operator, left, right)
        }
        (Value::Boolean(left), Value::Boolean(right)) => {
            eval_boolean_infix_expression(operator, left, right)
        }
        (left, right) => {
            let left_type = left.value_type();
            let right_type = right.value_type();
            if left_type != right_type {
                return Value::Error(format!(
                    "Tipe tidak cocok: {} {} {}",
                    left_type,
                    operator.literal(),
                    right_type
                ));
            }
            Value::Error(format!(
                "Operator tidak dikenal: {} {} {}",
                left_type,
                operator.literal(),
                right_type
            ))
        }
    }
}

fn eval_if_expression(expression: &If, env: &mut Environment) -> Value {
    let condition = eval(Node::Expression(expression.condition.clone()), env);
    if let Value::Error(_) = condition {
        return condition
    }
    if is_truthy(&condition) {
        eval(Node::Statement(&*expression.consequence), env)
    } else if let Some(alternative) = expression.alternative.clone() {
        eval(Node::Statement(&*alternative), env)
    } else {
        value::NONE
    }
}

fn eval_identifier(identifier: &Identifier, env: &mut Environment) -> Value {
    match env.get(identifier.value.clone()) {
        Some(val) => val.clone(),
        None => Value::Error(format!("Pengenal tidak ditemukan: {}", identifier.value)),
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
        return Value::Error(format!("Operator tidak dikenal: -{}", right.value_type()));
    };

    Value::Integer(-value)
}

fn eval_integer_infix_expression(operator: &Token, left: i64, right: i64) -> Value {
    match operator {
        Token::Plus => Value::Integer(left + right),
        Token::Minus => Value::Integer(left - right),
        Token::Asterisk => Value::Integer(left * right),
        Token::Slash => Value::Integer(left / right),
        Token::GreaterThan => (left > right).into(),
        Token::LessThan => (left < right).into(),
        Token::Equal => (left == right).into(),
        Token::NotEqual => (left != right).into(),
        _ => Value::Error(format!(
            "Operator tidak dikenal: Bilangan bulat {} Bilangan bulat",
            operator.literal(),
        )),
    }
}

fn eval_boolean_infix_expression(operator: &Token, left: bool, right: bool) -> Value {
    match operator {
        Token::Equal => (left == right).into(),
        Token::NotEqual => (left != right).into(),
        _ => Value::Error(format!(
            "Operator tidak dikenal: Boolean {} Boolean",
            operator.literal(),
        )),
    }
}

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::None => false,
        Value::Boolean(val) => *val,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::Node,
        lexer::Lexer,
        parser::Parser,
        value::{self, Environment, Value},
    };

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
        let mut env = Environment::new();
        eval(Node::Program(program), &mut env)
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
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("1 + 2 + 3", 6),
            ("10 + 5 - 2", 13),
            ("2 * 2", 4),
            ("(7 + 5) * 2 / 3", 8),
            ("5 + 6 * 4 / 6", 9),
        ];
        for case in tests {
            let evaluated = test_eval(case.0.into());
            test_integer_value(evaluated, case.1);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![
            ("benar", true),
            ("salah", false),
            ("5>4", true),
            ("5<4", false),
            ("5==4", false),
            ("5!=4", true),
            ("5==5", true),
            ("5!=5", false),
            ("benar==benar", true),
            ("salah==salah", true),
            ("benar==salah", false),
            ("benar!=benar", false),
            ("benar!=salah", true),
            ("(2>1)==benar", true),
            ("(1>2)==benar", false),
            ("(5!=2)!=salah", true),
            ("(5!=2)!=benar", false),
        ];
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

    #[test]
    fn test_if_else_expression() {
        let tests = vec![
            ("jika benar { 10 }", Value::Integer(10)),
            ("jika salah { 10 }", value::NONE),
            ("jika 1 { 10 }", Value::Integer(10)),
            ("jika 1 < 2 { 10 }", Value::Integer(10)),
            ("jika 1 > 2 { 10 }", value::NONE),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.into());
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("kembalikan 10;", 10),
            ("kembalikan 9; 10;", 9),
            ("kembalikan 3 * 5; 9;", 15),
            ("1; kembalikan 4 * 5; 1;", 20),
            (
                r#"
            jika 1 > 0 {
                jika 1 > 0 {
                    kembalikan 2;
                }
                kembalikan 1;
            }
            "#,
                2,
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.into());
            test_integer_value(evaluated, expected);
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("0 + salah;", "Tipe tidak cocok: Bilangan bulat + Boolean"),
            (
                "0 + salah; 0;",
                "Tipe tidak cocok: Bilangan bulat + Boolean",
            ),
            ("-salah", "Operator tidak dikenal: -Boolean"),
            ("salah + salah", "Operator tidak dikenal: Boolean + Boolean"),
            (
                "0; salah + salah; 0;",
                "Operator tidak dikenal: Boolean + Boolean",
            ),
            (
                "jika 1 > 0 { salah + salah }",
                "Operator tidak dikenal: Boolean + Boolean",
            ),
            (
                r#"
            jika 1 > 0 {
                jika 1 > 0 {
                    kembalikan salah + salah;
                }
                kembalikan 0;
            }
            "#,
                "Operator tidak dikenal: Boolean + Boolean",
            ),
            ("foobar", "Pengenal tidak ditemukan: foobar")
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.into());
            let Value::Error(msg) = evaluated else {
                println!("Input: {}", input);
                panic!("No error object returned. Got: {:?}", evaluated)
            };
            assert_eq!(msg, expected)
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = vec![
            ("misal a = 1; a;", 1),
            ("misal a = 2 * 2; a;", 4),
            ("misal a = 2; misal b = a; b;", 2),
            ("misal a = 2; misal b = a; misal c = a + b + 1; c;", 5),
        ];

        for (input, expected) in tests {
            test_integer_value(test_eval(input.into()), expected)
        }
    }
}
