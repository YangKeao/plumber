use pest::{Parser};
use pest::iterators::Pair;

#[derive(Parser)]
#[grammar = "../grammar/plumber.pest"]
pub struct Plumber;

impl Plumber {
    pub fn compile(file: &str, target: &str) -> String {
        let mut ret = String::new();
        {
            ret.push_str("target triple = \"");
            ret.push_str(target);
            ret.push_str("\"")
        }

        let ast = Plumber::parse(Rule::program, &file).unwrap_or_else(|e| panic!("{}", e));
        let program = parse_ast(ast);
        ret
    }
}

#[derive(Debug)]
pub struct Variable(String);

#[derive(Debug)]
pub struct FunDefinition {
    ext: bool,
    name: String,
    args: Vec<Variable>,
    bindings: Vec<Binding>,
    expr: Expression
}

impl FunDefinition {
    pub fn parse(fun_definition: Pair<'_, Rule>) -> Self {
        if fun_definition.as_rule() != Rule::fun_definition {
            unreachable!()
        }
        let mut vec: Vec<Pair<'_, Rule>> = fun_definition.into_inner().collect();
        let ext = {
            if vec[0].as_rule() == Rule::ext {
                vec.remove(0);
                true
            } else {
                false
            }
        };


        let fun_name = {
            String::from(vec.remove(0).as_str())
        };

        let args = {
            let mut args = Vec::new();
            let mut max_arg_index = 0;
            for (index, arg) in vec.iter().enumerate() {
                if arg.as_rule() == Rule::variable_name {
                    args.push(Variable(String::from(arg.as_str())));
                } else {
                    max_arg_index = index;
                    break;
                }
            }
            vec.drain(0..max_arg_index);
            args
        };

        let bindings = {
            let mut bindings = Vec::new();
            if vec[0].as_rule() == Rule::local_binding {
                let mut local_binding: Vec<Pair<'_, Rule>> = vec.remove(0).into_inner().into_iter().collect();
                for local_binding in local_binding {
                    let mut bind: Vec<Pair<'_, Rule>> = local_binding.into_inner().into_iter().collect();
                    let name = bind.remove(0);
                    bindings.push(Binding {
                        var: Variable(String::from(name.as_str())),
                        value: Expression::parse(bind.remove(0))
                    })
                }
            }
            bindings
        };

        let expr = Expression::parse(vec.remove(0));
        FunDefinition {
            ext,
            name: fun_name,
            args,
            bindings,
            expr
        }
    }
}

#[derive(Debug)]
pub struct Binding {
    var: Variable,
    value: Expression
}

#[derive(Debug)]
pub enum Expression {
    BinaryExpression(BinaryExpression),
    MonadicExpression(MonadicExpression),
}

impl Expression {
    fn parse(expr: Pair<'_, Rule>) -> Self {
        if expr.as_rule() == Rule::expression {
            let mut expr: Vec<Pair<'_, Rule>> = expr.into_inner().collect();
            let mut expr = expr.remove(0);
            match expr.as_rule() {
                Rule::binary_expression => {
                    Expression::BinaryExpression(BinaryExpression::parse(expr))
                }
                Rule::monadic_expression => {
                    Expression::MonadicExpression(MonadicExpression::parse(expr))
                }
                _ => {
                    unreachable!()
                }
            }
        } else {
            unreachable!()
        }
    }
}

#[derive(Debug)]
pub struct BinaryExpression {
    left: Box<MonadicExpression>,
    right: Box<Expression>,
    op: BinaryOperation
}

impl BinaryExpression {
    pub fn parse(mut binary_expression: Pair<'_, Rule>) -> Self {
        if binary_expression.as_rule() != Rule::binary_expression {
            unreachable!()
        }

        let mut vec: Vec<Pair<'_, Rule>> = binary_expression.into_inner().collect();
        let first = vec.remove(0);
        let first_rule = first.as_rule();
        if first_rule == Rule::binary_expression {
            BinaryExpression::parse(first)
        } else {
            let left = first;
            let op = vec.remove(0);
            let right = vec.remove(0);
            BinaryExpression {
                left: Box::new(MonadicExpression::parse(left)),
                op: BinaryOperation::parse(op),
                right: Box::new(Expression::parse(right)),
            }
        }
    }
}

#[derive(Debug)]
pub enum BinaryOperation {
    Plus,
    Mul,
    Minus,
    Div
}

impl BinaryOperation {
    pub fn parse(op: Pair<'_, Rule>) -> Self {
        match op.as_rule() {
            Rule::plus => BinaryOperation::Plus,
            Rule::mul => BinaryOperation::Mul,
            Rule::minus => BinaryOperation::Minus,
            Rule::div => BinaryOperation::Div,
            _ => unreachable!()
        }
    }
}

#[derive(Debug)]
pub enum MonadicExpression {
    FunctionCall(FunctionCall),
    Variable(Variable),
    Const(i64)
}

impl MonadicExpression {
    pub fn parse(monadic_expression: Pair<'_, Rule>) -> Self {
        if monadic_expression.as_rule() != Rule::monadic_expression {
            unreachable!()
        }
        let mut pair = monadic_expression.into_inner().collect::<Vec<Pair<'_, Rule>>>().remove(0);
        let rule = pair.as_rule();
        match rule {
            Rule::function_call => {
                MonadicExpression::FunctionCall(FunctionCall::parse(pair))
            }
            Rule::variable_name => {
                MonadicExpression::Variable(Variable(String::from(pair.as_str())))
            }
            Rule::const_value => {
                MonadicExpression::Const(pair.as_str().parse::<i64>().unwrap())
            }
            _ => unreachable!()
        }
    }
}

#[derive(Debug)]
pub struct FunctionCall {
    function_name: Variable,
    args: Vec<Expression>
}

impl FunctionCall {
    pub fn parse(function_call: Pair<'_, Rule>) -> Self {
        if function_call.as_rule() != Rule::function_call {
            unreachable!()
        }
        let mut vec: Vec<Pair<'_, Rule>> = function_call.into_inner().collect();
        let function_name = Variable(String::from(vec.remove(0).as_str()));
        let args = {
            let mut args = Vec::new();
            for arg in vec {
                let rule = arg.as_rule();
                match rule {
                    Rule::monadic_expression => {
                        args.push(Expression::MonadicExpression(MonadicExpression::parse(arg)))
                    }
                    Rule::binary_expression => {
                        args.push(Expression::BinaryExpression(BinaryExpression::parse(arg)))
                    }
                    _ => {
                        unreachable!()
                    }
                }
            }
            args
        };
        FunctionCall {
            function_name,
            args,
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    FunDefinition(FunDefinition)
}

#[derive(Debug)]
pub struct Program (Vec<Statement>);

pub fn parse_ast(ast: pest::iterators::Pairs<'_, Rule>) -> Program {
    let mut program = Vec::new();

    for pair in ast.into_iter() {
        match pair.as_rule() {
            Rule::fun_definition => {
                program.push(Statement::FunDefinition(FunDefinition::parse(pair)));
            }
            Rule::EOI => {}
            _ => {
                panic!("Unexpected Rule")
            }
        }
    }
    Program(program)
}