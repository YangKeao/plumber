use pest::{Parser};
use pest::iterators::Pair;
use crate::codegen::CodeGen;
use pest::error::InputLocation::Span;

#[derive(Parser)]
#[grammar = "../grammar/plumber.pest"]
pub struct Plumber;

impl Plumber {
    pub fn compile(file: &str, target: &str, module_name: &[u8]) {
        let ast = Plumber::parse(Rule::program, &file).unwrap_or_else(|e| panic!("{}", e));
        let program = Program::parse(ast);
        program.codegen(module_name);
    }
}

#[derive(Debug)]
pub struct Variable(String);

#[derive(Debug)]
pub struct FunDefinition {
    attributes: Vec<Attribute>,
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
        let mut attributes = Vec::new();
        while vec[0].as_rule() == Rule::attribute {
            let attribute = vec.remove(0);
            attributes.push(Attribute::parse(attribute));
        }
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
            attributes,
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
        match first.as_rule() {
            Rule::binary_expression => BinaryExpression::parse(first),
            Rule::monadic_expression => {
                let left = first;
                let op = vec.remove(0);
                let right = vec.remove(0);
                BinaryExpression {
                    left: Box::new(MonadicExpression::parse(left)),
                    op: BinaryOperation::parse(op),
                    right: Box::new(Expression::parse(right)),
                }
            }
            _ => {
                unreachable!()
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
        match pair.as_rule() {
            Rule::function_call => {
                MonadicExpression::FunctionCall(FunctionCall::parse(pair))
            }
            Rule::variable_name => {
                MonadicExpression::Variable(Variable(String::from(pair.as_str())))
            }
            Rule::const_value => {
                MonadicExpression::Const(pair.as_str().parse::<i64>().unwrap())
            }
            Rule::monadic_expression => {
                MonadicExpression::parse(pair)
            }
            _ => {
                unreachable!()
            }
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

impl Program {
    pub fn parse(program: pest::iterators::Pairs<'_, Rule>) -> Self {
        let mut ret: Vec<Statement> = Vec::new();

        for pair in program.into_iter() {
            match pair.as_rule() {
                Rule::fun_definition => {
                    ret.push(Statement::FunDefinition(FunDefinition::parse(pair)));
                }
                Rule::EOI => {}
                _ => {
                    panic!("Unexpected Rule")
                }
            }
        }
        Program(ret)
    }
}

#[derive(Debug)]
enum Attribute {
    Split(Split)
}

impl Attribute {
    fn parse(attribute: Pair<'_, Rule>) -> Self {
        if attribute.as_rule() != Rule::attribute {
            unreachable!()
        }
        let mut vec: Vec<Pair<'_, Rule>> = attribute.into_inner().into_iter().collect();
        let schedule = vec.remove(0);
        match schedule.as_rule() {
            Rule::split => {
                Attribute::Split(Split::parse(schedule))
            }
            _ => {
                unreachable!()
            }
        }
    }
}

#[derive(Debug)]
struct Split {
    var: Variable,
    bound: i64,
    var1: Variable,
    var2: Variable,
}

impl Split {
    fn parse(split: Pair<'_, Rule>) -> Self {
        if split.as_rule() != Rule::split {
            unreachable!()
        }
        let mut vec: Vec<Pair<'_, Rule>> = split.into_inner().into_iter().collect();
        Split {
            var: Variable(String::from(vec.remove(0).as_str())),
            bound: vec.remove(0).as_str().parse().unwrap(),
            var1: Variable(String::from(vec.remove(0).as_str())),
            var2: Variable(String::from(vec.remove(0).as_str())),
        }
    }
}