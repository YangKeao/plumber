use crate::codegen::Compile;
use pest::iterators::Pair;
use pest::Parser;

#[derive(Parser)]
#[grammar = "../grammar/plumber.pest"]
pub struct Plumber;

impl Plumber {
    pub fn compile(file: &str, _target: &str, module_name: &str) {
        let ast = Plumber::parse(Rule::program, &file).unwrap_or_else(|e| panic!("{}", e));
        let program = Program::parse(ast);
        program.compile(module_name);
    }
}

#[derive(Debug)]
pub struct Variable(String);

impl Variable {
    pub fn get_name(&self) -> &str {
        &self.0
    }
}

#[derive(Debug)]
pub struct VariableDefine {
    pub name: String,
    pub typ: Typ,
}

impl VariableDefine {
    pub fn parse(var_definition: Pair<'_, Rule>) -> Self {
        if var_definition.as_rule() != Rule::variable_define {
            unreachable!()
        }
        let mut vec: Vec<Pair<'_, Rule>> = var_definition.into_inner().into_iter().collect();
        VariableDefine {
            name: String::from(vec.remove(0).as_str()),
            typ: Typ::parse(vec.remove(0)),
        }
    }
}

#[derive(Debug)]
pub struct Struct(String);

impl Struct {
    pub fn get_name(&self) -> &str {
        &self.0
    }
}

#[derive(Debug)]
pub enum Typ {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Struct(Struct),
}

impl Typ {
    pub fn parse(typ: Pair<'_, Rule>) -> Self {
        if typ.as_rule() != Rule::type_name {
            unreachable!()
        }
        match typ.as_str() {
            "i8" => Typ::I8,
            "i16" => Typ::I16,
            "i32" => Typ::I32,
            "i64" => Typ::I64,
            "u8" => Typ::U8,
            "u16" => Typ::U16,
            "u32" => Typ::U32,
            "u64" => Typ::U64,
            _ => Typ::Struct(Struct(String::from(typ.as_str()))),
        }
    }
}

#[derive(Debug)]
pub struct FunDefinition {
    pub attributes: Vec<Attribute>,
    pub ext: bool,
    pub name: String,
    pub typ: Typ,
    pub args: Vec<VariableDefine>,
    pub bindings: Vec<Binding>,
    pub expr: Expression,
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

        let fun_name = { String::from(vec.remove(0).as_str()) };

        let args = {
            let mut args = Vec::new();
            while vec[0].as_rule() == Rule::variable_define {
                let arg = vec.remove(0);
                args.push(VariableDefine::parse(arg));
            }
            args
        };

        let typ = Typ::parse(vec.remove(0));

        let bindings = {
            let mut bindings = Vec::new();
            while vec[0].as_rule() == Rule::local_binding {
                let local_binding: Vec<Pair<'_, Rule>> =
                    vec.remove(0).into_inner().into_iter().collect();
                for local_binding in local_binding {
                    let mut bind: Vec<Pair<'_, Rule>> =
                        local_binding.into_inner().into_iter().collect();
                    bindings.push(Binding {
                        var: VariableDefine::parse(bind.remove(0)),
                        value: Expression::parse(bind.remove(0)),
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
            typ,
            args,
            bindings,
            expr,
        }
    }
}

#[derive(Debug)]
pub struct Binding {
    pub var: VariableDefine,
    pub value: Expression,
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
            let expr = expr.remove(0);
            match expr.as_rule() {
                Rule::binary_expression => {
                    Expression::BinaryExpression(BinaryExpression::parse(expr))
                }
                Rule::monadic_expression => {
                    Expression::MonadicExpression(MonadicExpression::parse(expr))
                }
                _ => unreachable!(),
            }
        } else {
            dbg!(expr);
            unreachable!()
        }
    }
}

#[derive(Debug)]
pub struct BinaryExpression {
    pub left: Box<MonadicExpression>,
    pub right: Box<Expression>,
    pub op: BinaryOperator,
}

impl BinaryExpression {
    pub fn parse(binary_expression: Pair<'_, Rule>) -> Self {
        if binary_expression.as_rule() != Rule::binary_expression {
            unreachable!()
        }

        let mut vec: Vec<Pair<'_, Rule>> = binary_expression.into_inner().collect();
        let first = vec.remove(0);
        let _first_rule = first.as_rule();
        match first.as_rule() {
            Rule::binary_expression => BinaryExpression::parse(first),
            Rule::monadic_expression => {
                let left = first;
                let op = vec.remove(0);
                let right = vec.remove(0);
                BinaryExpression {
                    left: Box::new(MonadicExpression::parse(left)),
                    op: BinaryOperator::parse(op),
                    right: Box::new(Expression::parse(right)),
                }
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum BinaryOperator {
    Plus,
    Mul,
    Minus,
    Div,
    Dot
}

impl BinaryOperator {
    pub fn parse(op: Pair<'_, Rule>) -> Self {
        match op.as_rule() {
            Rule::plus => BinaryOperator::Plus,
            Rule::mul => BinaryOperator::Mul,
            Rule::minus => BinaryOperator::Minus,
            Rule::div => BinaryOperator::Div,
            Rule::dot => BinaryOperator::Dot,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct TypeCast {
    pub inner: Box<Expression>,
    pub typ: Typ,
}

impl TypeCast {
    pub fn parse(type_cast: Pair<'_, Rule>) -> Self {
        let mut inner: Vec<Pair<'_, Rule>> = type_cast.into_inner().into_iter().collect();
        TypeCast {
            typ: Typ::parse(inner.remove(0)),
            inner: Box::new(Expression::parse(inner.remove(0))),
        }
    }
}

#[derive(Debug)]
pub enum MonadicExpression {
    FunctionCall(FunctionCall),
    Variable(Variable),
    TypeCast(TypeCast),
    Expression(Box<Expression>),
    Const(i64),
}

impl MonadicExpression {
    pub fn parse(monadic_expression: Pair<'_, Rule>) -> Self {
        if monadic_expression.as_rule() != Rule::monadic_expression {
            unreachable!()
        }
        let pair = monadic_expression
            .into_inner()
            .collect::<Vec<Pair<'_, Rule>>>()
            .remove(0);
        match pair.as_rule() {
            Rule::function_call => MonadicExpression::FunctionCall(FunctionCall::parse(pair)),
            Rule::variable_name => {
                MonadicExpression::Variable(Variable(String::from(pair.as_str())))
            }
            Rule::const_value => MonadicExpression::Const(pair.as_str().parse::<i64>().unwrap()),
            Rule::expression => MonadicExpression::Expression(Box::new(Expression::parse(pair))),
            Rule::type_cast => MonadicExpression::TypeCast(TypeCast::parse(pair)),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionCall {
    pub function_name: Variable,
    pub args: Vec<Expression>,
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
                    _ => unreachable!(),
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
    FunDefinition(FunDefinition),
    StructDefinition(StructDefinition),
}

#[derive(Debug)]
pub struct StructDefinition {
    pub name: Struct,
    pub fields: Vec<Typ>,
}

impl StructDefinition {
    pub fn parse(struct_def: Pair<'_, Rule>) -> Self {
        if struct_def.as_rule() != Rule::struct_definition {
            unreachable!()
        }
        let mut inner: Vec<Pair<'_, Rule>> = struct_def.into_inner().into_iter().collect();

        let name = Struct(String::from(inner.remove(0).as_str()));
        let fields = {
            let mut fields = Vec::new();
            while inner.len() > 0 {
                fields.push(Typ::parse(inner.remove(0)))
            }
            fields
        };
        Self { name, fields }
    }
}

#[derive(Debug)]
pub struct Program(Vec<Statement>);

impl Program {
    pub fn get_statements(&self) -> &Vec<Statement> {
        &self.0
    }
    pub fn parse(program: pest::iterators::Pairs<'_, Rule>) -> Self {
        let mut ret: Vec<Statement> = Vec::new();

        for pair in program.into_iter() {
            match pair.as_rule() {
                Rule::fun_definition => {
                    ret.push(Statement::FunDefinition(FunDefinition::parse(pair)));
                }
                Rule::struct_definition => {
                    ret.push(Statement::StructDefinition(StructDefinition::parse(pair)))
                }
                Rule::EOI => {}
                _ => unreachable!(),
            }
        }
        Program(ret)
    }
}

#[derive(Debug)]
pub enum Attribute {
    Split(Split),
}

impl Attribute {
    fn parse(attribute: Pair<'_, Rule>) -> Self {
        if attribute.as_rule() != Rule::attribute {
            unreachable!()
        }
        let mut vec: Vec<Pair<'_, Rule>> = attribute.into_inner().into_iter().collect();
        let schedule = vec.remove(0);
        match schedule.as_rule() {
            Rule::split => Attribute::Split(Split::parse(schedule)),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct Split {
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
